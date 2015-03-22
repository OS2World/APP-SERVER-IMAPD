(**************************************************************************)
(*                                                                        *)
(*  IMAP add-on for the Weasel mail server                                *)
(*  Copyright (C) 2014   Peter Moylan                                     *)
(*                                                                        *)
(*  This program is free software: you can redistribute it and/or modify  *)
(*  it under the terms of the GNU General Public License as published by  *)
(*  the Free Software Foundation, either version 3 of the License, or     *)
(*  (at your option) any later version.                                   *)
(*                                                                        *)
(*  This program is distributed in the hope that it will be useful,       *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU General Public License for more details.                          *)
(*                                                                        *)
(*  You should have received a copy of the GNU General Public License     *)
(*  along with this program.  If not, see <http://www.gnu.org/licenses/>. *)
(*                                                                        *)
(*  To contact author:   http://www.pmoylan.org   peter@pmoylan.org       *)
(*                                                                        *)
(**************************************************************************)

IMPLEMENTATION MODULE UserInstances;

        (********************************************************)
        (*                                                      *)
        (*      User instance handler for the IMAP4 server      *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            8 March 2001                    *)
        (*  Last edited:        2 January 2014                  *)
        (*  Status:             OK (but incomplete?)            *)
        (*                                                      *)
        (********************************************************)


IMPORT Strings;

FROM Users IMPORT
    (* type *)  User,
    (* proc *)  FindUser, DiscardUser, UCreateBox, UDeleteBox,
                UFindBox, UDoListing, USubscribeToBox, UUnsubscribeToBox,
                URenameBox, UGetStatus, UAddMessage, UCheckpoint;

FROM Domains IMPORT
    (* type *)  Domain,
    (* proc *)  NameOfDomain;

FROM MailAccounts IMPORT
    (* proc *)  ConfirmPassword, IsIMAPUser;

FROM BoxLists IMPORT
    (* type *)  BoxPtr,
    (* proc *)  BoxExists, DirectoryOf, UIDValidityResponse;

FROM Boxes IMPORT
    (* type *)  MailboxUser,
    (* proc *)  OpenMailbox, CloseMailboxUser, SelectBoxReplies, FetchItems,
                StoreItemData, DoExpunge, CopySet, SearchResult, StatusResponse,
                LockMailbox, UnlockMailbox, RefreshItemList;

FROM MSet IMPORT
    (* type *)  MessageSet,
    (* proc *)  GetSet;

FROM Parser IMPORT
    (* type *)  MessageFlagSet;

FROM Replies IMPORT
    (* type *)  ReplyCxt;

FROM SBuffers IMPORT
    (* type *)  SBuffer;

FROM TransLog IMPORT
    (* type *)  TransactionLogID,
    (* proc *)  LogTransaction;

FROM Names IMPORT
    (* type *)  FilenameString, DomainName;

FROM Inet2Misc IMPORT
    (* proc *)  StringMatch;

FROM TaskControl IMPORT
    (* type *)  Lock,
    (* proc *)  CreateLock, DestroyLock, Obtain, Release;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

(************************************************************************)

TYPE
    (* Because one username can have multiple simultaneous logins, we   *)
    (* have to distinguish between a "User" and a "UserInstance".  The  *)
    (* UserInstance belongs to one particular session.  The User data   *)
    (* are potentially shared by several UserInstance sessions.         *)

    UserInstance = POINTER TO
                       RECORD
                           SB: SBuffer;
                           LogID: TransactionLogID;
                           RC: ReplyCxt;
                           user: User;
                           MBUselected: MailboxUser;
                           secondMBU: MailboxUser;
                       END (*RECORD*);

    (* A UserChain is a list of instances sharing the same User. *)

    UserChain = POINTER TO
                       RECORD
                           this: UserInstance;
                           next: UserChain;
                       END (*RECORD*);

    (* Linear list of all UserChain lists. *)

    ListOfUserChains = POINTER TO
                       RECORD
                           user: User;
                           head: UserChain;
                           next: ListOfUserChains;
                       END (*RECORD*);

(************************************************************************)

CONST
    Nul = CHR(0);

VAR
    (* Biglist is the list of users we currently need to know about. *)
    (* It will change as people log in and out.                      *)

    Biglist: ListOfUserChains;

    (* Critical section protection for Biglist. *)

    BiglistLock: Lock;

(************************************************************************)
(*                  CONNECTING USER INSTANCES TO USERS                  *)
(************************************************************************)

PROCEDURE MakeUserInstance (VAR (*IN*) username: ARRAY OF CHAR;
                                       domain: Domain): UserInstance;

    (* Creates a data structure for one instance of this username.      *)

    VAR result: UserInstance;  U: User;
        previous, current: ListOfUserChains;
        prev, p: UserChain;

    BEGIN
        U := FindUser (username, domain);
        Obtain (BiglistLock);
        previous := NIL;  current := Biglist;
        WHILE (current <> NIL) AND (current^.user <> U) DO
            previous := current;
            current := current^.next;
        END (*WHILE*);

        IF current = NIL THEN

            (* There are no existing instances for this user, so we     *)
            (* have to start a new list of instances.                   *)

            NEW (current);
            WITH current^ DO
                user := U;
                head := NIL;
                next := NIL;
            END (*WITH*);
            IF previous = NIL THEN
                Biglist := current;
            ELSE
                previous^.next := current;
            END (*IF*);
        END (*IF*);

        (* Now current is the head of the list of instances for this    *)
        (* user.  Find the tail of this list and add a new instance.    *)

        prev := NIL;
        p := current^.head;

        WHILE p <> NIL DO
            prev := p;
            p :=  p^.next;
        END (*WHILE*);

        NEW (p);
        p^.this := NIL;
        p^.next := NIL;
        IF prev = NIL THEN
            current^.head := p;
        ELSE
            prev^.next := p;
        END (*IF*);

        (* Fill in the details for the new instance. *)

        NEW (result);
        p^.this := result;
        WITH result^ DO
            user := U;
            MBUselected := NIL;
            secondMBU := NIL;
        END (*WITH*);

        Release (BiglistLock);

        RETURN result;

    END MakeUserInstance;

(************************************************************************)

PROCEDURE DisconnectInstance (UI: UserInstance);

    (* Removes UI from the list of instances belonging to its user.*)

    VAR U: User;  p: UserChain;
        previous, current: ListOfUserChains;
        uprev, ucurrent: UserChain;

    BEGIN
        U := UI^.user;

        (* First find the user on the Biglist list. *)

        Obtain (BiglistLock);
        previous := NIL;  current := Biglist;
        WHILE (current <> NIL) AND (current^.user <> U) DO
            previous := current;
            current := previous^.next;
        END (*WHILE*);

        IF current = NIL THEN
            p := NIL;
        ELSE
            p := current^.head;
        END (*IF*);

        (* Now p is the head of the chain of instances for one user. *)

        IF p <> NIL THEN
            uprev := NIL;  ucurrent := p;
            WHILE (ucurrent <> NIL) AND (ucurrent^.this <> UI) DO
                uprev := ucurrent;
                ucurrent := uprev^.next;
            END (*WHILE*);

            IF ucurrent <> NIL THEN
                IF uprev = NIL THEN
                    current^.head := ucurrent^.next;
                ELSE
                    uprev^.next := ucurrent^.next;
                END (*IF*);
                DISPOSE (ucurrent);
            END (*IF*);

            IF current^.head = NIL THEN

                (* We have just removed the last instance for this      *)
                (* user, so remove the user data structure.             *)

                IF previous = NIL THEN
                    Biglist := current^.next;
                ELSE
                    previous^.next := current^.next;
                END (*IF*);
                DISPOSE (current);
                DiscardUser (U);

            END (*IF*);

        END (*IF*);

        Release (BiglistLock);

    END DisconnectInstance;

(************************************************************************)
(*                     OPERATIONS ON USER INSTANCES                     *)
(************************************************************************)

PROCEDURE LoginUser (VAR (*OUT*) UI: UserInstance;  SB: SBuffer;
                     VAR (*IN*) username, password: ARRAY OF CHAR;
                     hostaddr: CARDINAL;
                     RC: ReplyCxt;
                     LogID: TransactionLogID): BOOLEAN;

    (* Checks that username and password are correct.  If so, creates   *)
    (* a new user instance record and returns TRUE.                     *)

    VAR D: Domain;

    BEGIN
        UI := NIL;
        IF ConfirmPassword (hostaddr, username, password, D)
                           AND IsIMAPUser (username, D) THEN
            UI := MakeUserInstance (username, D);
            UI^.SB := SB;
            UI^.RC := RC;
            UI^.LogID := LogID;
        END (*IF*);

        RETURN UI <> NIL;

    END LoginUser;

(************************************************************************)

PROCEDURE CreateUserInstance (VAR (*OUT*) UI: UserInstance;  SB: SBuffer;
                                        username: ARRAY OF CHAR;
                                        D: Domain;
                                        RC: ReplyCxt;
                                        LogID: TransactionLogID): BOOLEAN;

    (* Like LoginUser, except that we assume that no password check     *)
    (* is needed because the user has been authenticated by some other  *)
    (* mechanism.  We do however check that this user is allowed to     *)
    (* use IMAP.                                                        *)

    VAR success: BOOLEAN;
        message: ARRAY [0..255] OF CHAR;
        name: DomainName;

    BEGIN
        success := IsIMAPUser (username, D);
        IF success THEN
            UI := MakeUserInstance (username, D);
            UI^.SB := SB;
            UI^.RC := RC;
            UI^.LogID := LogID;
        ELSE
            Strings.Assign (username, message);
            Strings.Append ("@", message);
            NameOfDomain (D, name);
            Strings.Append (name, message);
            Strings.Append (" is not a valid IMAP user.", message);
            LogTransaction (LogID, message);
        END (*IF*);
        RETURN success;
    END CreateUserInstance;

(************************************************************************)

PROCEDURE CloseUserInstance (VAR (*INOUT*) UI: UserInstance);

    (* Discards the user instance record, after any tidying up that is  *)
    (* needed on a logout (whether explicit or forced).                 *)

    BEGIN
        IF UI <> NIL THEN
            CloseMailboxUser (UI^.MBUselected);
            CloseMailboxUser (UI^.secondMBU);
            DisconnectInstance (UI);
            DISPOSE (UI);
        END (*IF*);
    END CloseUserInstance;

(************************************************************************)

PROCEDURE DoListing (UI: UserInstance;  refname, mailbox: FilenameString;
                                    IncludeAll: BOOLEAN): BOOLEAN;

    (* Responds to the LIST or LSUB command.  The IncludeAll parameter  *)
    (* is TRUE for LIST, FALSE FOR LSUB.                                *)

    (* The mailbox parameter may include wildcards, the refname is not  *)
    (* wild.                                                            *)

    BEGIN
        RETURN UDoListing (UI^.RC, UI^.user, mailbox, IncludeAll);
    END DoListing;

(************************************************************************)
(*                       OPERATIONS ON MAILBOXES                        *)
(************************************************************************)

PROCEDURE StatusUpdate (UI: UserInstance);

    (* Sends the client the current status of the selected mailbox.  In *)
    (* the present version, this is invoked only by the NOOP command.   *)

    VAR MBU: MailboxUser;

    BEGIN
        MBU := UI^.MBUselected;
        RefreshItemList (MBU);
        LockMailbox (MBU);
        StatusResponse (MBU);
        UnlockMailbox (MBU);
    END StatusUpdate;

(************************************************************************)

PROCEDURE SelectBox (UI: UserInstance;  boxname: ARRAY OF CHAR;
                                              ReadOnly: BOOLEAN): BOOLEAN;

    (* Selects a mailbox.  Note that SELECT is a 'session' operation    *)
    (* rather than a 'user' one.  Note also that the currently selected *)
    (* mailbox is deselected whether or not the selection succeeds.     *)

    VAR directory: FilenameString;
        boxptr: BoxPtr;
        IsInbox, exists: BOOLEAN;

    BEGIN
        CloseMailboxUser (UI^.MBUselected);
        CloseMailboxUser (UI^.secondMBU);
        boxptr := UFindBox (UI^.user, boxname);
        exists := BoxExists (boxptr);
        IF exists THEN
            DirectoryOf (boxptr, directory);
            IsInbox := StringMatch (boxname, 'INBOX');
            UI^.MBUselected := OpenMailbox (UI^.RC, directory,
                                      IsInbox, ReadOnly);
            SelectBoxReplies (UI^.MBUselected);
            UIDValidityResponse (UI^.RC, boxptr);
        END (*IF*);
        RETURN exists;
    END SelectBox;

(************************************************************************)

PROCEDURE SecondarySelectBox (UI: UserInstance;  boxname: ARRAY OF CHAR): BOOLEAN;

    (* Opens a mailbox without marking it as selected.  This is to      *)
    (* support the COPY operation that refers to two mailboxes.  The    *)
    (* secondary box will be closed at the next available opportunity,  *)
    (* i.e. it cannot be counted on to be open for more than one        *)
    (* operation.                                                       *)

    VAR directory: FilenameString;
        boxptr: BoxPtr;
        IsInbox, exists: BOOLEAN;

    BEGIN
        CloseMailboxUser (UI^.secondMBU);
        boxptr := UFindBox (UI^.user, boxname);
        exists := BoxExists (boxptr);
        IF exists THEN
            DirectoryOf (boxptr, directory);
            IsInbox := StringMatch (boxname, 'INBOX');
            UI^.secondMBU := OpenMailbox (UI^.RC, directory,
                                     IsInbox, FALSE);
        END (*IF*);
        RETURN exists;
    END SecondarySelectBox;

(************************************************************************)

PROCEDURE DeselectBox (UI: UserInstance);

    (* Deselects the currently selected mailbox.  *)

    BEGIN
        CloseMailboxUser (UI^.MBUselected);
        CloseMailboxUser (UI^.secondMBU);
    END DeselectBox;

(************************************************************************)

PROCEDURE FindBox (UI: UserInstance;  boxname: ARRAY OF CHAR): BoxPtr;

    (* Finds a mailbox, returns NIL if it does not exist. *)

    BEGIN
        RETURN UFindBox (UI^.user, boxname);
    END FindBox;

(************************************************************************)

PROCEDURE CreateBox (UI: UserInstance;
                       VAR (*INOUT*) boxname: ARRAY OF CHAR;
                       VAR (*OUT*) alreadyexists: BOOLEAN): BOOLEAN;

    (* Creates a new mailbox. *)

    BEGIN
        RETURN UCreateBox (UI^.user, boxname, alreadyexists);
    END CreateBox;

(************************************************************************)

PROCEDURE DeleteBox (UI: UserInstance;
                       VAR (*INOUT*) boxname: ARRAY OF CHAR): BOOLEAN;

    (* Deletes an existing mailbox. *)

    BEGIN
        IF StringMatch (boxname, "INBOX") THEN
            RETURN FALSE;
        ELSE
            RETURN UDeleteBox (UI^.user, boxname);
        END (*IF*);
    END DeleteBox;

(************************************************************************)

PROCEDURE RenameBox (UI: UserInstance;
                    VAR (*INOUT*) oldname, newname: ARRAY OF CHAR;
                    VAR (*OUT*) targetexists: BOOLEAN): BOOLEAN;

    (* Renames an existing mailbox. *)

    BEGIN
        RETURN URenameBox (UI^.user, oldname, newname, targetexists);
    END RenameBox;

(************************************************************************)

PROCEDURE SubscribeToBox (UI: UserInstance;
                       VAR (*INOUT*) boxname: ARRAY OF CHAR): BOOLEAN;

    (* Subscribes to a mailbox. *)

    BEGIN
        RETURN USubscribeToBox (UI^.user, boxname);
    END SubscribeToBox;

(************************************************************************)

PROCEDURE UnsubscribeFromBox (UI: UserInstance;
                       VAR (*INOUT*) boxname: ARRAY OF CHAR): BOOLEAN;

    (* Removes a subscription. *)

    BEGIN
        RETURN UUnsubscribeToBox (UI^.user, boxname);
    END UnsubscribeFromBox;

(************************************************************************)

PROCEDURE DoCheckpoint (UI: UserInstance);

    (* CHECK command. *)

    BEGIN
        (* This is a checkpoint of both the user information (the list  *)
        (* of mailboxes) and the currently selected mailbox.            *)

        UCheckpoint (UI^.user);
        RefreshItemList (UI^.MBUselected);

    END DoCheckpoint;

(************************************************************************)

PROCEDURE DoFetch (UI: UserInstance;  VAR (*INOUT*) args: ARRAY OF CHAR;
                   UseUID: BOOLEAN;  LogID: TransactionLogID): BOOLEAN;

    (* Fetches one or more messages as specified in args.  If the       *)
    (* UseUID argument is TRUE, we are doing a UID FETCH.               *)
    (* The args string is destroyed as a side-effect.                   *)

    VAR MSet: MessageSet;

    BEGIN
        MSet := GetSet (args);
        RETURN FetchItems (UI^.RC, UI^.MBUselected, MSet, args, UseUID, LogID);
    END DoFetch;

(************************************************************************)

PROCEDURE DoStore (UI: UserInstance;  VAR (*INOUT*) args: ARRAY OF CHAR;
                   UseUID: BOOLEAN;  LogID: TransactionLogID): BOOLEAN;

    (* Stores attributes for one or more messages as specified in args. *)
    (* If the UseUID argument is TRUE, we are doing a UID STORE.        *)

    VAR MSet: MessageSet;

    BEGIN
        MSet := GetSet (args);
        RETURN StoreItemData (UI^.RC, UI^.MBUselected, MSet, args, UseUID, LogID);
    END DoStore;

(************************************************************************)

PROCEDURE Expunge (UI: UserInstance): BOOLEAN;

    (* Removes messages marked for deletion.  Returns FALSE if the      *)
    (* box was read-only or no box was selected.                        *)

    BEGIN
        RETURN DoExpunge (UI^.RC, UI^.MBUselected);
    END Expunge;

(************************************************************************)

PROCEDURE DoCopy (UI: UserInstance;  set: MessageSet;
                                     UseUID: BOOLEAN): BOOLEAN;

    (* Copies messages from the selected mailbox to the secondary box. *)

    VAR success: BOOLEAN;

    BEGIN
        success := UI <> NIL;
        IF success THEN
            success := CopySet (set, UI^.MBUselected, UI^.secondMBU, UseUID);
            CloseMailboxUser (UI^.secondMBU);
        END (*IF*);
        RETURN success;
    END DoCopy;

(************************************************************************)

PROCEDURE SearchBox (UI: UserInstance;  VAR (*INOUT*) keys: ARRAY OF CHAR;
                                                         UseUID: BOOLEAN);

    (* Searches mailbox with search criteria given in keys. *)

    BEGIN
        SearchResult (UI^.RC, UI^.MBUselected, keys, UseUID);
    END SearchBox;

(************************************************************************)

PROCEDURE GetStatus (UI: UserInstance;
                         VAR (*IN*) boxname, args: ARRAY OF CHAR): BOOLEAN;

    (* Responds to the STATUS command.  The 'args' argument should be a *)
    (* parenthesised list.                                              *)

    BEGIN
        RETURN UGetStatus (UI^.RC, UI^.user, boxname, args);
    END GetStatus;

(************************************************************************)

PROCEDURE AddMessage (UI: UserInstance;  BP: BoxPtr;  IsInbox: BOOLEAN;
                      flags: MessageFlagSet;  date, time: CARDINAL;
                       count: CARDINAL): BOOLEAN;

    (* Creates a new message in box BP^, whose content is the     *)
    (* next 'count' characters to be fetched from the client.     *)
    (* The file date/time is set iff date > 0.                    *)

    BEGIN
        RETURN UAddMessage (UI^.RC, UI^.user, BP, IsInbox, flags,
                                                       date, time, count);
    END AddMessage;

(************************************************************************)
(*                           INITIALISATION                             *)
(************************************************************************)

BEGIN
    Biglist := NIL;
    CreateLock (BiglistLock);
FINALLY
    DestroyLock (BiglistLock);
END UserInstances.

