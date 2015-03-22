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

IMPLEMENTATION MODULE Users;

        (********************************************************)
        (*                                                      *)
        (*           User data for the IMAP4 server             *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            8 March 2001                    *)
        (*  Last edited:        13 January 2014                 *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (********************************************************)


IMPORT Strings, FileSys;

FROM LogCtx IMPORT
    (* var  *)  WCtx;

FROM BoxLists IMPORT
    (* type *)  BoxPtr, ListOfBoxes,
    (* proc *)  CreateBoxList, CloseBoxList, UpdateBoxList, ListTo, FindBox,
                HeadSplit, RemoveBox, DoSubscribe, DoUnsubscribe,
                DoRename, UIDValidityCode, DirectoryOf;

FROM Boxes IMPORT
    (* type *)  MailboxUser,
    (* proc *)  OpenMailbox, CloseMailboxUser, DoStatusReply,
                DoAddMessage;

FROM Domains IMPORT
    (* type *)  Domain,
    (* proc *)  MailDirectoryFor;

FROM Parser IMPORT
    (* type *)  MessageFlagSet,
    (* proc *)  MatchNoDelete;

FROM Replies IMPORT
    (* type *)  ReplyCxt,
    (* proc *)  ReplyUntagged;

FROM SBuffers IMPORT
    (* type *)  SBuffer,
    (* proc *)  SendLine;

FROM FileOps IMPORT
    (* type *)  DirectoryEntry,
    (* proc *)  FirstDirEntry, NextDirEntry, DirSearchDone, MoveFile;

FROM TransLog IMPORT
    (* type *)  TransactionLogID,
    (* proc *)  CreateLogID, DiscardLogID;

FROM Inet2Misc IMPORT
    (* proc *)  StringMatch;

FROM Names IMPORT
    (* type *)  UserName, FilenameString;

FROM TaskControl IMPORT
    (* type *)  Lock,
    (* proc *)  CreateLock, DestroyLock, Obtain, Release;

FROM LowLevel IMPORT
    (* proc *)  EVAL;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

(************************************************************************)

CONST
    Nul = CHR(0);

TYPE
    (* For a user, the fields are:                                      *)
    (*   access         critical section protection lock                *)
    (*   name           the username                                    *)
    (*   directory      the name of the directory where this user's     *)
    (*                    mail is held (no trailing '\')                *)
    (*   boxlist        mailboxes in the user's directory               *)

    User =  POINTER TO RECORD
                           access: Lock;
                           name: UserName;
                           directory: FilenameString;
                           boxlist: ListOfBoxes;
                       END (*RECORD*);

    ListOfUsers = POINTER TO
                       RECORD
                           this: User;
                           next: ListOfUsers;
                       END (*RECORD*);

(************************************************************************)

VAR
    (* Masterlist is the list of users we currently need to know about. *)
    (* It will change as people log in and out.                         *)

    Masterlist: ListOfUsers;

    (* Critical section protection for Masterlist. *)

    MasterlistLock: Lock;

    (* Transaction log id for error messages.  Not used at present, so  *)
    (* we can probably get rid of it in the production version.         *)

    StarID: TransactionLogID;

(************************************************************************)
(*                          OPERATIONS ON USERS                         *)
(************************************************************************)

PROCEDURE MakeSubdirectory (dir: FilenameString;
                             subdir: ARRAY OF CHAR;
                             VAR (*OUT*) alreadyexists: BOOLEAN): BOOLEAN;
                                                                  FORWARD;

(************************************************************************)

PROCEDURE MakeUser (VAR (*IN*) username: ARRAY OF CHAR;  D: Domain): User;

    (* Introduces a new user to the system.      *)

    VAR U: User;  dummy: BOOLEAN;

    BEGIN
        NEW (U);
        WITH U^ DO
            CreateLock (access);
            Strings.Assign (username, name);
            MailDirectoryFor (D, directory);
            Strings.Append (username, directory);
            boxlist := CreateBoxList (directory);
        END (*WITH*);

        (* Make sure that a directory called INBOX exists.  Most of the *)
        (* time this call will fail and do nothing, but it is needed    *)
        (* for the case of a genuine new user.                          *)

        EVAL (MakeSubdirectory (U^.directory, "INBOX", dummy));
        RETURN U;
    END MakeUser;

(************************************************************************)

PROCEDURE FindUser (VAR (*IN*) username: ARRAY OF CHAR;  D: Domain): User;

    (* Finds this user in the master list of users, or creates a new    *)
    (* user record if it doesn't already exist.                         *)

    VAR previous, current: ListOfUsers;

    BEGIN
        (* Search the Masterlist for the record for this user.  If it   *)
        (* does not exist, add a new record to the Masterlist.          *)

        Obtain (MasterlistLock);
        previous := NIL;  current := Masterlist;
        WHILE (current <> NIL)
                 AND NOT Strings.Equal (username, current^.this^.name) DO
            previous := current;
            current := current^.next;
        END (*WHILE*);

        IF current = NIL THEN

            (* No user record found, create a new one. *)

            NEW(current);
            current^.next := NIL;
            current^.this := MakeUser (username, D);
            IF previous = NIL THEN
                Masterlist := current;
            ELSE
                previous^.next := current;
            END (*IF*);
        END (*IF*);

        Release (MasterlistLock);

        RETURN current^.this;

    END FindUser;

(************************************************************************)

PROCEDURE DiscardUser (VAR (*INOUT*) U: User);

    (* Discards the user record. *)

    VAR previous, current: ListOfUsers;

    BEGIN
        IF U <> NIL THEN

            (* Detach this user record from the master list. *)

            Obtain (MasterlistLock);
            previous := NIL;  current := Masterlist;
            WHILE (current <> NIL) AND (current^.this <> U) DO
                previous := current;
                current := current^.next;
            END (*WHILE*);
            IF current <> NIL THEN
                 IF previous = NIL THEN
                     Masterlist := current^.next;
                 ELSE
                     previous^.next := current^.next;
                 END (*IF*);
                 DISPOSE (current);
            END (*IF*);
            Release (MasterlistLock);

            (* Now remove the data held in the user record,     *)
            (* then dispose of the record.  We may assume that  *)
            (* its list of instances is already empty.          *)

            CloseBoxList (U^.boxlist);
            DestroyLock (U^.access);
            DISPOSE (U);
        END (*IF*);

    END DiscardUser;

(************************************************************************)

PROCEDURE UCheckpoint (U: User);

    (* Brings the user information up to date. *)

    BEGIN
        IF U <> NIL THEN
            Obtain (U^.access);
            UpdateBoxList (U^.boxlist);
            Release (U^.access);
        END (*IF*);
    END UCheckpoint;

(************************************************************************)

PROCEDURE UDoListing (RC: ReplyCxt;  U: User;
                           VAR (*IN*) mask: FilenameString;
                                            IncludeAll: BOOLEAN): BOOLEAN;

    (* Responds to the LIST or LSUB command.  The IncludeAll parameter  *)
    (* is TRUE for LIST, FALSE FOR LSUB.                                *)

    (* The mask parameter may include wildcards.  In the mask, '*'      *)
    (* matches anything including the hierarchy delimiter, and '%' is   *)
    (* like '*' except that it doesn't match the hierarchy delimiter.   *)

    VAR success: BOOLEAN;
        pos: CARDINAL;
        name: FilenameString;

    BEGIN
        success := TRUE;
        IF IncludeAll AND (mask[0] = Nul) THEN

            (* Special case; the client wants to know the directory     *)
            (* delimiter and whether the references are rooted.         *)

            ReplyUntagged (RC, 'LIST (\Noselect) "/" ""');
        ELSE
            Strings.FindNext ('*', mask, 0, success, pos);
            IF NOT success THEN
                Strings.FindNext ('%', mask, 0, success, pos);
            END (*IF*);
            Obtain (U^.access);
            IF NOT success THEN
                Strings.Assign (U^.directory, name);
                Strings.Append ('\', name);
                Strings.Append (mask, name);
                success := FileSys.Exists (name);
            END (*IF*);
            IF success THEN
                UpdateBoxList (U^.boxlist);
                name := "";
                ListTo (RC, U^.boxlist, name, mask, IncludeAll);
            END (*IF*);
            Release (U^.access);
        END (*IF*);

        RETURN success;

    END UDoListing;

(************************************************************************)

PROCEDURE MakeSubdirectory (dir: FilenameString;
                             subdir: ARRAY OF CHAR;
                             VAR (*OUT*) alreadyexists: BOOLEAN): BOOLEAN;

    (* Attempts to create a new directory dir\subdir.  Can handle the   *)
    (* case where subdir contains '/' level separators.  Returns FALSE  *)
    (* if the directory already exists, or if for any other reason      *)
    (* we can't create it.                                              *)

    VAR success: BOOLEAN;
        head: FilenameString;

    BEGIN
        alreadyexists := FALSE;
        HeadSplit (head, subdir);
        IF head[0] = Nul THEN
            success := TRUE;
        ELSE
            Strings.Append ('\', dir);
            Strings.Append (head, dir);
            IF subdir[0] = Nul THEN
                IF FileSys.Exists (dir) THEN
                    alreadyexists := TRUE;
                    success := FALSE;
                ELSE
                    success := FileSys.CreateDirectory (dir);
                END (*IF*);
            ELSE
                success := MakeSubdirectory (dir, subdir, alreadyexists);
            END (*IF*);
        END (*IF*);
        RETURN success;
    END MakeSubdirectory;

(************************************************************************)
(*                       OPERATIONS ON MAILBOXES                        *)
(************************************************************************)

PROCEDURE UCreateBox (U: User;  VAR (*IN*) boxname: ARRAY OF CHAR;
                             VAR (*OUT*) alreadyexists: BOOLEAN): BOOLEAN;

    (* Creates a new mailbox.  Note that this will not immediately      *)
    (* appear in the BoxList, but will be added there when the user     *)
    (* lists or selects new boxes.                                      *)

    VAR result: BOOLEAN;

    BEGIN
        Obtain (U^.access);
        IF boxname[0] = Nul THEN
            alreadyexists := FALSE;
            result := FALSE;
        ELSE
            result := MakeSubdirectory (U^.directory, boxname, alreadyexists);
        END (*IF*);
        Release (U^.access);
        RETURN result;
    END UCreateBox;

(************************************************************************)

PROCEDURE UDeleteBox (U: User;  VAR (*IN*) boxname: ARRAY OF CHAR): BOOLEAN;

    (* Deletes a mailbox, including deleting all the files it contains. *)
    (* If it has subdirectories, it acquires the \Noselect attribute.   *)
    (* If it has subdirectories and already has the \Noselect attribute *)
    (* the deletion attempt will fail.                                  *)

    VAR result: BOOLEAN;

    BEGIN
        Obtain (U^.access);
        IF boxname[0] = Nul THEN
            result := FALSE;
        ELSE
            UpdateBoxList (U^.boxlist);
            result := RemoveBox (U^.boxlist, boxname);
        END (*IF*);
        Release (U^.access);
        RETURN result;
    END UDeleteBox;

(************************************************************************)

PROCEDURE USubscribeToBox (U: User;
                           VAR (*INOUT*) boxname: ARRAY OF CHAR): BOOLEAN;

    (* The client is subscribing to this mailbox. *)

    VAR success: BOOLEAN;

    BEGIN
        Obtain (U^.access);
        UpdateBoxList (U^.boxlist);
        success := DoSubscribe (FindBox (U^.boxlist, boxname));
        Release (U^.access);
        RETURN success;
    END USubscribeToBox;

(************************************************************************)

PROCEDURE MoveAllMessages (U: User;  olddir, newdir: ARRAY OF CHAR;
                                     MoveSubdirectories: BOOLEAN);

    (* Moves all message files from olddir to newdir.  Also moves all   *)
    (* subdirectories if MoveSubdirectories is TRUE.                    *)

    VAR D: DirectoryEntry;
        found: BOOLEAN;
        src, dst, src0, dst0, mask: FilenameString;

    BEGIN
        src0 := U^.directory;  dst0 := src0;
        Strings.Append ("\", src0);
        IF olddir[0] <> Nul THEN
            Strings.Append (olddir, src0);
            Strings.Append ("\", src0);
        END (*IF*);
        IF newdir[0] <> Nul THEN
            Strings.Append ("\", dst0);
            Strings.Append (newdir, dst0);
        END (*IF*);
        mask := src0;
        Strings.Append ("*.MSG", mask);

        found := FirstDirEntry (mask, FALSE, TRUE, D);
        WHILE found DO
            src := src0;
            Strings.Append (D.name, src);
            dst := dst0;
            Strings.Append (D.name, dst);
            EVAL (MoveFile (src, dst));
            found := NextDirEntry (D);
        END (*WHILE*);
        DirSearchDone (D);

        (* Now move the subdirectories, if required.  This code *)
        (* needs to be tested, because it's not clear from the  *)
        (* API documentation that lower subdirectories will be  *)
        (* moved.                                               *)

        IF MoveSubdirectories THEN
            Strings.Append ("\", dst0);
            mask := src0;
            Strings.Append ("*", mask);

            found := FirstDirEntry (mask, TRUE, TRUE, D);
            WHILE found DO

                (* Remember to ignore the '.' and '..' entries. *)

                IF (D.name[0] <> '.') OR (
                          (D.name[1] <> Nul)
                           AND ((D.name[1] <> '.') OR (D.name[2] <> Nul))
                                         ) THEN
                    src := src0;
                    Strings.Append (D.name, src);
                    dst := dst0;
                    Strings.Append (D.name, dst);
                    EVAL (MoveFile (src, dst));
                END (*IF*);
                found := NextDirEntry (D);
            END (*WHILE*);
            DirSearchDone (D);
        END (*IF*);

    END MoveAllMessages;

(************************************************************************)

PROCEDURE URenameBox (U: User;
                  VAR (*INOUT*) oldname, newname: ARRAY OF CHAR;
                  VAR (*OUT*) targetexists: BOOLEAN): BOOLEAN;

    (* Renames a mailbox. *)

    VAR success: BOOLEAN;

    BEGIN
        success := UCreateBox (U, newname, targetexists);
        IF success THEN
            Obtain (U^.access);
            IF StringMatch (oldname, "INBOX") THEN

                (* Move all inbox files to the new directory, which has     *)
                (* just been created.                                       *)

                MoveAllMessages (U, "", newname, FALSE);

            ELSE
                (* This is not a simple file system "rename", because the   *)
                (* two names might be at different hierarchical levels in   *)
                (* the file system.  The best solution, I think, is to do   *)
                (* the operation as a three-step process: create new        *)
                (* mailbox, move the messages and subdirectories, then      *)
                (* delete the old mailbox.                                  *)

                (* In addition, we have to call BoxLists.DoRename to update *)
                (* its data structures.                                     *)

                MoveAllMessages (U, oldname, newname, TRUE);
                DoRename (U^.boxlist, oldname, newname);
                success := UDeleteBox (U, oldname);

            END (*IF*);
            Release (U^.access);
        END (*IF*);
        RETURN success;
    END URenameBox;

(************************************************************************)

PROCEDURE UUnsubscribeToBox (U: User;
                             VAR (*IN*) boxname: ARRAY OF CHAR): BOOLEAN;

    (* The client is unsubscribing from this mailbox. *)

    VAR success: BOOLEAN;

    BEGIN
        Obtain (U^.access);
        UpdateBoxList (U^.boxlist);
        success := DoUnsubscribe (FindBox (U^.boxlist, boxname));
        Release (U^.access);
        RETURN success;
    END UUnsubscribeToBox;

(************************************************************************)

PROCEDURE UFindBox (U: User;  VAR (*IN*) boxname: ARRAY OF CHAR): BoxPtr;

    (* Returns a pointer to the mailbox in the user's boxlist tree.     *)
    (* Returns NIL if no box found.                                     *)

    VAR result: BoxPtr;

    BEGIN
        Obtain (U^.access);
        result := FindBox (U^.boxlist, boxname);
        Release (U^.access);
        RETURN result;
    END UFindBox;

(************************************************************************)

PROCEDURE UGetStatus (RC: ReplyCxt;  U: User;
                        VAR (*IN*) boxname, args: ARRAY OF CHAR): BOOLEAN;

    (* Responds to the STATUS command.  The 'args' argument should be a *)
    (* parenthesised list.                                              *)

    VAR UIDV: CARDINAL;  result, IsInbox: BOOLEAN;
        BP: BoxPtr;  MBU: MailboxUser;
        dir: FilenameString;

    BEGIN
        IsInbox := MatchNoDelete (boxname, 'INBOX');
        Obtain (U^.access);
        BP := FindBox (U^.boxlist, boxname);
        UIDV := UIDValidityCode (BP);
        DirectoryOf (BP, dir);
        MBU := OpenMailbox (RC, dir, IsInbox, TRUE);
        result := DoStatusReply (MBU, UIDV, boxname, args);
        CloseMailboxUser (MBU);
        Release (U^.access);
        RETURN result;
    END UGetStatus;

(************************************************************************)

PROCEDURE UAddMessage (RC: ReplyCxt;  U: User;  BP: BoxPtr;
                       IsInbox: BOOLEAN;
                       flags: MessageFlagSet;
                       date, time, count: CARDINAL): BOOLEAN;

    (* Creates a new message in box BP^, whose content is the     *)
    (* next 'count' characters to be fetched from the client.     *)
    (* The file date/time is set iff date > 0.                    *)

    VAR result: BOOLEAN;
        MBU: MailboxUser;
        dir: FilenameString;

    BEGIN
        Obtain (U^.access);
        DirectoryOf (BP, dir);
        MBU := OpenMailbox (RC, dir, IsInbox, TRUE);
        result := DoAddMessage (MBU, flags, date, time, count);
        CloseMailboxUser (MBU);
        Release (U^.access);
        RETURN result;
    END UAddMessage;

(************************************************************************)
(*                           INITIALISATION                             *)
(************************************************************************)

BEGIN
    StarID := CreateLogID (WCtx, "*      ");
    Masterlist := NIL;
    CreateLock (MasterlistLock);
FINALLY
    DestroyLock (MasterlistLock);
    DiscardLogID (StarID);
END Users.

