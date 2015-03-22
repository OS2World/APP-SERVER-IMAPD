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

IMPLEMENTATION MODULE IMCommands;

        (********************************************************)
        (*                                                      *)
        (*       Command interpreter for IMAP4 server           *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            2 March 2001                    *)
        (*  Last edited:        2 January 2014                  *)
        (*  Status:             All commands implemented, and a *)
        (*                      large subset tested and working *)
        (*                                                      *)
        (********************************************************)

(********************************************************************************)
(*                        COMPLIANCE WITH THE STANDARD                          *)
(********************************************************************************)
(*                                                                              *)
(* I'm working from the IMAP4rev1 standard RFC2060.  All commands in that RFC   *)
(* are recognised by this module, although not all are completely implemented.  *)
(*                                                                              *)
(* Commands believed to be implemented completely:                              *)
(*    AUTHENTICATE, CAPABILITY, CHECK, COPY, CLOSE, DELETE, EXAMINE, EXPUNGE,   *)
(*    LOGIN, LOGOUT, SELECT, STORE, SUBSCRIBE, UNSUBSCRIBE, UID COPY, UID STORE *)
(*                                                                              *)
(* Implemented but not fully tested:                                            *)
(*    SEARCH, UID SEARCH                                                        *)
(*    RENAME (subdirectories now being handled correctly, still need to check   *)
(*            that messages are copied)                                         *)
(*    APPEND (saving correctly, but date/time option is untested)               *)
(*                                                                              *)
(* Commands believed to be completely implemented, subject to a further         *)
(*        check against the standard:                                           *)
(*    CREATE, LIST, LSUB                                                        *)
(*                                                                              *)
(* Implemented but with options missing:                                        *)
(*    FETCH, UID FETCH                                                          *)
(*                                                                              *)
(* Now working on:                                                              *)
(*    FETCH, UID FETCH                                                          *)
(*                                                                              *)
(* Next priorities: NOOP                                                        *)
(*                                                                              *)
(* Commands partly implemented:                                                 *)
(*    NOOP (OK except for pending replies)                                      *)
(*                                                                              *)
(* Other quirks and bugs:                                                       *)
(*    APPEND: If the mailbox to which we're appending is the one that is        *)
(*            already selected, we get the EXISTS reply twice.  I doubt that    *)
(*            this really matters.                                              *)
(*    AUTHENTICATE: sometimes (but not consistently) CPU load goes to 100%      *)
(*            when opening a new connection from Mozilla.  I have not been      *)
(*            able to work out whether this is a Mozilla bug or an IMAP4        *)
(*            bug.                                                              *)
(*                                                                              *)
(* Extensions to the standard:                                                  *)
(*    NAMESPACE (RFC 2342)                                                      *)
(*                                                                              *)
(********************************************************************************)

IMPORT Strings;

FROM Parser IMPORT
    (* type *)  MessageFlagSet,
    (* proc *)  SPACE, Atom, AString, AStringW, FlagList, DateTime,
                Literal, Match;

FROM MSet IMPORT
    (* type *)  MessageSet,
    (* proc *)  GetSet, NonEmptySet;

FROM Authentication IMPORT
    (* type *)  AuthenticationState,
    (* proc *)  StartAuthentication, CreateNextChallenge, CheckResponse,
                AuthenticationIncomplete, GetAuthNames, AuthenticationDone;

FROM Domains IMPORT
    (* type *)  Domain,
    (* proc *)  NameOfDomain;

FROM UserInstances IMPORT
    (* type *)  UserInstance,
    (* proc *)  LoginUser, CreateUserInstance, CloseUserInstance, DoListing,
                SelectBox, SecondarySelectBox, DeselectBox, CreateBox,
                RenameBox, SubscribeToBox, UnsubscribeFromBox, DoCheckpoint,
                DoFetch, DoStore, Expunge, DoCopy, SearchBox,
                GetStatus, AddMessage, FindBox, StatusUpdate, DeleteBox;

FROM BoxLists IMPORT
    (* type *)  BoxPtr,
    (* proc *)  BoxExists;

FROM Replies IMPORT
    (* type *)  ReplyCxt,
    (* proc *)  OpenReplyGroup, CloseReplyGroup;

FROM SBuffers IMPORT
    (* type *)  SBuffer,
    (* proc *)  SendString, SendLine, FlushOutput, GetLine;

FROM Inet2Misc IMPORT
    (* proc *)  StringMatch;

FROM Names IMPORT
    (* type *)  FilenameString, UserName, ArgStringIndex, DomainName;

FROM TransLog IMPORT
    (* type *)  TransactionLogID,
    (* proc *)  LogTransaction, LogTransactionL;

FROM Semaphores IMPORT
    (* type *)  Semaphore;

FROM LowLevel IMPORT
    (* proc *)  EVAL;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

(********************************************************************************)

CONST
    Nul = CHR(0);
    MaxTagChars = 32;

TYPE
    TagType = ARRAY [0..MaxTagChars-1] OF CHAR;

    ClientState = (MustExit, nonAuthenticated, Authenticated, Selected);

    (* The session record.  The fields are:                             *)
    (*     ID          a session identifier for transaction logging     *)
    (*     SB          The socket buffer                                *)
    (*     ReplyEnv    session context for module Replies               *)
    (*     HostAddr    our own IP address                               *)
    (*     state       To track whether the user is currently logged in.*)
    (*     Tag         The tag for the command in progress              *)
    (*     userdata    record for the logged-in user                    *)

    Session = POINTER TO
                  RECORD
                      ID: TransactionLogID;
                      SB: SBuffer;
                      ReplyEnv: ReplyCxt;
                      HostAddr: CARDINAL;
                      state: ClientState;
                      Tag: TagType;
                      userdata: UserInstance;
                  END (*RECORD*);

(********************************************************************************)
(*                       THE TABLE OF COMMAND HANDLERS                          *)
(********************************************************************************)

CONST
    StringTableSize = 158;
    CommandTableSize = 26;

TYPE
    StringTableIndex = [0..StringTableSize-1];
    CommandTableIndex = [0..CommandTableSize-1];
    HandlerProc = PROCEDURE (Session, VAR (*IN*) ARRAY OF CHAR): BOOLEAN;

VAR
    (* We store the command strings as one long concatenation.  Individual      *)
    (* strings are identified by giving their origin and length.                *)

    StringTable: ARRAY StringTableIndex OF CHAR;

    (* First unused position in the string table. *)

    NextString: StringTableIndex;

    (* Each entry in the command table is a record with fields      *)
    (*                                                              *)
    (*    strstart, strlen:  the command in the string table        *)
    (*    CmdHandler: procedure to handle this command              *)
    (*    StateNeeded: says when this command is legal.             *)
    (*                                                              *)
    (* The Stateneeded field is to be interpreted as follows:       *)
    (*                                                              *)
    (*    MustExit: the command is legal in any state               *)
    (*    nonAuthenticated: the command is legal only in the        *)
    (*                      nonAuthenticated state                  *)
    (*    Authenticated: the command is legal in both the           *)
    (*                      Authenticated and Selected states       *)
    (*    Selected: the command is legal only in the Selected state *)

    CommandTable: ARRAY CommandTableIndex OF
                         RECORD
                             strstart: StringTableIndex;
                             strlen: CARDINAL;
                             CmdHandler: HandlerProc;
                             StateNeeded: ClientState;
                         END (*RECORD*);

    NoOfCommands: CARDINAL;

(********************************************************************************)
(*                         STARTING A NEW SESSION                               *)
(********************************************************************************)

PROCEDURE OpenSession (SBuff: SBuffer;  ServerIPAddress, ClientIPAddress: CARDINAL;
                                    KeepAlive: Semaphore;
                                    LogID: TransactionLogID): Session;

    (* Creates a new session state record.  During lengthy operations           *)
    (* we have to do a Signal(KeepAlive) every so often in order to stop the    *)
    (* session from timing out.                                                 *)

    VAR result: Session;

    BEGIN
        NEW (result);
        WITH result^ DO
            ID := LogID;
            SB := SBuff;
            ReplyEnv := OpenReplyGroup (SB, ID);
            HostAddr := ServerIPAddress;
            state := nonAuthenticated;
            Tag := "";
            userdata := NIL;
        END (*WITH*);
        RETURN result;
    END OpenSession;

(********************************************************************************)

PROCEDURE CloseSession (S: Session);

    (* Destroys the session state record.  We do not discard the transaction    *)
    (* log ID or the SBuffer or the KeepAlive semaphore, because those belong   *)
    (* to the caller.                                                           *)

    BEGIN
        CloseUserInstance (S^.userdata);
        CloseReplyGroup (S^.ReplyEnv);
        FlushOutput (S^.SB);
        DISPOSE (S);
    END CloseSession;

(********************************************************************************)
(*                       SENDING REPLY BACK TO CLIENT                           *)
(********************************************************************************)

PROCEDURE Reply (session: Session;  str: ARRAY OF CHAR);

    BEGIN
        IF SendLine (session^.SB, str) THEN
            LogTransaction (session^.ID, str);
        ELSE
            session^.state := MustExit;
        END (*IF*);
    END Reply;

(********************************************************************************)

PROCEDURE Reply2 (session: Session;  str1, str2: ARRAY OF CHAR);

    VAR buffer: ARRAY [0..511] OF CHAR;

    BEGIN
        Strings.Append (str1, buffer);
        Strings.Append (str2, buffer);
        IF SendLine (session^.SB, buffer) THEN
            LogTransaction (session^.ID, buffer);
        ELSE
            session^.state := MustExit;
        END (*IF*);
    END Reply2;

(********************************************************************************)

PROCEDURE ReplyUntagged (session: Session;  str: ARRAY OF CHAR);

    VAR buffer: ARRAY [0..511] OF CHAR;

    BEGIN
        Strings.Assign ("* ", buffer);
        Strings.Append (str, buffer);
        IF SendLine (session^.SB, buffer) THEN
            LogTransaction (session^.ID, buffer);
        ELSE
            session^.state := MustExit;
        END (*IF*);
    END ReplyUntagged;

(********************************************************************************)

PROCEDURE ReplyWithTag (session: Session;  str: ARRAY OF CHAR);

    VAR buffer: ARRAY [0..511] OF CHAR;

    BEGIN
        Strings.Assign (session^.Tag, buffer);
        Strings.Append (' ', buffer);
        Strings.Append (str, buffer);
        IF SendLine (session^.SB, buffer) THEN
            LogTransaction (session^.ID, buffer);
        ELSE
            session^.state := MustExit;
        END (*IF*);
    END ReplyWithTag;

(********************************************************************************)

PROCEDURE Reply2WithTag (session: Session;  str1, str2: ARRAY OF CHAR);

    VAR buffer: ARRAY [0..511] OF CHAR;

    BEGIN
        Strings.Assign (session^.Tag, buffer);
        Strings.Append (' ', buffer);
        Strings.Append (str1, buffer);
        Strings.Append (str2, buffer);
        IF SendLine (session^.SB, buffer) THEN
            LogTransaction (session^.ID, buffer);
        ELSE
            session^.state := MustExit;
        END (*IF*);
    END Reply2WithTag;

(********************************************************************************)

PROCEDURE Reply3WithTag (session: Session;  str1, str2, str3: ARRAY OF CHAR);

    VAR buffer: ARRAY [0..511] OF CHAR;

    BEGIN
        Strings.Assign (session^.Tag, buffer);
        Strings.Append (' ', buffer);
        Strings.Append (str1, buffer);
        Strings.Append (str2, buffer);
        Strings.Append (str3, buffer);
        IF SendLine (session^.SB, buffer) THEN
            LogTransaction (session^.ID, buffer);
        ELSE
            session^.state := MustExit;
        END (*IF*);
    END Reply3WithTag;

(********************************************************************************)
(*                     HANDLERS FOR SOME ERROR CONDITIONS                       *)
(********************************************************************************)

PROCEDURE NoSuchCommand (session: Session;  VAR (*IN*) Command: ARRAY OF CHAR): BOOLEAN;

    (* Command is not a recognised command. *)

    BEGIN
        Reply2WithTag (session, "BAD Unrecognised command", Command);
        RETURN FALSE;
    END NoSuchCommand;

(********************************************************************************)

PROCEDURE NotLoggedIn (session: Session;  VAR (*IN*) dummy: ARRAY OF CHAR): BOOLEAN;

    (* Command is illegal because user is not yet logged in. *)

    BEGIN
        dummy[0] := dummy[0];      (* to remove a compiler warning *)
        ReplyWithTag (session, "BAD You are not logged in");
        RETURN FALSE;
    END NotLoggedIn;

(********************************************************************************)

PROCEDURE AlreadyLoggedIn (session: Session;  VAR (*IN*) dummy: ARRAY OF CHAR): BOOLEAN;

    (* Command is illegal because user is already logged in. *)

    BEGIN
        dummy[0] := dummy[0];      (* to remove a compiler warning *)
        ReplyWithTag (session, "BAD You are already logged in");
        RETURN FALSE;
    END AlreadyLoggedIn;

(********************************************************************************)

PROCEDURE NoMailboxSelected (session: Session;  VAR (*IN*) dummy: ARRAY OF CHAR): BOOLEAN;

    (* Command is illegal because user is not in Selected state. *)

    BEGIN
        dummy[0] := dummy[0];      (* to remove a compiler warning *)
        ReplyWithTag (session, "BAD No mailbox is selected");
        RETURN FALSE;
    END NoMailboxSelected;

(********************************************************************************)
(*                     HANDLERS FOR THE INDIVIDUAL COMMANDS                     *)
(********************************************************************************)

PROCEDURE NullCommand (session: Session;  VAR (*IN*) args: ARRAY OF CHAR): BOOLEAN;

    (* To be tolerant, we accept an empty command at any time. *)

    BEGIN
        args[0] := args[0];      (* to suppress a compiler warning *)
        RETURN session <> NIL;
    END NullCommand;

(********************************************************************************)

PROCEDURE APPEND (session: Session;  VAR (*IN*) args: ARRAY OF CHAR): BOOLEAN;

    VAR success, IsInbox: BOOLEAN;  date, time, count: CARDINAL;
        boxname: FilenameString;
        flags: MessageFlagSet;
        BP: BoxPtr;

    BEGIN
        date := 0;
        time := 0;
        count := 0;
        BP := NIL;
        flags := MessageFlagSet{};
        IsInbox := FALSE;
        success := AString (args, boxname) AND SPACE(args);
        IF success THEN
            IsInbox := StringMatch (boxname, 'INBOX');
            BP := FindBox (session^.userdata, boxname);
            success := BoxExists (BP);
            IF NOT success THEN
                ReplyWithTag (session, "NO [TRYCREATE] Mailbox does not exist");
            END (*IF*);
        END (*IF*);
        IF success AND (args[0] = '(') THEN
            success := FlagList(args, flags);
        END (*IF*);
        IF success THEN
            WHILE SPACE(args) DO
            END (*WHILE*);
            IF args[0] = '"' THEN
                success := DateTime(args, date, time) AND SPACE(args);
            END (*IF*);
        END (*IF*);
        IF success THEN
            WHILE SPACE(args) DO
            END (*WHILE*);
            success := Literal(args, count);
        END (*IF*);
        IF success THEN
            success := AddMessage (session^.userdata, BP, IsInbox, flags,
                                                      date, time, count);
        ELSE
            ReplyWithTag (session, "BAD Syntax error in APPEND");
        END (*IF*);
        RETURN success;
    END APPEND;

(********************************************************************************)

PROCEDURE AUTHENTICATE (session: Session;  VAR (*IN*) args: ARRAY OF CHAR): BOOLEAN;

    VAR mechanism: ARRAY [0..20] OF CHAR;
        message:  ARRAY [0..511] OF CHAR;
        state: AuthenticationState;
        working, authenticated: BOOLEAN;
        username: UserName;
        domain: Domain;
        name: DomainName;

    BEGIN
        authenticated := FALSE;
        IF Atom (FALSE, args, mechanism) THEN
            IF NOT (SPACE(args) AND AString (args, message)) THEN
                message[0] := Nul;
            END (*IF*);
            Strings.Capitalize (mechanism);
            working := TRUE;
            IF StartAuthentication (state, session^.HostAddr,
                                    MAX(CARDINAL), mechanism, message) THEN
                WHILE working AND AuthenticationIncomplete (state, authenticated) DO
                    CreateNextChallenge (state, message);
                    IF message[0] = Nul THEN
                        Reply (session, "+");
                    ELSE
                        Reply2 (session, "+ ", message);
                    END (*IF*);
                    IF GetLine (session^.SB, message) THEN
                        LogTransaction (session^.ID, message);
                        IF (message[0] = '*') AND (message[1] = Nul) THEN
                            ReplyWithTag (session, "BAD Authentication cancelled");
                            working := FALSE;
                        ELSE
                            CheckResponse (state, message);
                        END (*IF*);
                    ELSE
                        session^.state := MustExit;
                        working := FALSE;
                        ReplyWithTag (session, "NO Connection lost");
                    END (*IF*);
                END (*WHILE*);
                IF NOT authenticated THEN
                    ReplyWithTag (session, "NO Authentication failed");
                END (*IF*);
            ELSE
                ReplyWithTag (session, "NO Unsupported authentication mechanism ");
            END (*IF*);
        ELSE
            ReplyWithTag (session, "BAD No authentication mechanism specified");
        END (*IF*);
        AuthenticationDone (state, username, domain);
        IF authenticated THEN
            Strings.Assign ("Authentication successful for domain ", message);
            NameOfDomain (domain, name);
            Strings.Append (name, message);
            LogTransaction (session^.ID, message);

            IF CreateUserInstance (session^.userdata, session^.SB,
                          username, domain, session^.ReplyEnv, session^.ID) THEN

                session^.state := Authenticated;
                RETURN TRUE;
            ELSE
                ReplyWithTag (session, "NO Not a valid IMAP user");
                RETURN FALSE;
            END (*IF*);
        ELSE
            LogTransactionL (session^.ID, "No password match for any domain.");
        END (*IF*);
        RETURN authenticated;

    END AUTHENTICATE;

(********************************************************************************)

PROCEDURE CAPABILITY (session: Session;  VAR (*IN*) args: ARRAY OF CHAR): BOOLEAN;

    (* Netscape & Mozilla can handle AUTH=PLAIN *)
    (* PolarBar can handle AUTH=LOGIN *)
    (* I can't find any client that does CRAM-MD5 or KERBEROS *)

    VAR list, reply: ARRAY [0..511] OF CHAR;

    BEGIN
        args[0] := args[0];      (* to suppress a compiler warning *)
        GetAuthNames (list, TRUE);
        Strings.Assign ("CAPABILITY IMAP4rev1 NAMESPACE", reply);
        Strings.Append (list, reply);
        ReplyUntagged (session, reply);
        RETURN TRUE;
    END CAPABILITY;

(********************************************************************************)

PROCEDURE CHECK (session: Session;  VAR (*IN*) args: ARRAY OF CHAR): BOOLEAN;

    BEGIN
        args[0] := args[0];      (* to suppress a compiler warning *)
        DoCheckpoint (session^.userdata);
        RETURN TRUE;
    END CHECK;

(********************************************************************************)

PROCEDURE CLOSE (session: Session;  VAR (*IN*) args: ARRAY OF CHAR): BOOLEAN;

    BEGIN
        args[0] := args[0];      (* to suppress a compiler warning *)
        DeselectBox (session^.userdata);
        session^.state := Authenticated;
        RETURN TRUE;
    END CLOSE;

(********************************************************************************)

PROCEDURE COPY (session: Session;  VAR (*IN*) args: ARRAY OF CHAR): BOOLEAN;

    VAR set: MessageSet;  box: FilenameString;

    BEGIN
        set := GetSet(args);
        IF NonEmptySet(set) AND AString (args, box) THEN
            IF SecondarySelectBox (session^.userdata, box) THEN
                IF DoCopy (session^.userdata, set, FALSE) THEN
                    RETURN TRUE;
                ELSE
                    ReplyWithTag (session, "NO COPY failed");
                    RETURN FALSE;
                END (*IF*);
            ELSE
                ReplyWithTag (session, "NO [TRYCREATE] invalid destination");
                RETURN FALSE;
            END (*IF*);
        ELSE
            ReplyWithTag (session, "BAD no valid destination specified");
            RETURN FALSE;
        END (*IF*);
    END COPY;

(********************************************************************************)

PROCEDURE CREATE (session: Session;  VAR (*IN*) args: ARRAY OF CHAR): BOOLEAN;

    VAR boxname: FilenameString;
        alreadyexists: BOOLEAN;

    BEGIN
        IF AString (args, boxname) THEN
            IF CreateBox (session^.userdata, boxname, alreadyexists) THEN
                RETURN TRUE;
            ELSIF alreadyexists THEN
                ReplyWithTag (session, "NO Mailbox already exists");
                RETURN FALSE;
            ELSE
                ReplyWithTag (session, "NO Cannot create mailbox");
                RETURN FALSE;
            END (*IF*);
        ELSE
            ReplyWithTag (session, "BAD No valid mailbox name specified");
            RETURN FALSE;
        END (*IF*);
    END CREATE;

(********************************************************************************)

PROCEDURE DELETE (session: Session;  VAR (*IN*) args: ARRAY OF CHAR): BOOLEAN;

    VAR boxname: FilenameString;

    BEGIN
        IF AString (args, boxname) THEN
            IF DeleteBox (session^.userdata, boxname) THEN
                RETURN TRUE;
            ELSE
                ReplyWithTag (session, "NO Cannot delete mailbox");
                RETURN FALSE;
            END (*IF*);
        ELSE
            ReplyWithTag (session, "BAD No valid mailbox specified");
            RETURN FALSE;
        END (*IF*);
    END DELETE;

(********************************************************************************)

PROCEDURE EXAMINE (session: Session;  VAR (*IN*) args: ARRAY OF CHAR): BOOLEAN;

    VAR boxname: FilenameString;  success: BOOLEAN;

    BEGIN
        success := AString (args, boxname)
                   AND SelectBox (session^.userdata, boxname, TRUE);
        IF success THEN
            session^.state := Selected;
            ReplyWithTag (session, " OK [READ-ONLY] EXAMINE completed");
        ELSE
            session^.state := Authenticated;
            ReplyWithTag (session, " NO EXAMINE failed");
        END (*IF*);
        RETURN FALSE;
    END EXAMINE;

(********************************************************************************)

PROCEDURE EXPUNGE (session: Session;  VAR (*IN*) args: ARRAY OF CHAR): BOOLEAN;

    BEGIN
        args[0] := args[0];      (* to suppress a compiler warning *)
        IF Expunge (session^.userdata) THEN
            RETURN TRUE;
        ELSE
            ReplyWithTag (session, "NO [READ-ONLY] Mailbox not writeable");
            RETURN FALSE;
        END (*IF*);
    END EXPUNGE;

(********************************************************************************)

PROCEDURE FETCH (session: Session;  VAR (*IN*) args: ARRAY OF CHAR): BOOLEAN;

    BEGIN
        IF DoFetch (session^.userdata, args, FALSE, session^.ID) THEN
            RETURN TRUE;
        ELSE
            ReplyWithTag (session, "NO Fetch failed");
            RETURN FALSE;
        END (*IF*);
    END FETCH;

(********************************************************************************)

PROCEDURE LIST (session: Session;  VAR (*IN*) args: ARRAY OF CHAR): BOOLEAN;

    VAR refname, mailbox: FilenameString;

    BEGIN
        IF AString (args, refname) AND SPACE(args)
                   AND AStringW (TRUE, args, mailbox) THEN
            IF DoListing (session^.userdata, refname, mailbox, TRUE) THEN
                RETURN TRUE;
            ELSE
                ReplyWithTag (session, "NO Can't list that reference or name");
                RETURN FALSE;
            END (*IF*);
        ELSE
            ReplyWithTag (session, "BAD Syntax error");
            RETURN FALSE;
        END (*IF*);
    END LIST;

(********************************************************************************)

PROCEDURE LOGIN (session: Session;  VAR (*IN*) args: ARRAY OF CHAR): BOOLEAN;

    VAR username, password: ARRAY [0..127] OF CHAR;

    BEGIN
        IF AString (args, username) AND SPACE(args) AND AString (args, password) THEN
            IF LoginUser (session^.userdata, session^.SB,
                            username, password, session^.HostAddr,
                              session^.ReplyEnv, session^.ID) THEN
                session^.state := Authenticated;
                RETURN TRUE;
            ELSE
                ReplyWithTag (session, "NO Login failure");
                RETURN FALSE;
            END (*IF*);
        ELSE
            ReplyWithTag (session, "BAD Syntax error");
            RETURN FALSE;
        END (*IF*);
    END LOGIN;

(********************************************************************************)

PROCEDURE LOGOUT (session: Session;  VAR (*IN*) args: ARRAY OF CHAR): BOOLEAN;

    BEGIN
        args[0] := args[0];      (* to suppress a compiler warning *)
        ReplyUntagged (session, "BYE IMAP4 server terminating connection");
        session^.state := MustExit;

        (* All remaining tidying up (closing the currently selected mailbox,    *)
        (* etc., will be done by the caller as the session is closed.           *)

        RETURN TRUE;
    END LOGOUT;

(********************************************************************************)

PROCEDURE LSUB (session: Session;  VAR (*IN*) args: ARRAY OF CHAR): BOOLEAN;

    VAR refname, mailbox: FilenameString;

    BEGIN
        IF AString (args, refname) AND SPACE(args)
                   AND AStringW (TRUE, args, mailbox)
                   AND DoListing (session^.userdata, refname, mailbox, FALSE) THEN
            RETURN TRUE;
        ELSE
            ReplyWithTag (session, "NO Can't list that reference or name");
            RETURN FALSE;
        END (*IF*);
    END LSUB;

(********************************************************************************)

PROCEDURE NAMESPACE (session: Session;  VAR (*IN*) args: ARRAY OF CHAR): BOOLEAN;

    BEGIN
        args[0] := args[0];      (* to suppress a compiler warning *)
        ReplyUntagged (session, 'NAMESPACE (("" "/")) NIL NIL');
        RETURN TRUE;
    END NAMESPACE;

(********************************************************************************)

PROCEDURE NOOP (session: Session;  VAR (*IN*) args: ARRAY OF CHAR): BOOLEAN;

    BEGIN
        args[0] := args[0];      (* to suppress a compiler warning *)
        IF session^.state = Selected THEN
            StatusUpdate (session^.userdata);
        END (*IF*);
        RETURN TRUE;
    END NOOP;

(********************************************************************************)

PROCEDURE RENAME (session: Session;  VAR (*IN*) args: ARRAY OF CHAR): BOOLEAN;

    VAR oldname, newname: FilenameString;
        targetexists: BOOLEAN;

    BEGIN
        IF AString (args, oldname) AND SPACE(args)
                   AND AString (args, newname) THEN
            IF RenameBox (session^.userdata, oldname, newname, targetexists) THEN
                RETURN TRUE;
            ELSIF targetexists THEN
                ReplyWithTag (session, "NO Destination folder already exists");
                RETURN FALSE;
            ELSE
                ReplyWithTag (session, "NO RENAME operation failed");
                RETURN FALSE;
            END (*IF*);
        ELSE
            ReplyWithTag (session, "BAD Two mailbox names are required");
            RETURN FALSE;
        END (*IF*);
    END RENAME;

(********************************************************************************)

PROCEDURE SEARCH (session: Session;  VAR (*IN*) args: ARRAY OF CHAR): BOOLEAN;

    VAR CharsetName: FilenameString;
        OK: BOOLEAN;

    BEGIN
        OK := TRUE;
        IF Match (args, "CHARSET") THEN
            IF SPACE (args) AND AString (args, CharsetName) THEN
                IF NOT StringMatch (CharsetName, "US-ASCII") THEN
                    ReplyWithTag (session, "NO That Charset is not supported");
                    OK := FALSE;
                END (*IF*);
            ELSE
                ReplyWithTag (session, "BAD No Charset specified");
                OK := FALSE;
            END (*IF*);
        END (*IF*);

        IF OK THEN
            SearchBox (session^.userdata, args, FALSE);
        END (*IF*);

        RETURN OK;

    END SEARCH;

(********************************************************************************)

PROCEDURE SELECT (session: Session;  VAR (*IN*) args: ARRAY OF CHAR): BOOLEAN;

    VAR boxname: FilenameString;

    BEGIN
        IF AString (args, boxname) THEN
            IF SelectBox (session^.userdata, boxname, FALSE) THEN
                session^.state := Selected;
                ReplyWithTag (session, "OK [READ-WRITE] SELECT completed");
            ELSE
                session^.state := Authenticated;
                ReplyWithTag (session, "NO SELECT failed");
            END (*IF*);
        ELSE
            ReplyWithTag (session, "BAD No valid mailbox specified");
        END (*IF*);
        RETURN FALSE;
    END SELECT;

(********************************************************************************)

PROCEDURE STATUS (session: Session;  VAR (*IN*) args: ARRAY OF CHAR): BOOLEAN;

    VAR boxname: FilenameString;

    BEGIN
        IF AString (args, boxname) AND SPACE(args) THEN
            IF GetStatus (session^.userdata, boxname, args) THEN
                RETURN TRUE;
            ELSE
                ReplyWithTag (session, "NO STATUS operation failed");
                RETURN FALSE;
            END (*IF*);
        ELSE
            ReplyWithTag (session, "BAD Syntax error in command");
            RETURN FALSE;
        END (*IF*);
    END STATUS;

(********************************************************************************)

PROCEDURE STORE (session: Session;  VAR (*IN*) args: ARRAY OF CHAR): BOOLEAN;

    BEGIN
        IF DoStore (session^.userdata, args, FALSE, session^.ID) THEN
            RETURN TRUE;
        ELSE
            ReplyWithTag (session, "NO STORE failed");
            RETURN FALSE;
        END (*IF*);
    END STORE;

(********************************************************************************)

PROCEDURE SUBSCRIBE (session: Session;  VAR (*IN*) args: ARRAY OF CHAR): BOOLEAN;

    VAR boxname: FilenameString;

    BEGIN
        IF AString (args, boxname) THEN
            IF SubscribeToBox (session^.userdata, boxname) THEN
                RETURN TRUE;
            ELSE
                ReplyWithTag (session, "NO SUBSCRIBE operation failed");
                RETURN FALSE;
            END (*IF*);
        ELSE
            ReplyWithTag (session, "BAD No valid mailbox specified");
            RETURN FALSE;
        END (*IF*);
    END SUBSCRIBE;

(********************************************************************************)

PROCEDURE UID (session: Session;  VAR (*IN*) args: ARRAY OF CHAR): BOOLEAN;

    VAR set: MessageSet;  box, CharsetName: FilenameString;  OK: BOOLEAN;

    BEGIN
        IF Match (args, "COPY ") THEN
            set := GetSet (args);
            IF NonEmptySet(set) AND AString (args, box) THEN
                IF SecondarySelectBox (session^.userdata, box) THEN
                    IF DoCopy (session^.userdata, set, TRUE) THEN
                        ReplyWithTag (session, "OK UID COPY completed");
                    ELSE
                        ReplyWithTag (session, "NO COPY failed");
                    END (*IF*);
                ELSE
                    ReplyWithTag (session, "NO [TRYCREATE] invalid destination");
                END (*IF*);
            ELSE
                ReplyWithTag (session, "BAD no valid destination specified");
            END (*IF*);
        ELSIF Match (args, "FETCH ") THEN
            IF DoFetch (session^.userdata, args, TRUE, session^.ID) THEN
                ReplyWithTag  (session, "OK UID FETCH completed");
            ELSE
                ReplyWithTag  (session, "NO unimplemented option");
            END (*IF*);
        ELSIF Match (args, "STORE ") THEN
            IF DoStore (session^.userdata, args, TRUE, session^.ID) THEN
                ReplyWithTag  (session, "OK UID STORE completed");
            ELSE
                ReplyWithTag (session, "NO STORE failed");
            END (*IF*);
        ELSIF Match (args, "SEARCH ") THEN
            OK := TRUE;
            IF Match (args, "CHARSET") THEN
                IF SPACE (args) AND AString (args, CharsetName) THEN
                    IF NOT StringMatch (CharsetName, "US-ASCII") THEN
                        ReplyWithTag (session, "NO That Charset is not supported");
                        OK := FALSE;
                    END (*IF*);
                ELSE
                    ReplyWithTag (session, "BAD No Charset specified");
                    OK := FALSE;
                END (*IF*);
            END (*IF*);

            IF OK THEN
                SearchBox (session^.userdata, args, FALSE);
                ReplyWithTag  (session, "OK UID SEARCH completed");
            END (*IF*);

        ELSE
            ReplyWithTag (session, "BAD Invalid command in UID command");
        END (*IF*);

        RETURN FALSE;
    END UID;

(********************************************************************************)

PROCEDURE UNSUBSCRIBE (session: Session;  VAR (*IN*) args: ARRAY OF CHAR): BOOLEAN;

    VAR boxname: FilenameString;

    BEGIN
        IF AString (args, boxname) THEN
            IF UnsubscribeFromBox (session^.userdata, boxname) THEN
                RETURN TRUE;
            ELSE
                ReplyWithTag (session, "NO UNSUBSCRIBE operation failed");
                RETURN FALSE;
            END (*IF*);
        ELSE
            ReplyWithTag (session, "BAD No valid mailbox specified");
            RETURN FALSE;
        END (*IF*);
    END UNSUBSCRIBE;

(********************************************************************************)
(*                          SEARCHING THE COMMAND TABLE                         *)
(********************************************************************************)

PROCEDURE FindCommand (VAR (*IN*) cmd: ARRAY OF CHAR;
                       VAR (*OUT*) place: CommandTableIndex): BOOLEAN;

    (* Sets place to the position in the command table where cmd is (if         *)
    (* present) or where it should be inserted (if not present).  The function  *)
    (* returns TRUE iff cmd was found in the table.                             *)

    (****************************************************************************)

    PROCEDURE Compare (N: CommandTableIndex): INTEGER;

        (* Compares Command with KeywordList[N].          *)
        (* Returns >0 if Command > Table[N], and so on.   *)

        VAR ch1, ch2: CHAR;  j, k1, k2: CARDINAL;

        BEGIN
            k1 := 0;  k2 := CommandTable[N].strstart;  j := 0;
            LOOP
                IF (k1 > HIGH(cmd)) OR (cmd[k1] = ' ')
                                       OR (cmd[k1] = Nul) THEN
                    IF j = CommandTable[N].strlen THEN
                        RETURN 0;
                    ELSE
                        RETURN -1;
                    END (*IF*);
                ELSIF j = CommandTable[N].strlen THEN
                    RETURN +1;
                END (*IF*);
                ch1 := CAP(cmd[k1]);  ch2 := StringTable[k2+j];
                IF ch1 > ch2 THEN RETURN +1
                ELSIF ch1 < ch2 THEN RETURN -1
                END (*IF*);
                INC (k1);  INC (j);
            END (*LOOP*);
        END Compare;

    (****************************************************************************)

    VAR first, last: CommandTableIndex;  Match: BOOLEAN;
        test: INTEGER;

    BEGIN
        IF NoOfCommands = 0 THEN
            place := 0;  RETURN FALSE;
        END (*IF*);

        (* In this version I'm using a binary search.                     *)

        first := 0;  last := NoOfCommands - 1;  Match := FALSE;
        LOOP
            place := (first + last) DIV 2;
            test := Compare (place);
            IF test < 0 THEN
                IF place = 0 THEN
                    EXIT (*LOOP*);
                ELSE
                    last := place - 1;
                END (*IF*);
            ELSIF test = 0 THEN
                Match := TRUE;  EXIT (*LOOP*);
            ELSIF place = MAX(CommandTableIndex) THEN
                EXIT (*LOOP*);
            ELSE
                INC(place);  first := place;
            END (*IF*);
            IF first > last THEN EXIT (*LOOP*) END (*IF*);
        END (*LOOP*);
        RETURN Match;
    END FindCommand;

(********************************************************************************)
(*                       THE MAIN COMMAND DISPATCHER                            *)
(********************************************************************************)

PROCEDURE HandleCommand (S: Session;  VAR (*INOUT*) CmdLine: ARRAY OF CHAR;
                                                     VAR (*OUT*) Quit: BOOLEAN);

    (* Executes one user command.  Returns with Quit=TRUE if the command is one *)
    (* that closes the session, or if the connection is lost.                   *)

    VAR j, pos: CARDINAL;
        Handler: HandlerProc;
        N: CommandTableIndex;
        found, ExplicitLogout: BOOLEAN;
        OriginalCommand: ARRAY [0..1023] OF CHAR;
        CmdWord: ARRAY [0..12] OF CHAR;  (* longest command is AUTHENTICATE *)

    BEGIN
        Strings.Assign (CmdLine, OriginalCommand);

        (* Skip leading spaces. *)

        pos := 0;
        WHILE CmdLine[pos] = ' ' DO
            INC (pos);
        END (*WHILE*);

        (* Extract the tag. *)

        j := 0;
        LOOP
            IF (pos > MAX(ArgStringIndex)) OR (CmdLine[pos] = ' ')
                                     OR (CmdLine[pos] = Nul) THEN
                EXIT (*LOOP*);
            END (*IF*);
            IF j >= MaxTagChars THEN
                EXIT (*LOOP*);
            END (*IF*);
            S^.Tag[j] := CmdLine[pos];
            INC (j);  INC(pos);
        END (*LOOP*);

        IF j < MaxTagChars THEN
            S^.Tag[j] := Nul;
        END (*IF*);

        (* Delete the tag, and separate the command and arguments. *)

        IF CmdLine[pos] = ' ' THEN
            INC(pos);
        END (*IF*);
        IF pos > 0 THEN
            Strings.Delete (CmdLine, 0, pos);
        END (*IF*);
        Strings.FindNext (' ', CmdLine, 0, found, j);
        IF found THEN
            Strings.Extract (CmdLine, 0, j, CmdWord);
            REPEAT
                INC (j);
            UNTIL CmdLine[j] <> ' ';
            Strings.Delete (CmdLine, 0, j);
        ELSE
            Strings.Assign (CmdLine, CmdWord);
            CmdLine[0] := Nul;
        END (*IF*);

        (* Look up the table of commands, also log the command. *)

        Handler := NoSuchCommand;
        found := FindCommand (CmdWord, N);
        IF found THEN
            Handler := CommandTable[N].CmdHandler;
        END (*IF*);
        IF found AND (Handler = LOGIN) THEN
            LogTransactionL (S^.ID, "LOGIN ******");
        ELSE
            LogTransaction (S^.ID, OriginalCommand);
        END (*IF*);

        IF found THEN

            (* Check whether we're in the right state for this command. *)

            CASE CommandTable[N].StateNeeded OF
               | MustExit:
                      (* All commands legal in this case *)
               | nonAuthenticated:
                      IF S^.state <> nonAuthenticated THEN
                          Handler := AlreadyLoggedIn;
                      END (*IF*);
               | Authenticated:
                      IF S^.state < Authenticated THEN
                          Handler := NotLoggedIn;
                      END (*IF*);
               | Selected:
                      IF S^.state <> Selected THEN
                          Handler := NoMailboxSelected;
                      END (*IF*);
            END (*CASE*);

        END (*IF*);

        ExplicitLogout := Handler = LOGOUT;

        (* Call the handler. *);

        IF Handler <> NullCommand THEN
            IF Handler (S, CmdLine) THEN
                Reply3WithTag (S, "OK ", CmdWord, " completed");
            END (*IF*);
        END (*IF*);
        FlushOutput (S^.SB);

        IF (S^.state = MustExit) AND NOT ExplicitLogout THEN
            LogTransactionL (S^.ID, "Connection lost");
        END (*IF*);
        Quit := S^.state = MustExit;

    END HandleCommand;

(********************************************************************************)
(*                               INITIALISATION                                 *)
(********************************************************************************)

PROCEDURE MakePosition (CommandString: ARRAY OF CHAR): CommandTableIndex;

    (* Finds a suitable position in the command table for the CommandString,    *)
    (* creates the hole in the table.                                           *)

    VAR j, place: CommandTableIndex;

    BEGIN
        EVAL (FindCommand (CommandString, place));
        IF place < NoOfCommands THEN
            FOR j := NoOfCommands TO place+1 BY -1 DO
                CommandTable[j] := CommandTable[j-1];
            END (*FOR*);
        END (*IF*);
        INC (NoOfCommands);
        RETURN place;
    END MakePosition;

(********************************************************************************)

PROCEDURE AddCommand (CommandString: ARRAY OF CHAR;  Handler: HandlerProc;
                                                     minState: ClientState);

    (* Adds an entry to our table of commands. *)

    VAR pos: CommandTableIndex;  j, length: CARDINAL;

    BEGIN
        pos := MakePosition (CommandString);
        length := LENGTH (CommandString);
        WITH CommandTable[pos] DO
            strstart := NextString;
            strlen := length;
            CmdHandler := Handler;
            StateNeeded := minState;
        END (*WITH*);
        IF length > 0 THEN
            FOR j := 0 TO length-1 DO
                StringTable[NextString+j] := CommandString[j];
            END (*FOR*);
        END (*IF*);
        INC (NextString, length);
    END AddCommand;

(********************************************************************************)

PROCEDURE FillCommandTable;

    (* Creates the table of commands. *)

    BEGIN
        NextString := 0;
        NoOfCommands := 0;

        AddCommand ('', NullCommand, MustExit);
        AddCommand ('CAPABILITY', CAPABILITY, MustExit);
        AddCommand ('NOOP', NOOP, MustExit);
        AddCommand ('LOGOUT', LOGOUT, MustExit);
        AddCommand ('NAMESPACE', NAMESPACE, MustExit);

        AddCommand ('AUTHENTICATE', AUTHENTICATE, nonAuthenticated);
        AddCommand ('LOGIN', LOGIN, nonAuthenticated);

        AddCommand ('SELECT', SELECT, Authenticated);
        AddCommand ('EXAMINE', EXAMINE, Authenticated);
        AddCommand ('CREATE', CREATE, Authenticated);
        AddCommand ('DELETE', DELETE, Authenticated);
        AddCommand ('RENAME', RENAME, Authenticated);
        AddCommand ('SUBSCRIBE', SUBSCRIBE, Authenticated);
        AddCommand ('UNSUBSCRIBE', UNSUBSCRIBE, Authenticated);
        AddCommand ('LIST', LIST, Authenticated);
        AddCommand ('LSUB', LSUB, Authenticated);
        AddCommand ('STATUS', STATUS, Authenticated);
        AddCommand ('APPEND', APPEND, Authenticated);

        AddCommand ('CHECK', CHECK, Selected);
        AddCommand ('CLOSE', CLOSE, Selected);
        AddCommand ('EXPUNGE', EXPUNGE, Selected);
        AddCommand ('SEARCH', SEARCH, Selected);
        AddCommand ('FETCH', FETCH, Selected);
        AddCommand ('STORE', STORE, Selected);
        AddCommand ('COPY', COPY, Selected);
        AddCommand ('UID', UID, Selected);

    END FillCommandTable;

(********************************************************************************)

BEGIN
    FillCommandTable;
END IMCommands.

