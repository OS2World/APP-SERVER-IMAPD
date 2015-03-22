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

IMPLEMENTATION MODULE IMSession;

        (********************************************************)
        (*                                                      *)
        (*           Session handler for the IMAP4 server       *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            1 March 2001                    *)
        (*  Last edited:        2 January 2014                  *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (*       The StopAllUsers procedure currently does      *)
        (*       nothing, because we don't have a global        *)
        (*       list of all users.                             *)
        (*                                                      *)
        (********************************************************)

IMPORT Strings, OS2, IMCommands;

FROM LogCtx IMPORT
    (* var  *)  WCtx;

FROM SYSTEM IMPORT
    (* type *)  ADDRESS;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

FROM LowLevel IMPORT
    (* proc *)  EVAL;

FROM Conversions IMPORT
    (* proc *)  CardinalToString;

FROM SBuffers IMPORT
    (* type *)  SBuffer,
    (* proc *)  CreateSBuffer, CloseSBuffer, SetTimeout, Getch, SendLineL;

FROM Sockets IMPORT
    (* type *)  Socket, SockAddr,
    (* proc *)  send, getsockname, soclose, so_cancel;

FROM Inet2Misc IMPORT
    (* proc *)  LockScreen, UnlockScreen, IPToString, AddEOL, AddressToHostName,
                ConvertCard, Synch;

FROM Names IMPORT
    (* type *)  HostName, ArgStringIndex;

FROM SplitScreen IMPORT
    (* proc *)  NotDetached, WriteStringAt;

FROM Semaphores IMPORT
    (* type *)  Semaphore,
    (* proc *)  CreateSemaphore, DestroySemaphore, Signal;

FROM Timer IMPORT
    (* proc *)  TimedWait, Sleep;

FROM TaskControl IMPORT
    (* type *)  Lock,
    (* proc *)  CreateLock, Obtain, Release, CreateTask1, TaskExit;

FROM TransLog IMPORT
    (* type *)  TransactionLogID,
    (* proc *)  CreateLogID, DiscardLogID, LogTransaction, LogTransactionL;

(************************************************************************)

CONST
    Nul = CHR(0);

TYPE
    (* Session state record. *)

    Session = RECORD
                  LogID: TransactionLogID;
                  IPAddress: CARDINAL;     (* client - network byte order *)
                  SS: IMCommands.Session;
              END (*RECORD*);

    (* Data used in creating a new instance of the session handler task. *)

    NewSessionPointer = POINTER TO
                           RECORD
                               socket: Socket;
                               IPAddress: CARDINAL;  (* network byte order *)
                           END (*RECORD*);

    (* Data needed by the timeout checker task. *)

    KeepAlivePointer = POINTER TO
                           RECORD
                               SocketOpen, dying: BOOLEAN;
                               sem: Semaphore;
                               socket: Socket;
                               TimedOut: BOOLEAN;
                           END (*RECORD*);


VAR
    (* Maximum allowed number of simultaneous users. *)

    MaxUsers: CARDINAL;

    (* Count of active users, and a lock to protect it. *)

    UserCount: CARDINAL;
    UserCountLock: Lock;

    (* Timeout delay, in seconds. *)

    MaxTime: CARDINAL;

    (* Flag to say whether we're showing the number of users on screen. *)

    LogToScreen: BOOLEAN;

(************************************************************************)

PROCEDURE SetSessionParameters (MaxUserLimit, TimeoutLimit: CARDINAL);

    (* Sets some parameters specified by the INI file: maximum nunber   *)
    (* of simultaneous users, and how long a session can be idle before *)
    (* it is forcibly closed.                                           *)

    BEGIN
        MaxUsers := MaxUserLimit;
        IF TimeoutLimit > MAX(CARDINAL) DIV 1000 THEN
            MaxTime := MAX(CARDINAL) DIV 1000;
        ELSE
            MaxTime := TimeoutLimit;
        END (*IF*);
    END SetSessionParameters;

(************************************************************************)

PROCEDURE UpdateCount (increment: INTEGER): CARDINAL;

    (* Updates the count of the number of users, and returns the new    *)
    (* count.  Special case: if this would take us beyond the MaxUsers  *)
    (* limit, then the count is not updated and the returned value      *)
    (* is zero.                                                         *)

    VAR value, pos: CARDINAL;  Buffer: ARRAY [0..15] OF CHAR;

    BEGIN
        Obtain (UserCountLock);
        IF increment > 0 THEN INC (UserCount, increment);
        ELSIF increment < 0 THEN DEC (UserCount, -increment)
        END (*IF*);
        value := UserCount;
        IF value > MaxUsers THEN
            DEC (UserCount, increment);  value := 0;
        ELSIF LogToScreen THEN
            pos := 0;
            ConvertCard (value, Buffer, pos);
            Buffer[pos] := ' ';  INC(pos);
            Buffer[pos] := CHR(0);
            WriteStringAt (0, 72, Buffer);
        END (*IF*);
        Release (UserCountLock);
        RETURN value;
    END UpdateCount;

(************************************************************************)

PROCEDURE NumberOfUsers(): CARDINAL;

    (* Returns the number of users who are currently logged on. *)

    BEGIN
        RETURN UpdateCount (0);
    END NumberOfUsers;

(************************************************************************)

PROCEDURE StopAllUsers;

    (* Attempts to close down all sessions that are still open. *)

    (* CURRENTLY DOES NOTHING, because we have no global record of all  *)
    (* open sessions.                                                   *)

    BEGIN
    END StopAllUsers;

(************************************************************************)

PROCEDURE TimeoutChecker (arg: ADDRESS);

    (* A new instance of this task is created for each client session.  *)
    (* It kills the corresponding SessionHandler task if more than      *)
    (* MaxTime seconds have passed since the last Signal() on the       *)
    (* session's KeepAlive semaphore.                                   *)

    VAR p: KeepAlivePointer;

    BEGIN
        p := arg;
        REPEAT
            TimedWait (p^.sem, 1000*MaxTime, p^.TimedOut);
        UNTIL p^.TimedOut OR p^.dying;
        IF p^.SocketOpen THEN
            so_cancel (p^.socket);
        END (*IF*);

        (* Wait for the socket to be closed. *)

        WHILE p^.SocketOpen DO
            Sleep (500);
        END (*WHILE*);
        DestroySemaphore (p^.sem);
        DISPOSE (p);

    END TimeoutChecker;

(************************************************************************)

PROCEDURE CloseSession (session: Session);

    (* End-of-session tidying up. *)

    BEGIN
        WITH session DO
            IMCommands.CloseSession (SS);
        END (*WITH*);
    END CloseSession;

(************************************************************************)

PROCEDURE SessionHandler (arg: ADDRESS);

    (* The task that handles a client session, i.e. this is where all the real  *)
    (* work is done.  There might be several instances of this task running,    *)
    (* one for each session that is still open.                                 *)

    CONST CR = CHR(13);  LF = CHR(10);

    VAR S: Socket;
        SB: SBuffer;
        sess: Session;
        CmdBuffer: ARRAY [0..4095] OF CHAR;

    (********************************************************************)

    PROCEDURE BuildCommand(): BOOLEAN;

        VAR length: CARDINAL;  ch: CHAR;
            IACreceived, IgnoreNext: BOOLEAN;

        BEGIN
            length := 0;
            IACreceived := FALSE;  IgnoreNext := FALSE;
            LOOP
                ch := Getch(SB);
                IF ch = Nul THEN RETURN FALSE END(*IF*);

                (* This next section skips over Telnet control codes (which we  *)
                (* don't really want to know about).  A Telnet control code is  *)
                (* two or three bytes long, where the first byte is CHR(255).   *)

                IF IgnoreNext THEN
                    IgnoreNext := FALSE;
                ELSIF IACreceived THEN
                    IACreceived := FALSE;
                    IF ORD(ch) = 255 THEN
                        IF length <= MAX(ArgStringIndex) THEN
                            CmdBuffer[length] := ch;  INC(length);
                        END (*IF*);
                    ELSIF ORD(ch) > 250 THEN
                        IgnoreNext := TRUE;
                    END (*IF*);
                ELSIF ORD(ch) = 255 THEN
                    IACreceived := TRUE;

                (* Command should end with CR LF, but for simplicity we'll      *)
                (* ignore the CR.                                               *)

                ELSIF ch = CR THEN  (* Do nothing *)
                ELSIF ch = LF THEN
                    IF length <= MAX(ArgStringIndex) THEN
                        CmdBuffer[length] := CHR(0);
                    END (*IF*);
                    RETURN TRUE;
                ELSIF length <= MAX(ArgStringIndex) THEN
                    CmdBuffer[length] := ch;  INC(length);
                END (*IF*);

            END (*LOOP*);

        END BuildCommand;

    (********************************************************************)

    VAR NSP: NewSessionPointer;
        size, UserNumber: CARDINAL;
        KA: KeepAlivePointer;
        KeepAliveSemaphore: Semaphore;
        Quit: BOOLEAN;
        LogFilePrefix: ARRAY [0..6] OF CHAR;
        IPBuffer: ARRAY [0..16] OF CHAR;
        LogMessage: ARRAY [0..127] OF CHAR;
        OurHostName: HostName;
        ServerName: SockAddr;

    BEGIN                   (* Body of SessionHandler *)

        OS2.DosError (OS2.FERR_DISABLEHARDERR);              (* disable hard error popups *)

        (* Copy the NewSessionPointer^ information. *)

        NSP := arg;
        S := NSP^.socket;
        sess.IPAddress := NSP^.IPAddress;
        DISPOSE (NSP);

        (* Create the log file ID for this session. *)

        CardinalToString (S, LogFilePrefix, 7);
        sess.LogID := CreateLogID (WCtx, LogFilePrefix);

        (* Log the new session commencement. *)

        Strings.Assign ("New client ", LogMessage);
        IPToString (sess.IPAddress, TRUE, IPBuffer);
        Strings.Append (IPBuffer, LogMessage);
        LogTransaction (sess.LogID, LogMessage);

        (* Work out our host name. *)

        size := SIZE(SockAddr);
        getsockname (S, ServerName, size);
        AddressToHostName (ServerName.in_addr.addr, OurHostName);

        (* Check for too many users. *)

        UserNumber := UpdateCount (+1);
        IF UserNumber = 0 THEN
            LogTransactionL (sess.LogID, "Too many users");
            Strings.Assign ("* BYE Too many users, try again later", CmdBuffer);
            size := AddEOL (CmdBuffer);
            EVAL (send (S, CmdBuffer, size, 0));
            EVAL (soclose (S));
            DiscardLogID (sess.LogID);
            TaskExit;
        END (*IF*);

        (* Create the session information structure. *)

        CreateSemaphore (KeepAliveSemaphore, 0);
        SB := CreateSBuffer (S, TRUE);
        SetTimeout (SB, MaxTime);
        sess.SS := IMCommands.OpenSession (SB, ServerName.in_addr.addr, 0,
                                         KeepAliveSemaphore, sess.LogID);

        (* Create an instance of the TimeoutChecker task. *)

        NEW (KA);
        WITH KA^ DO
            SocketOpen := TRUE;  socket := S;  dying := FALSE;
            sem := KeepAliveSemaphore;
            TimedOut := FALSE;
        END (*WITH*);

        (* Start watchdog, send the "welcome" message. *)

        Quit := NOT (CreateTask1 (TimeoutChecker, 3, "IMAP4 timeout", KA)
                              AND SendLineL (SB, "* OK IMAP4rev1 ready"));

        (* Here's the main command processing loop.  We leave it when the client  *)
        (* issues a QUIT command, or when socket communications are lost, or      *)
        (* when we get a timeout on the KeepAlive semaphore.                      *)

        LOOP
            IF Quit THEN EXIT(*LOOP*) END(*IF*);
            IF BuildCommand() THEN
                Signal (KeepAliveSemaphore);
                IMCommands.HandleCommand (sess.SS, CmdBuffer, Quit);
            ELSE
                EXIT (*LOOP*);
            END (*IF*);
        END (*LOOP*);

        (* Work out whether the session was terminated by a QUIT, or a timeout, *)
        (* or a communications failure.                                         *)

        IF KA^.TimedOut THEN
            EVAL(SendLineL (SB, "* BYE Autologout; idle for too long"));
            LogTransactionL (sess.LogID, "Timed out");
        ELSIF NOT Quit THEN
            LogTransactionL (sess.LogID, "Session aborted by client");
        END (*IF*);

        CloseSession (sess);
        KA^.dying := TRUE;  Signal (KA^.sem);
        CloseSBuffer (SB);
        KA^.SocketOpen := FALSE;

        (* The watchdog will now destroy the semaphore and discard KA. *)

        EVAL (UpdateCount (-1));
        DiscardLogID (sess.LogID);
        TaskExit;

    (*
    EXCEPT
        LogTransaction (sess.LogID, "SessionHandler detected exception.");
        CloseSession (sess);
        UserNumber := UpdateCount (sess.service, -1);
        soclose(S);
        TaskExit;
    *)

    END SessionHandler;

(********************************************************************************)

PROCEDURE NewSession (S: Socket;  addr: SockAddr): BOOLEAN;

    (* Starts and runs a client session.  The session runs in a separate        *)
    (* thread; this procedure returns after starting the session, it does not   *)
    (* wait until the session is over.                                          *)

    VAR NSP: NewSessionPointer;  success: BOOLEAN;

    BEGIN
        NEW (NSP);
        WITH NSP^ DO
            socket := S;  IPAddress := addr.in_addr.addr;
        END (*WITH*);
        success := CreateTask1 (SessionHandler, 3, "IMAP4 session", NSP);
        IF NOT success THEN
            EVAL (soclose(S));
            DISPOSE (NSP);
        END (*IF*);
        RETURN success;
    END NewSession;

(********************************************************************************)
(*                            MODULE INITIALISATION                             *)
(********************************************************************************)

BEGIN
    LogToScreen := NotDetached();
    CreateLock (UserCountLock);
    Obtain (UserCountLock);
    UserCount := 0;
    Release (UserCountLock);
END IMSession.

