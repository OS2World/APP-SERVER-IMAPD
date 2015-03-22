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

MODULE IMAPD;

        (********************************************************)
        (*                                                      *)
        (*                    IMAP4 server                      *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            1 March 2001                    *)
        (*  Last edited:        2 October 2014                  *)
        (*  Status:             This module OK                  *)
        (*                                                      *)
        (********************************************************)

IMPORT IMV, TextIO, Strings, FileSys, OS2;

FROM SYSTEM IMPORT LOC, CARD8;

(*
FROM MailAccounts IMPORT                (* while debugging *)
    (* proc *)  StartDebugLogging;
*)

FROM IOChan IMPORT
    (* type *)  ChanId;

FROM Sockets IMPORT
    (* const*)  NotASocket,
    (* type *)  Socket, SockAddr, AddressFamily, SocketType,
    (* proc *)  sock_init, socket, so_cancel, setsockopt,
                bind, listen, select, accept, soclose, psock_errno,
                getsockname, getpeername, sock_errno;

FROM Internet IMPORT
    (* const*)  Zero8, INADDR_ANY;

FROM LowLevel IMPORT
    (* proc *)  EVAL;

FROM Timer IMPORT
    (* proc *)  Sleep;

FROM Semaphores IMPORT
    (* type *)  Semaphore,
    (* proc *)  CreateSemaphore, Wait, Signal;

FROM TaskControl IMPORT
    (* proc *)  CreateTask;

FROM SplitScreen IMPORT
    (* proc *)  NotDetached, SetBoundary, ClearScreen,
                WriteStringAt, WriteString, WriteCard, WriteLn;

FROM IMAPMisc IMPORT
    (* proc *)  SetTNImode, OpenOurINIFile;

FROM INIData IMPORT
    (* type *)  HINI,
    (* proc *)  INIValid, CloseINIFile, INIGet, INIGetString;

FROM Inet2Misc IMPORT
    (* proc *)  Swap2, ConvertCard;

FROM LogCtx IMPORT
    (* var  *)  WCtx;

FROM BoxLists IMPORT
    (* proc *)  InitUIDValidity;

FROM TransLog IMPORT
    (* type *)  TransactionLogID,
    (* proc *)  StartTransactionLogging, CreateLogID, LogTransaction, LogTransactionL;

FROM Domains IMPORT
    (* proc *)  CheckRegistration, RefreshMasterDomainList;

FROM CtrlC IMPORT
    (* proc *)  SetBreakHandler;

FROM IMSession IMPORT
    (* proc *)  SetSessionParameters, NewSession, NumberOfUsers, StopAllUsers;

FROM Names IMPORT
    (* type *)  FilenameString;

FROM ProgramArgs IMPORT
    (* proc *)  ArgChan, IsArgPresent;

(************************************************************************)

TYPE
    ThreeCard = ARRAY [0..2] OF CARDINAL;
    FourCard = ARRAY [0..3] OF CARDINAL;

CONST
    DefaultPort = 143;
    DefaultMaxUsers = 10;
    DefaultTimeout = 30*60;               (* seconds   *)

VAR
    IMAPEnabled: BOOLEAN;
    CalledFromInetd: BOOLEAN;
    ScreenEnabled: BOOLEAN;
    MainSocket: Socket;
    ServerPort: CARDINAL;

    (* Flag to specify use of TNI file. *)

    UseTNI: BOOLEAN;

    (* Variables needed for shutdown processing. *)

    ShutdownSignal: OS2.HEV;
    ShutdownRequest: Semaphore;
    TaskDone: Semaphore;
    ShutdownInProgress, RapidShutdown: BOOLEAN;

(************************************************************************)

PROCEDURE ShutdownChecker;

    (* A separate task that waits for a shutdown request.  *)

    VAR StillRunning: BOOLEAN;

    BEGIN
        StillRunning := TRUE;
        LOOP
            Wait (ShutdownRequest);
            RapidShutdown := ShutdownInProgress;
            ShutdownInProgress := TRUE;
            IF StillRunning THEN
                IF MainSocket <> NotASocket THEN
                    so_cancel (MainSocket);
                END (*IF*);
                StillRunning := FALSE;
            END (*IF*);
            IF RapidShutdown THEN
                EXIT (*LOOP*);
            END (*IF*);
        END (*LOOP*);
    END ShutdownChecker;

(************************************************************************)

PROCEDURE ["C"] ControlCHandler(): BOOLEAN;

    (* Intercepts a Ctrl/C from the keyboard. *)

    BEGIN
        Signal (ShutdownRequest);
        RETURN TRUE;
    END ControlCHandler;

(************************************************************************)

PROCEDURE LoadINIData;

    (* Loads setup parameters from "weasel.ini".  Also starts   *)
    (* transaction logging if it is enabled.                    *)

    VAR hini: HINI;
        SYSapp: ARRAY [0..4] OF CHAR;

    PROCEDURE GetItem (name: ARRAY OF CHAR;
                            VAR (*OUT*) variable: ARRAY OF LOC): BOOLEAN;

        BEGIN
            RETURN INIGet (hini, SYSapp, name, variable);
        END GetItem;

    (********************************************************************)

    PROCEDURE LoadThird (key: ARRAY OF CHAR;  default: CARDINAL): CARDINAL;

        (* Loads dialogue element (hwnd, field) from the third element  *)
        (* of a three-cardinal or four-cardinal value from the INI file,*)
        (* or with the default value if the INI value is not found.     *)

        VAR fourval: FourCard;  threeval: ThreeCard;  result: CARDINAL;

        BEGIN
            IF GetItem (key, fourval) THEN
                result := fourval[2];
            ELSIF GetItem (key, threeval) THEN
                result := threeval[2];
            ELSE
                result := default;
            END (*IF*);
            RETURN result;
        END LoadThird;

    (********************************************************************)

    VAR TimeoutLimit, MaxUsers, temp: CARDINAL;
        LogLevel: CARDINAL;
        LogFile: FilenameString;

    BEGIN
        SYSapp := "$SYS";
        LogLevel := 0;
        MaxUsers := DefaultMaxUsers;
        TimeoutLimit := DefaultTimeout;
        hini := OpenOurINIFile();
        IF INIValid(hini) THEN
            ServerPort := LoadThird ("ServerPort", DefaultPort);
            MaxUsers := LoadThird ("MaxUsers", DefaultMaxUsers);
            TimeoutLimit := LoadThird ("TimeOut", DefaultTimeout);
            IMAPEnabled := GetItem ('Enable', temp) AND (temp >= 4);
            EVAL(GetItem ('IMAPTransLevel', LogLevel));
            EVAL(INIGetString(hini, SYSapp, 'IMAPLogFileName', LogFile));
            CloseINIFile (hini);
        END (*IF*);

        SetSessionParameters (MaxUsers, TimeoutLimit);
        IF LogLevel > 0 THEN
            StartTransactionLogging (WCtx, LogFile, LogLevel);
            (*StartDebugLogging (WCtx);*)
        END (*IF*);

    END LoadINIData;

(************************************************************************)

PROCEDURE GetParameters (VAR (*OUT*) result: CARDINAL): BOOLEAN;

    (* Picks up optional program arguments from the command line.  If a numeric *)
    (* argument is present, returns TRUE and returns its value in result.       *)

    TYPE CharSet = SET OF CHAR;
    CONST Digits = CharSet {'0'..'9'};  Nul = CHR(0);

    VAR j: CARDINAL;  ch: CHAR;
        args: ChanId;
        ParameterString: ARRAY [0..79] OF CHAR;
        NumberPresent: BOOLEAN;

    BEGIN
        UseTNI := FALSE;
        NumberPresent := FALSE;
        args := ArgChan();
        IF IsArgPresent() THEN
            TextIO.ReadString (args, ParameterString);
            j := 0;
            LOOP
                ch := ParameterString[j];  INC(j);
                IF ch = Nul THEN
                    EXIT (*LOOP*);
                ELSIF CAP(ch) = '-' THEN
                    (* ignore it *)
                ELSIF CAP(ch) = 'T' THEN
                    UseTNI := TRUE;
                ELSIF ch IN Digits THEN
                    NumberPresent := TRUE;
                    result := 0;
                    REPEAT
                        result := 10*result + ORD(ch) - ORD('0');
                        ch := ParameterString[j];  INC(j);
                    UNTIL NOT (ch IN Digits);
                    DEC (j);
                ELSIF ch <> ' ' THEN
                    EXIT (*LOOP*);
                END (*IF*);
            END (*LOOP*);
        END (*IF*);
        SetTNImode (UseTNI);
        RETURN NumberPresent;
    END GetParameters;

(************************************************************************)

PROCEDURE AppendCard (number: CARDINAL;  VAR (*INOUT*) result: ARRAY OF CHAR);

    (* Converts number to decimal string, appends it to result. *)

    VAR pos: CARDINAL;

    BEGIN
        pos := Strings.Length (result);
        ConvertCard (number, result, pos);
        result[pos] := CHR(0);
    END AppendCard;

(************************************************************************)

PROCEDURE WriteError (LogID: TransactionLogID);

    VAR LogLine: ARRAY [0..255] OF CHAR;

    BEGIN
        Strings.Assign ("         Socket error ", LogLine);
        AppendCard (sock_errno(), LogLine);
        LogTransaction (LogID, LogLine);
    END WriteError;

(************************************************************************)

PROCEDURE RunTheServer (ns: Socket);

    (*  OPERATING AS A SERVER                                                       *)
    (*     1. (Compulsory) Call "bind" to bind the socket with a local address.     *)
    (*        You can usually afford to specify INADDR_ANY as the machine           *)
    (*        address, but you'd normally bind to a specific port number.           *)
    (*     2. Call "listen" to indicate your willingness to accept connections.     *)
    (*     3. Call "accept", getting a new socket (say ns) from the client.         *)
    (*     4. Use procedures "send" and "recv" to transfer data, using socket ns.   *)
    (*        (Meanwhile, your original socket remains available to accept          *)
    (*        more connections, so you can continue with more "accept" operations   *)
    (*        in parallel with these data operations.  If so, you should of course  *)
    (*        be prepared to run multiple threads.)                                 *)
    (*     5. Use "soclose(ns)" to terminate the session with that particular       *)
    (*        client.                                                               *)
    (*     6. Use "soclose" on your original socket to clean up at the end.         *)

    CONST Blanks = "                             ";

    VAR myaddr, client: SockAddr;
        temp: CARDINAL;
        SocketsToTest: ARRAY [0..0] OF Socket;
        StartupSuccessful: BOOLEAN;
        LogID: TransactionLogID;
        message: ARRAY [0..127] OF CHAR;

    BEGIN
        LogID := CreateLogID (WCtx, "       ");
        IF sock_init() <> 0 THEN
            IF ScreenEnabled THEN
                WriteString ("No network.");
            END (*IF*);
            RETURN;
        END (*IF*);

        IF NOT IMAPEnabled THEN
            WriteLn;
            WriteString ("          IMAP disabled, please run Setup.exe.");
            WriteLn;
            WriteLn;

        ELSIF CalledFromInetd THEN

            Strings.Assign ("IMAP4 started from inetd, socket ", message);
            AppendCard (ns, message);
            LogTransaction (LogID, message);
            temp := SIZE(client);
            getpeername (ns, client, temp);
            IF NOT NewSession (ns, client) THEN
                LogTransactionL (LogID, "Failed to create handler thread");
            END (*IF*);

        ELSE

            IF ScreenEnabled THEN
                SetBoundary (2, 29);  ClearScreen;
                WriteStringAt (0, 0, "IMAP4 Weasel plugin v");
                WriteStringAt (0, 21, IMV.version);
                WriteStringAt (0, 27, "Copyright (C) 2005-2014 Peter Moylan");
                WriteStringAt (0, 66, "Users: 0");

                EVAL (SetBreakHandler (ControlCHandler));
            END (*IF*);

            StartupSuccessful := FALSE;
            IF CreateTask (ShutdownChecker, 1, "ctrl/c hook") THEN

                MainSocket := socket (AF_INET, SOCK_STREAM, AF_UNSPEC);

                (* Allow reuse of the port we're binding to. *)

                temp := 1;
                setsockopt (MainSocket, 0FFFFH, 4, temp, SIZE(CARDINAL));

                IF ScreenEnabled THEN
                    WriteString (Blanks);
                    WriteString ("IMAP4 listening on port ");
                    WriteCard (ServerPort);
                    WriteString (", socket ");
                    WriteCard (MainSocket);
                    WriteLn;
                END (*IF*);

                (* Now have the socket, bind to our machine. *)

                WITH myaddr DO
                    family := AF_INET;
                    WITH in_addr DO
                        port := Swap2 (ServerPort);
                        (* Bind to all interfaces. *)
                        addr := INADDR_ANY;
                        zero := Zero8;
                    END (*WITH*);
                END (*WITH*);

                IF bind (MainSocket, myaddr, SIZE(myaddr)) THEN
                    IF ScreenEnabled THEN
                        WriteError (LogID);
                        WriteString (Blanks);
                        WriteString ("Cannot bind to server port.");
                        WriteLn;
                    END (*IF*);

                ELSE

                    (* Go into listening mode. *)

                    IF listen (MainSocket, 1) THEN
                        IF ScreenEnabled THEN
                            WriteError (LogID);
                        END (*IF*);
                    ELSE
                        StartupSuccessful := TRUE;
                    END (*IF*);

                END (*IF bind*);
            END (*IF CreateTask*);

            IF NOT StartupSuccessful THEN
                LogTransactionL (LogID, "Startup failed");
            END (*IF*);

            IF StartupSuccessful THEN

                LogTransactionL (LogID, "Server started.");

                (* Here's the main service loop. *)

                SocketsToTest[0] := MainSocket;
                WHILE select (SocketsToTest, 1, 0, 0, MAX(CARDINAL)) > 0 DO
                    IF SocketsToTest[0] <> NotASocket THEN
                        temp := SIZE(client);
                        ns := accept (MainSocket, client, temp);
                        IF ns <> NotASocket THEN
                            IF NOT NewSession (ns, client) THEN
                                LogTransactionL (LogID,
                                         "Failed to create handler thread");
                            END (*IF*);
                        END (*IF*);
                    END (*IF*);
                    SocketsToTest[0] := MainSocket;
                END (*WHILE*);

                (* Close the socket. *)

                IF soclose(MainSocket) THEN
                    psock_errno ("");
                END (*IF*);

            END (*IF*);

        END (*IF (not) CalledFromInetd *);

        (* End of operation, shut down the server. *)

        IF NOT RapidShutdown THEN
            IF CalledFromInetd THEN
                Sleep (3000);
            ELSIF NumberOfUsers() > 0 THEN
                LogTransactionL (LogID, "Waiting for existing users to finish");
                StopAllUsers;
            END (*IF*);
            WHILE (NumberOfUsers() > 0) AND NOT RapidShutdown DO
                Sleep (1000);
            END (*WHILE*);
            RapidShutdown := TRUE;
        END (*IF*);

        LogTransactionL (LogID, "IMAP4 closing down");

    END RunTheServer;

(********************************************************************************)
(*                   TASK TO CATCH EXTERNAL SHUTDOWN REQUESTS                   *)
(********************************************************************************)

PROCEDURE ShutdownRequestDetector;

    (* Runs as a separate task, and responds to the global event semaphore      *)
    (* that requests Imapd to shut down.                                        *)

    CONST semName = "\SEM32\IMAP\SHUTDOWN";

    BEGIN
        ShutdownSignal := 0;
        IF OS2.DosOpenEventSem (semName, ShutdownSignal) = OS2.ERROR_SEM_NOT_FOUND THEN
            OS2.DosCreateEventSem (semName, ShutdownSignal, OS2.DC_SEM_SHARED, FALSE);
        END (*IF*);

        (* A second signal on the same semaphore means that we want a *)
        (* rapid shutdown (or that shutdown has already happened.)    *)

        WHILE NOT RapidShutdown DO
            OS2.DosWaitEventSem (ShutdownSignal, OS2.SEM_INDEFINITE_WAIT);
            Signal (ShutdownRequest);
        END (*WHILE*);

        Signal (TaskDone);

    END ShutdownRequestDetector;

(********************************************************************************)
(*                                 MAIN PROGRAM                                 *)
(********************************************************************************)

VAR ns: Socket;
    ININame: ARRAY [0..15] OF CHAR;
    count: CARDINAL;

BEGIN
    ScreenEnabled := NotDetached();
    UseTNI := FALSE;
    CalledFromInetd := GetParameters (ns);
    IF UseTNI THEN
        ININame := "WEASEL.TNI";
    ELSE
        ININame := "WEASEL.INI";
    END (*IF*);
    IF FileSys.Exists (ININame) THEN
        InitUIDValidity;
        LoadINIData;
        CheckRegistration(UseTNI);
        RefreshMasterDomainList (TRUE);
        ShutdownInProgress := FALSE;  RapidShutdown := FALSE;
        CreateSemaphore (ShutdownRequest, 0);
        CreateSemaphore (TaskDone, 0);
        EVAL (CreateTask (ShutdownRequestDetector, 2, "shutdown"));
        RunTheServer (ns);
        OS2.DosPostEventSem (ShutdownSignal);
        OS2.DosResetEventSem (ShutdownSignal, count);
        Wait (TaskDone);
ELSE
        WriteString ("Missing file ");
        WriteString (ININame);
        WriteLn;
        WriteString ("Run Setup then try again.");
        WriteLn;
    END (*IF*);
END IMAPD.

