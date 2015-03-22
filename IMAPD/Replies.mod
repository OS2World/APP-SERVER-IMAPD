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

IMPLEMENTATION MODULE Replies;

        (********************************************************)
        (*                                                      *)
        (*      Replies to the client in the IMAP4 server       *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            02 March 2003                   *)
        (*  Last edited:        17 January 2013                 *)
        (*  Status:             OK so far                       *)
        (*                                                      *)
        (********************************************************)


IMPORT Strings;

FROM SBuffers IMPORT
    (* type *)  SBuffer,
    (* proc *)  SendString, SendEOL, SendLine, GetLine;

FROM TransLog IMPORT
    (* type *)  TransactionLogID,
    (* proc *)  LogTransaction;

FROM Inet2Misc IMPORT
    (* proc *)  EVAL;

FROM TaskControl IMPORT
    (* type *)  Lock,
    (* proc *)  CreateLock, DestroyLock, Obtain, Release;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

(************************************************************************)

CONST
    Nul = CHR(0);
    LF = CHR(10);
    MaxIndex = 1023;
    MaxLLIndex = 4095;      (* but TransLog only accept 256 chars,      *)
                            (* so we'll put long lines out in sections. *)

VAR CR: ARRAY [0..1] OF CHAR;

TYPE
    ReplyCxt = POINTER TO
                   RECORD
                       access: Lock;
                       SB: SBuffer;
                       InProgress: BOOLEAN;
                       LogEnabled: BOOLEAN;
                       WasLogging: BOOLEAN;
                       ID: TransactionLogID;
                       partial: ARRAY [0..MaxIndex] OF CHAR;
                       LogLine: ARRAY [0..MaxLLIndex] OF CHAR;
                   END (*RECORD*);

(************************************************************************)
(*                  OPENING AND CLOSING A REPLY CONTEXT                 *)
(************************************************************************)

PROCEDURE OpenReplyGroup (SB: SBuffer;  ID: TransactionLogID): ReplyCxt;

    (* Opens a new session. *)

    VAR RC: ReplyCxt;

    BEGIN
        NEW (RC);
        CreateLock (RC^.access);
        RC^.SB := SB;
        RC^.ID := ID;
        RC^.InProgress := FALSE;
        RC^.LogEnabled := TRUE;
        RC^.WasLogging := TRUE;
        RC^.partial[0] := Nul;
        RC^.LogLine[0] := Nul;
        RETURN RC;
    END OpenReplyGroup;

(************************************************************************)

PROCEDURE CloseReplyGroup (VAR (*INOUT*) RC: ReplyCxt);

    (* Closes a session. *)

    BEGIN
        DestroyLock (RC^.access);
        DISPOSE (RC);
    END CloseReplyGroup;

(************************************************************************)
(*                               LOGGING                                *)
(************************************************************************)

PROCEDURE PartialLogFlush (VAR (*INOUT*) line: ARRAY OF CHAR;
                                         ID: TransactionLogID;
                                         LogEnabled: BOOLEAN): CARDINAL;

    (* Sends the first part of RC^.LogLine to the log, returns *)
    (* the number of bytes written.                            *)

    CONST LLmax = 255;

    VAR pos, amount: CARDINAL;
        found: BOOLEAN;
        outline: ARRAY [0..LLmax] OF CHAR;

    BEGIN
        (* Search for end-of-line in the LogLine part.  Put first   *)
        (* line out if found, otherwise put out up to LLmax chars.  *)

        Strings.FindNext (CR, line, 0, found, pos);
        IF NOT found THEN
            pos := LENGTH(line);
        END (*IF*);
        IF pos > LLmax THEN
            pos := LLmax;
        END (*IF*);
        amount := pos;

        IF amount = 0 THEN
            outline[0] := Nul;
        ELSE
            Strings.Extract (line, 0, amount, outline);
        END (*IF*);

        (* Delete what we've extracted, plus line terminators if present. *)

        IF amount > 0 THEN
            IF line[pos] = CR[0] THEN
                INC (pos);
            END (*IF*);
            IF line[pos] = LF THEN
                INC (pos);
            END (*IF*);
            Strings.Delete (line, 0, pos);
        END (*IF*);

        IF amount > 0 THEN
            IF LogEnabled THEN
                LogTransaction (ID, outline);
            END (*IF*);
        END (*IF*);

        RETURN amount;

    END PartialLogFlush;

(************************************************************************)

PROCEDURE OldPartialLogFlush (RC: ReplyCxt): CARDINAL;

    (* Sends the first part of RC^.LogLine to the log, returns *)
    (* the number of bytes written.                            *)

    VAR pos: CARDINAL;
        found: BOOLEAN;
        outline: ARRAY [0..255] OF CHAR;

    BEGIN
        (* Search for end-of-line in the LogLine part.  Put first   *)
        (* line out if we find a first line.                        *)

        Strings.FindNext (CR, RC^.LogLine, 0, found, pos);
        IF found THEN
            IF pos > 0 THEN
                Strings.Extract (RC^.LogLine, 0, pos, outline);
            END (*IF*);
            outline[pos] := Nul;
            INC (pos);
            IF RC^.LogLine[pos] = LF THEN
                INC (pos);
            END (*IF*);

        ELSIF LENGTH(RC^.LogLine) > 256 THEN

            (* We have no choice but to break up a long line. *)

            pos := 256;
            Strings.Extract (RC^.LogLine, 0, pos, outline);

        ELSE
            pos := 0;
        END (*IF*);

        IF pos > 0 THEN
            IF RC^.LogEnabled THEN
                LogTransaction (RC^.ID, outline);
            END (*IF*);
            Strings.Delete (RC^.LogLine, 0, pos);
        END (*IF*);

        RETURN pos;

    END OldPartialLogFlush;

(************************************************************************)

PROCEDURE AddToLog (RC: ReplyCxt);

    (* Appends RC^.partial to RC^.LogLine, sending the leading part     *)
    (* to the log if appropriate.                                       *)

    VAR remaining, amount: CARDINAL;

    BEGIN
        remaining := LENGTH(RC^.LogLine) + LENGTH(RC^.partial);
        WHILE remaining > MaxLLIndex DO
            amount := PartialLogFlush (RC^.LogLine, RC^.ID, RC^.LogEnabled);
            DEC (remaining, amount);
        END (*WHILE*);
        Strings.Append (RC^.partial, RC^.LogLine);
    END AddToLog;

(************************************************************************)

PROCEDURE FlushLog (RC: ReplyCxt);

    (* Appends RC^.partial to RC^.LogLine, sending the leading part     *)
    (* to the log if appropriate.                                       *)

    VAR amount: CARDINAL;

    BEGIN
        REPEAT
            amount := PartialLogFlush (RC^.LogLine, RC^.ID, RC^.LogEnabled);
        UNTIL amount = 0;

        IF RC^.LogEnabled AND (RC^.LogLine[0] <> Nul) THEN
            LogTransaction (RC^.ID, RC^.LogLine);
        END (*IF*);
        RC^.LogLine[0] := Nul;
    END FlushLog;

(************************************************************************)

PROCEDURE LogMessage (RC: ReplyCxt;  message: ARRAY OF CHAR);

    (* Sends message to log, but not to output channel. *)

    BEGIN
        IF RC^.LogEnabled THEN
            LogTransaction (RC^.ID, message);
        END (*IF*);
    END LogMessage;

(************************************************************************)

PROCEDURE SuppressLogging (VAR (*INOUT*) RC: ReplyCxt);

    (* Temporarily turns off logging. *)

    BEGIN
        FlushLog (RC);
        RC^.WasLogging := RC^.LogEnabled;
        RC^.LogEnabled := FALSE;
    END SuppressLogging;

(************************************************************************)

PROCEDURE RestoreLogging (VAR (*INOUT*) RC: ReplyCxt);

    (* Reverses the effect of the SuppressLogging call. *)

    BEGIN
        FlushLog (RC);
        RC^.LogEnabled := RC^.WasLogging;
    END RestoreLogging;

(************************************************************************)
(*                        RESPONSES TO THE CLIENT                       *)
(************************************************************************)

PROCEDURE SendBuffer (RC: ReplyCxt);

    (* Sends out the partial line accumulated so far.  We assume that *)
    (* we already have exclusive access to RC.                        *)

    BEGIN
        IF NOT RC^.InProgress THEN
            RC^.LogLine[0] := Nul;
            RC^.InProgress := TRUE;
        END (*IF*);
        IF RC^.partial[0] <> Nul THEN
            IF RC^.LogEnabled THEN
                AddToLog (RC);
            END (*IF*);
            EVAL (SendString (RC^.SB, RC^.partial));
            RC^.partial[0] := Nul;
        END (*IF*);
    END SendBuffer;

(************************************************************************)

PROCEDURE PartialReply (RC: ReplyCxt;  str: ARRAY OF CHAR);

    (* Adds a substring to the output line being built. *)

    VAR remaining: CARDINAL;

    BEGIN
        Obtain (RC^.access);
        remaining := LENGTH(str) + LENGTH(RC^.partial);
        WHILE remaining > MaxIndex DO
            DEC (remaining, LENGTH(RC^.partial));
            SendBuffer (RC);
            IF remaining > MaxIndex THEN
                Strings.Extract (str, 0, MaxIndex, RC^.partial);
                Strings.Delete (str, 0, MaxIndex);
            END (*IF*);
        END (*WHILE*);
        IF remaining > 0 THEN
            Strings.Append (str, RC^.partial);
        END (*IF*);
        Release (RC^.access);
    END PartialReply;

(************************************************************************)

PROCEDURE CommitReply (RC: ReplyCxt);

    (* Sends the line that has been built with PartialReply, including *)
    (* adding an EOL to it.                                            *)

    BEGIN
        Obtain (RC^.access);
        SendBuffer (RC);
        IF RC^.InProgress THEN
            FlushLog (RC);
            EVAL (SendEOL (RC^.SB));
            RC^.InProgress := FALSE;
        END (*IF*);
        Release (RC^.access);
    END CommitReply;

(************************************************************************)

PROCEDURE Reply (RC: ReplyCxt;  str: ARRAY OF CHAR);

    (* Sends one line without any sort of tag marker. *)

    BEGIN
        Obtain (RC^.access);
        IF SendLine (RC^.SB, str) THEN
            IF RC^.LogEnabled THEN
                LogTransaction (RC^.ID, str);
            END (*IF*);
        END (*IF*);
        RC^.partial[0] := Nul;
        Release (RC^.access);
    END Reply;

(************************************************************************)

PROCEDURE PartialReplyUntagged (RC: ReplyCxt;  str: ARRAY OF CHAR);

    (* Starts an untagged response in the output line being built. *)

    BEGIN
        Obtain (RC^.access);
        Strings.Append ("* ", RC^.partial);
        IF LENGTH(str) + LENGTH(RC^.partial) > MaxIndex THEN
            SendBuffer (RC);
        END (*IF*);
        IF str[0] <> Nul THEN
            Strings.Append (str, RC^.partial);
        END (*IF*);
        Release (RC^.access);
    END PartialReplyUntagged;

(************************************************************************)

PROCEDURE ReplyUntagged (RC: ReplyCxt;  str: ARRAY OF CHAR);

    VAR buffer: ARRAY [0..MaxIndex] OF CHAR;

    BEGIN
        Obtain (RC^.access);
        Strings.Assign ("* ", buffer);
        Strings.Append (str, buffer);
        IF SendLine (RC^.SB, buffer) THEN
            IF RC^.LogEnabled THEN
                LogTransaction (RC^.ID, buffer);
            END (*IF*);
        END (*IF*);
        RC^.partial[0] := Nul;
        Release (RC^.access);
    END ReplyUntagged;

(************************************************************************)

PROCEDURE Reply2Untagged (RC: ReplyCxt;  str1: ARRAY OF CHAR;
                                         VAR (*IN*) str2: ARRAY OF CHAR);

    VAR buffer: ARRAY [0..MaxIndex] OF CHAR;

    BEGIN
        Obtain (RC^.access);
        Strings.Assign ("* ", buffer);
        IF str1[0] <> Nul THEN
            Strings.Append (str1, buffer);
        END (*IF*);
        IF str2[0] <> Nul THEN
            Strings.Append (str2, buffer);
        END (*IF*);
        IF SendLine (RC^.SB, buffer) THEN
            IF RC^.LogEnabled THEN
                LogTransaction (RC^.ID, buffer);
            END (*IF*);
        END (*IF*);
        RC^.partial[0] := Nul;
        Release (RC^.access);
    END Reply2Untagged;

(************************************************************************)

PROCEDURE Reply3Untagged (RC: ReplyCxt;  str1, str2, str3: ARRAY OF CHAR);

    VAR buffer: ARRAY [0..MaxIndex] OF CHAR;

    BEGIN
        Obtain (RC^.access);
        Strings.Assign ("* ", buffer);
        IF str1[0] <> Nul THEN
            Strings.Append (str1, buffer);
        END (*IF*);
        IF str2[0] <> Nul THEN
            Strings.Append (str2, buffer);
        END (*IF*);
        IF str3[0] <> Nul THEN
            Strings.Append (str3, buffer);
        END (*IF*);
        IF SendLine (RC^.SB, buffer) THEN
            IF RC^.LogEnabled THEN
                LogTransaction (RC^.ID, buffer);
            END (*IF*);
        END (*IF*);
        RC^.partial[0] := Nul;
        Release (RC^.access);
    END Reply3Untagged;

(************************************************************************)

(*
PROCEDURE Reply4Untagged (RC: ReplyCxt;
                                   str1, str2, str3, str4: ARRAY OF CHAR);

    VAR buffer: ARRAY [0..MaxIndex] OF CHAR;

    BEGIN
        Obtain (RC^.access);
        Strings.Assign ("* ", buffer);
        Strings.Append (str1, buffer);
        Strings.Append (str2, buffer);
        Strings.Append (str3, buffer);
        Strings.Append (str4, buffer);
        IF SendLine (RC^.SB, buffer) THEN
            IF RC^.LogEnabled THEN
                LogTransaction (RC^.ID, buffer);
            END (*IF*);
        END (*IF*);
        RC^.partial[0] := Nul;
        Release (RC^.access);
    END Reply4Untagged;
*)

(************************************************************************)

PROCEDURE Reply5Untagged (RC: ReplyCxt;
                             str1, str2, str3, str4, str5: ARRAY OF CHAR);

    VAR buffer: ARRAY [0..MaxIndex] OF CHAR;

    BEGIN
        Obtain (RC^.access);
        Strings.Assign ("* ", buffer);
        Strings.Append (str1, buffer);
        Strings.Append (str2, buffer);
        Strings.Append (str3, buffer);
        Strings.Append (str4, buffer);
        Strings.Append (str5, buffer);
        IF SendLine (RC^.SB, buffer) THEN
            IF RC^.LogEnabled THEN
                LogTransaction (RC^.ID, buffer);
            END (*IF*);
        END (*IF*);
        RC^.partial[0] := Nul;
        Release (RC^.access);
    END Reply5Untagged;

(************************************************************************)

PROCEDURE AcceptLine (RC: ReplyCxt;  VAR (*OUT*) result: ARRAY OF CHAR): BOOLEAN;

    (* Reads one line of input, logs it if logging enabled.  *)

    VAR success: BOOLEAN;

    BEGIN
        Obtain (RC^.access);
        success := GetLine (RC^.SB, result);
        IF success THEN
            IF RC^.LogEnabled THEN
                LogTransaction (RC^.ID, result);
            END (*IF*);
        END (*IF*);
        Release (RC^.access);
        RETURN success;
    END AcceptLine;

(************************************************************************)

BEGIN
    CR[0] := CHR(13);
    CR[1] := Nul;
END Replies.

