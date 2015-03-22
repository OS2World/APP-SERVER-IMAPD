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

IMPLEMENTATION MODULE Messages;

        (********************************************************)
        (*                                                      *)
        (*         Properties of messages at the file level     *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            16 March 2003                   *)
        (*  Last edited:        12 January 2014                 *)
        (*  Status:                                             *)
        (*           Undergoing a major overhaul.               *)
        (*                                                      *)
        (*     BODY[part]<partial> implemented but untested.    *)
        (*     I'm now working on a new version of fetching     *)
        (*     that is based on the fact that the structure     *)
        (*     has usually already been worked out.             *)
        (*                                                      *)
        (*     Now resuming this job after a long time gap,     *)
        (*     so I'll have to re-check my design.              *)
        (*                                                      *)
        (*     Update 31-Aug-2009: BODY[]<partial> is working   *)
        (*     except for the fact that it seems to be          *)
        (*     returning too few bytes.  (10009 instead of      *)
        (*     10240, in the example I'm working on, so at      *)
        (*     least it's the right order of magnitude.)        *)
        (*                                                      *)
        (*     Procedure SendFragmentedFilePart needs more work *)
        (*     to cover the (unlikely but possible) "partial"   *)
        (*     case.                                            *)
        (*                                                      *)
        (********************************************************)

(*******************)
(* Debugging stuff *)
(*******************)

FROM IOChan IMPORT ChanId;
FROM SeqFile IMPORT text, write, OpenWrite, OpenResults, Close;
FROM TextIO IMPORT WriteChar, WriteString, WriteLn;

(*******************)

FROM TransLog IMPORT
    (* type *)  TransactionLogID,
    (* proc *)  LogTransaction;

IMPORT Strings, FileSys;

FROM BufferedTextStreams IMPORT
    (* type *)  TextStream,
    (* proc *)  OpenForReading, CloseTS, TSReadRaw, TSReadLine,
                TSStartPosition, TSCurrentPosition, TSSetPosition;

FROM FileOps IMPORT
    (* type *)  (*ChanId,*) DirectoryEntry, FilePos,
    (* proc *)  GetFileSize, OpenOldFile, CloseFile, OpenNewFile1,
                FWriteString, FWriteLn, FirstDirEntry, DirSearchDone,
                SetFileSize, WriteRaw, DeleteFile, ReadRaw;

FROM Parser IMPORT
    (* proc *)  Date, PackDate, Match, SPACE, AString, Number;

FROM Replies IMPORT
    (* type *)  ReplyCxt,
    (* proc *)  Reply, PartialReply, CommitReply, LogMessage,
                SuppressLogging, RestoreLogging;

FROM Names IMPORT
    (* type *)  FilenameString, ArgPointer;

FROM Conversions IMPORT
    (* proc *)  CardinalToStringLJ;

FROM Inet2Misc IMPORT
    (* proc *)  StringMatch, EVAL;

FROM Types IMPORT
    (* type *)  CARD64;

FROM LONGLONG IMPORT
    (* proc *)  Add64, Sub64, Diff64, ORDL;

FROM TaskControl IMPORT
    (* type *)  Lock,
    (* proc *)  CreateLock, Obtain, Release;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

(************************************************************************)

CONST
    Nul = CHR(0);  Tab = CHR(9);  Space = ' ';  CtrlZ = CHR(26);

TYPE
    CharSet = SET OF CHAR;

CONST
    Digits = CharSet {'0'..'9'};

TYPE
    Param = ARRAY [0..511] OF CHAR;
    ParamList = POINTER TO
                    RECORD
                        next: ParamList;
                        text: Param;
                    END (*RECORD*);

    (* BodyInfo records what is being requested in a FETCH BODY[]       *)
    (* request.  The standard leaves it ambiguous as to whether         *)
    (* multiple parts can be requested in a single fetch.  (The syntax  *)
    (* permits it, but it's hard to tell whether that is what was       *)
    (* intended.)  For now I'm allowing for a single part, which means  *)
    (* that the "BodyParts" type definition is likely to be scrapped.   *)
    (* In the longer term it might be necessary to move to the          *)
    (* MultiBodyInfo linked list structure which is declared below.     *)
    (* If so, the parsing in module Boxes will probably also have to    *)
    (* be modified.  All that is certain for now is that some of the    *)
    (* following declarations are redundant; but I don't yet know       *)
    (* which ones.                                                      *)

    BodyPart = (HEADER_FIELDS, HEADER_FIELDS_NOT, HEADER, MIME, TEXT, ALL);
    BodyParts = SET OF BodyPart;
    BodyInfo = POINTER TO RECORD
                   section: ARRAY [0..63] OF CHAR;
                   part: BodyPart;
                   parts: BodyParts;
                   Headers: ParamList;
                   partial: BOOLEAN;
                   initialoffset, amount: CARDINAL;

                   (* Note: if partial is FALSE then initialoffset = 0  *)
                   (* and amount = MAX(CARDINAL).                       *)

               END (*RECORD*);

    (* New version of the above - which I will probably scrap. *)

    OneBodyInfo = POINTER TO RECORD
                      section: ARRAY [0..63] OF CHAR;
                      part: BodyPart;
                      Headers: ParamList;
                      partial: BOOLEAN;
                      initialoffset, amount: CARDINAL;
                  END (*RECORD*);

    MultiBodyInfo = POINTER TO MultiRecord;
    MultiRecord = RECORD
                      this: OneBodyInfo;
                      next: MultiBodyInfo;
                  END (*RECORD*);

VAR
    (* NextName is a string used in generating unique file names. *)

    NextName: ARRAY [0..7] OF CHAR;
    NextNameLock: Lock;

(************************************************************************)
(*                    CREATING A UNIQUE FILENAME                        *)
(************************************************************************)

(*
PROCEDURE MakeUniqueName (VAR (*OUT*) name: ARRAY OF CHAR);

    (* Generates a unique 8-character string.  The argument must of     *)
    (* course be big enough to take at least 8 characters.              *)

    (********************************************************************)

    PROCEDURE Increment (N: CARDINAL);

        (* Increments NextName[N], with carry as appropriate. *)

        BEGIN
            IF NextName[N] = '9' THEN
                NextName[N] := 'A';
            ELSIF NextName[N] = 'Z' THEN
                IF N = 0 THEN
                    NextName := "00000000";
                ELSE
                    NextName[N] := '0';
                    Increment (N-1);
                END (*IF*);
            ELSE
                INC (NextName[N]);
            END (*IF*);
        END Increment;

    (********************************************************************)

    BEGIN
        Obtain (NextNameLock);
        Strings.Assign (NextName, name);
        Increment (7);
        Release (NextNameLock);
    END MakeUniqueName;

(************************************************************************)
*)
(*
PROCEDURE MakeNewFilename (BaseName, tail: ARRAY OF CHAR;
                       VAR (*OUT*) NewName: ARRAY OF CHAR);

    (* Creates a file name of the form BaseNamexxxtail, where xxx is    *)
    (* chosen such that a file of that name does not already exist.     *)
    (* Note that BaseName and tail can include punctuation.             *)

    VAR UName: FilenameString;

    BEGIN
        REPEAT
            MakeUniqueName (UName);
            Strings.Assign (BaseName, NewName);
            Strings.Append (UName, NewName);
            Strings.Append (tail, NewName);
        UNTIL NOT FileSys.Exists(NewName);
    END MakeNewFilename;
*)
(************************************************************************)

(*
PROCEDURE OpenNewOutputFile (BaseName, tail: ARRAY OF CHAR;
                       VAR (*OUT*) NewName: FilenameString): ChanId;

    (* Creates a file name of the form BaseNamexxxtail, where xxx is    *)
    (* chosen such that a file of that name does not already exist, and *)
    (* opens that file.                                                 *)
    (* Note that BaseName and tail can include punctuation.             *)

    VAR cid: ChanId;  duplication: BOOLEAN;

    BEGIN
        REPEAT
            MakeNewFilename (BaseName, tail, NewName);
            cid := OpenNewFile1 (NewName, duplication);
        UNTIL NOT duplication;
        RETURN cid;
    END OpenNewOutputFile;
*)

(************************************************************************)
(*                          MESSAGE SIZE                                *)
(************************************************************************)

PROCEDURE SizeOfMessage (VAR (*IN*) file: FilenameString): CARD64;

    (* Returns the RFC822.SIZE attribute. *)

    BEGIN
        RETURN GetFileSize (file);
    END SizeOfMessage;

(************************************************************************)
(*                            DATE OPERATIONS                           *)
(************************************************************************)

PROCEDURE PackedDate (VAR (*IN*) file: FilenameString): CARDINAL;

    (* Returns the date (in packed format) that this file was received. *)

    VAR D: DirectoryEntry;
        packeddate: CARDINAL;

    BEGIN
        IF FirstDirEntry (file, FALSE, TRUE, D) THEN
            packeddate := D.datePkd;
        ELSE
            packeddate := 0;
        END (*IF*);
        DirSearchDone (D);
        RETURN packeddate;
    END PackedDate;

(************************************************************************)

PROCEDURE PackedDateTime (VAR (*IN*) file: FilenameString;  VAR (*OUT*) date, time: CARDINAL);

    (* Returns the date and time (in packed format) that this file was received. *)

    VAR D: DirectoryEntry;

    BEGIN
        IF FirstDirEntry (file, FALSE, TRUE, D) THEN
            date := D.datePkd;
            time := D.timePkd;
        ELSE
            date := 0;
            time := 0;
        END (*IF*);
        DirSearchDone (D);
    END PackedDateTime;

(************************************************************************)

PROCEDURE HeaderMatch (VAR (*IN*) line: ARRAY OF CHAR;
                                header: ARRAY OF CHAR): BOOLEAN;  FORWARD;

(************************************************************************)

PROCEDURE GetHeaderText (file: FilenameString;  key: ARRAY OF CHAR;
                            VAR (*OUT*) text: ARRAY OF CHAR): BOOLEAN;

    (* Sets text to the string following 'key:' in the header lines.  *)

    VAR InHeader, found: BOOLEAN;
        TS: TextStream;
        line: ARRAY [0..2047] OF CHAR;

    BEGIN
        found := FALSE;  InHeader := TRUE;
        TS := OpenForReading (file);
        WHILE InHeader AND NOT found DO
            TSReadLine (TS, line);
            IF (line[0] = CtrlZ) OR (line[0] = Nul) THEN
                InHeader := FALSE;
            ELSIF HeaderMatch (line, key) THEN
                Strings.Assign (line, text);
                Strings.Delete (text, 0, LENGTH(key)+1);
                found := TRUE;
            END (*IF*);
        END (*WHILE*);
        CloseTS (TS);
        RETURN found;
    END GetHeaderText;

(************************************************************************)

PROCEDURE HeaderDate (VAR (*IN*) file: FilenameString): CARDINAL;

    (* Returns the date (in packed format) from the 'Date:' header      *)
    (* line.  If there is no such line, date is 00 Jan 1980.            *)

    VAR result, pos: CARDINAL;
        text: ARRAY [0..2047] OF CHAR;
        datestring: ArgPointer;

    BEGIN
        IF GetHeaderText (file, 'Date', text) THEN
            pos := 0;
            WHILE (pos < 2048) AND NOT (text[pos] IN Digits) DO
                INC (pos);
            END (*WHILE*);
            IF pos > 0 THEN
                Strings.Delete (text, 0, pos);
            END (*IF*);
            NEW (datestring);
            IF Date (text, datestring^) THEN
                result := PackDate (datestring);
            ELSE
                result := 0;
            END (*IF*);
            DISPOSE (datestring);
        ELSE
            result := 0;
        END (*IF*);
        RETURN result;
    END HeaderDate;

(************************************************************************)
(*                             SEARCHES                                 *)
(*  Note that these do not require any parsing of the MIME structure,   *)
(*  since for our present searches it is sufficient to treat a message  *)
(*  as something of the form (header)+(everything else).                *)
(************************************************************************)

PROCEDURE HeaderMatch (VAR (*IN*) line: ARRAY OF CHAR;
                                  header: ARRAY OF CHAR): BOOLEAN;

    (* Returns TRUE iff header':' is a leading substring of line. *)

    VAR N: CARDINAL;
        test: ARRAY [0..1023] OF CHAR;

    BEGIN
        N := Strings.Length (header);

        (* Check for the colon first, because this will eliminate  *)
        (* most cases.                                             *)

        IF line[N] = ':' THEN
            Strings.Assign (line, test);
            test[N] := Nul;
            Strings.Capitalize (test);
            Strings.Capitalize (header);
            RETURN Strings.Equal (test, header);
        ELSE
            RETURN FALSE;
        END (*IF*);

    END HeaderMatch;

(************************************************************************)

PROCEDURE HeaderListMatch (VAR (*IN*) line: ARRAY OF CHAR;
                               VAR (*INOUT*) Headers: ParamList): BOOLEAN;

    (* Returns TRUE iff header':' is a leading substring of line, where *)
    (* header is one element of Headers.  We don't delete any of the    *)
    (* list, because it could be used more than once.                   *)

    VAR this: ParamList;
        success: BOOLEAN;

    BEGIN
        success := FALSE;  this := Headers;
        WHILE NOT success AND (this <> NIL) DO
            IF HeaderMatch (line, this^.text) THEN
                success := TRUE;
            ELSE
                this := this^.next;
            END (*IF*);
        END (*WHILE*);
        RETURN success;
    END HeaderListMatch;

(************************************************************************)

PROCEDURE MatchText (skip: CARDINAL;
                           VAR (*IN*) line, text: ARRAY OF CHAR): BOOLEAN;

    (* Returns TRUE iff text is a substring of line.  Before checking   *)
    (* we skip the first 'skip' characters of line.                     *)

    VAR pos: CARDINAL;
        found: BOOLEAN;

    BEGIN
        Strings.Capitalize (line);
        Strings.Capitalize (text);
        Strings.FindNext (text, line, skip, found, pos);
        RETURN found;
    END MatchText;

(************************************************************************)

PROCEDURE FindInHeader (VAR (*IN*) file: FilenameString;
                            VAR (*IN*) key, text: ARRAY OF CHAR): BOOLEAN;

    (* Returns TRUE iff there is a header line labelled 'key:'  *)
    (* that has text as a substring.                            *)

    VAR InHeader, found, keyfound: BOOLEAN;
        TS: TextStream;
        line: ARRAY [0..2047] OF CHAR;

    BEGIN
        found := FALSE;  InHeader := TRUE;  keyfound := FALSE;
        TS := OpenForReading (file);
        WHILE InHeader AND NOT found DO
            TSReadLine (TS, line);
            IF (line[0] = CtrlZ) OR (line[0] = Nul) THEN
                InHeader := FALSE;
            ELSIF (line[0] = Space) OR (line[0] = Tab) THEN
                IF keyfound THEN
                    found := MatchText (1, line, text);
                END (*IF*);
            ELSIF HeaderMatch (line, key) THEN
                keyfound := TRUE;
                found := MatchText (Strings.Length(key)+1, line, text);
            ELSE
                keyfound := FALSE;
            END (*IF*);
        END (*WHILE*);
        CloseTS (TS);
        RETURN found;
    END FindInHeader;

(************************************************************************)

PROCEDURE FindInText (VAR (*IN*) file: FilenameString;
                        VAR (*IN*) string: ARRAY OF CHAR;
                                 IncludeHeader: BOOLEAN): BOOLEAN;

    (* Returns TRUE iff the file includes the given string      *)
    (* (case insensitive).  Searches the header lines as well   *)
    (* as the body iff IncludeHeader is TRUE.                   *)

    VAR found, InBody, EOF: BOOLEAN;
        TS: TextStream;
        line: ARRAY [0..2047] OF CHAR;

    BEGIN
        found := FALSE;  InBody := FALSE;  EOF := FALSE;
        TS := OpenForReading (file);
        REPEAT
            TSReadLine (TS, line);
            IF line[0] = CtrlZ THEN
                EOF := TRUE;
            ELSIF line[0] = Nul THEN
                InBody := TRUE;
            ELSIF InBody OR IncludeHeader THEN
                found := MatchText (1, line, string);
            END (*IF*);
        UNTIL found OR EOF;
        CloseTS (TS);
        RETURN found;
    END FindInText;

(************************************************************************)
(*                               ENVELOPE                               *)
(************************************************************************)

TYPE
    HeaderLine = ARRAY [0..1023] OF CHAR;

    EmailAddress = RECORD
                       PersonalName: HeaderLine;
                       AtDomainList: HeaderLine;
                       MailboxName:  HeaderLine;
                       HostName:     HeaderLine;
                   END (*RECORD*);

    EmailAddressList = POINTER TO
                           RECORD
                               next: EmailAddressList;
                               this: EmailAddress;
                           END (*RECORD*);

    EnvelopePtr = POINTER TO
                      RECORD
                          date:        HeaderLine;
                          subject:     HeaderLine;
                          from:        EmailAddressList;
                          sender:      EmailAddressList;
                          reply_to:    EmailAddressList;
                          to:          EmailAddressList;
                          cc:          EmailAddressList;
                          bcc:         EmailAddressList;
                          in_reply_to: HeaderLine;
                          message_id:  HeaderLine;
                      END (*RECORD*);

(************************************************************************)

PROCEDURE DiscardAddressList (VAR (*INOUT*) AL: EmailAddressList);

    (* Disposes of an envelope structure. *)

    VAR p: EmailAddressList;

    BEGIN
        WHILE AL <> NIL DO
            p := AL^.next;
            DISPOSE (AL);
            AL := p;
        END (*WHILE*);
    END DiscardAddressList;

(************************************************************************)

PROCEDURE DiscardEnvelope (VAR (*INOUT*) env: EnvelopePtr);

    (* Disposes of an envelope structure. *)

    BEGIN
        IF env <> NIL THEN
            DiscardAddressList (env^.from);
            DiscardAddressList (env^.sender);
            DiscardAddressList (env^.reply_to);
            DiscardAddressList (env^.to);
            DiscardAddressList (env^.cc);
            DiscardAddressList (env^.bcc);
            DISPOSE (env);
        END (*IF*);
    END DiscardEnvelope;

(************************************************************************)

PROCEDURE ParseEnvelope (TS: TextStream;  startpos: FilePos): EnvelopePtr;

    (* Extracts the envelope date from the header lines that start in   *)
    (* position startpos in the file.                                   *)

    VAR line: HeaderLine;

    (********************************************************************)

    PROCEDURE DeleteLeading (N: CARDINAL);

        (* Deletes the leading N characters of line, plus any space or  *)
        (* tab characters that follow.                                  *)

        BEGIN
            WHILE (line[N] = Space) OR (line[N] = Tab) DO
                INC (N);
            END (*WHILE*);
            IF N > 0 THEN
                Strings.Delete (line, 0, N);
            END (*IF*);
        END DeleteLeading;

    (********************************************************************)

    (*
    PROCEDURE StripSpaces (VAR (*INOUT*) str: ARRAY OF CHAR);

        (* Removes leading and trailing spaces from str. *)

        VAR k: CARDINAL;

        BEGIN
            k := 0;
            WHILE str[k] = ' ' DO
                INC (k);
            END (*WHILE*);
            IF k > 0 THEN
                Strings.Delete (str, 0, k);
            END (*IF*);
            k := Strings.Length(str);
            WHILE (k > 0) AND (str[k-1] = ' ') DO
                DEC (k);
            END (*WHILE*);
            str[k] := Nul;
        END StripSpaces;
    *)

    (********************************************************************)

    PROCEDURE StripChars (VAR (*INOUT*) str: ARRAY OF CHAR;
                          LeftSet, RightSet: CharSet);

        (* Removes characters in LeftSet from the beginning of str,     *)
        (* and characters in RightSet from the end of str.              *)

        VAR length: CARDINAL;  ch: CHAR;

        BEGIN
            ch := str[0];
            WHILE ch IN LeftSet DO
                Strings.Delete (str, 0, 1);
                ch := str[0];
            END (*WHILE*);
            length := Strings.Length(str);
            IF length > 0 THEN
               ch := str[length-1];
            END (*IF*);
            WHILE (length > 0) AND (ch IN RightSet) DO
                DEC (length);
                ch := str[length-1];
            END (*WHILE*);
            str[length] := Nul;
        END StripChars;

    (********************************************************************)

    PROCEDURE ParseAddress (VAR (*OUT*) addr: EmailAddress);

        (* Parses 'line' to extract the address components, using       *)
        (* RFC821 <path> syntax.  At least, that's the theory.  In      *)
        (* practice, three forms of the syntax occur to the exclusion   *)
        (* of all others:                                               *)
        (*     personalname <user@domain>                               *)
        (*     user@domain (personalname)                               *)
        (*     user@domain                                              *)
        (* but I'll also allow for                                      *)
        (*     <user@domain> (personalname)                             *)
        (*     (personalname) <user@domain>                             *)
        (* and that's all I bother to check.                            *)
        (* On exit, line has anything left after extracting these parts.*)

        VAR posat, length: CARDINAL;
            lineend, UADstart, UADend, PNstart, PNend: CARDINAL;
            foundat, foundangle, foundparen, PNpresent, UADpresent: BOOLEAN;
            UAD: HeaderLine;

        BEGIN
            PNpresent := TRUE;  UADpresent := TRUE;
            UADend := 0;  PNend := 0;
            lineend := Strings.Length(line);
            Strings.FindNext ('<', line, 0, foundangle, UADstart);
            Strings.FindNext ('(', line, 0, foundparen, PNstart);
            IF foundangle THEN
                IF foundparen THEN
                    Strings.FindNext ('>', line, UADstart+1, foundangle, UADend);
                    IF NOT foundangle THEN
                        IF UADstart >= PNstart THEN
                            UADend := lineend;
                        ELSE
                            UADend := PNstart-1;
                        END (*IF*);
                    END (*IF*);
                    Strings.FindNext (')', line, PNstart+1, foundparen, PNend);
                    IF NOT foundparen THEN
                        IF UADstart > PNstart THEN
                            PNend := UADstart-1;
                        ELSE
                            PNend := lineend;
                        END (*IF*);
                    END (*IF*);
                ELSE
                    (* Must be personalname <user@domain> format *)

                    Strings.FindNext ('>', line, UADstart+1, foundangle, UADend);
                    IF NOT foundangle THEN
                        UADend := lineend;
                    END (*IF*);
                    PNstart := 0;
                    IF UADstart = 0 THEN
                        PNpresent := FALSE;
                    ELSE
                        PNend := UADstart-1;
                    END (*IF*);
                END (*IF*);
            ELSIF foundparen THEN
                (* Must be user@domain (personalname) format. *)

                Strings.FindNext (')', line, PNstart+1, foundparen, PNend);
                IF NOT foundparen THEN
                    PNend := lineend;
                END (*IF*);
                UADstart := 0;
                IF PNstart = 0 THEN
                    UADpresent := FALSE;
                ELSE
                    UADend := PNstart-1;
                END (*IF*);
            ELSE
               (* No '(' or '< found.  We can still have the user@domain  *)
               (* form, terminated either by end-of-line or comma.        *)

               PNpresent := FALSE;
               UADstart := 0;  UADend := lineend;
               Strings.FindNext (',', line, 0, foundat, posat);
               IF foundat THEN
                   IF posat = 0 THEN UADpresent := FALSE;
                   ELSE UADend := posat-1;
                   END (*IF*);
               END (*IF*);
            END (*IF*);

            (* Now we know where the two components are, so extract them. *)

            PNpresent := PNpresent AND (PNend >= PNstart);
            IF PNpresent THEN
                Strings.Extract (line, PNstart, PNend-PNstart+1, addr.PersonalName);
            ELSE
                addr.PersonalName[0] := Nul;
            END (*IF*);

            UADpresent := UADpresent AND (UADend >= UADstart);
            IF UADpresent THEN
                Strings.Extract (line, UADstart, UADend-UADstart+1, UAD);
            ELSE
                UAD[0] := Nul;
            END (*IF*);

            (* Delete that part of the line array that we have used. *)

            IF PNpresent THEN
                length := PNend+1;
            ELSE
                length := 0;
            END (*IF*);
            IF UADpresent AND (UADend >= length) THEN
                length := UADend+1;
            END (*IF*);
            IF length > 0 THEN
                Strings.Delete (line, 0, length);
            END (*IF*);

            (* Remove unwanted punctuation around the results. *)

            StripChars (addr.PersonalName, CharSet{'(', ' ', '"'},
                                           CharSet{')', ' ', '"'});
            StripChars (UAD,  CharSet{'<', ' '},  CharSet{'>', ' '});

            (* Finally, split UAD into user@domain.     *)

            addr.MailboxName := UAD;
            addr.HostName := UAD;
            Strings.FindNext ('@', UAD, 0, foundat, posat);
            IF foundat THEN
                addr.MailboxName[posat] := Nul;
                Strings.Delete (addr.HostName, 0, posat+1);
            ELSE
                addr.HostName[0] := Nul;
            END (*IF*);

        END ParseAddress;

    (********************************************************************)

    PROCEDURE ParseAddressList(): EmailAddressList;

        (* Parses 'line' to extract a comma-separated list of addresses. *)

        VAR result, tail: EmailAddressList;

        BEGIN
            NEW (result);
            tail := result;
            result^.next := NIL;
            ParseAddress (result^.this);
            DeleteLeading (0);
            WHILE line[0] = ',' DO
                DeleteLeading (1);
                IF line[0] = Nul THEN
                    TSReadLine (TS, line);
                END (*IF*);
                NEW (tail^.next);
                tail := tail^.next;
                tail^.next := NIL;
                ParseAddress (tail^.this);
            END (*WHILE*);
            RETURN result;
        END ParseAddressList;

    (********************************************************************)

    PROCEDURE CopyAddressList (arg: EmailAddressList): EmailAddressList;

        (* Produces a copy of arg. *)

        VAR result, tail: EmailAddressList;

        BEGIN
            IF arg = NIL THEN
                result := NIL;
            ELSE
                NEW (result);
                result^.this := arg^.this;
                tail := result;
                tail^.next := NIL;
                arg := arg^.next;
                WHILE arg <> NIL DO
                    NEW (tail^.next);
                    tail := tail^.next;
                    tail^.next := NIL;
                    tail^.this := arg^.this;
                    arg := arg^.next;
                END (*WHILE*);
            END (*IF*);
            RETURN result;
        END CopyAddressList;

    (********************************************************************)

    VAR InHeader: BOOLEAN;
        result: EnvelopePtr;

    BEGIN
        TSSetPosition (TS, startpos);
        NEW (result);
        WITH result^ DO
            date[0] := Nul;
            subject[0] := Nul;
            result^.from := NIL;
            result^.sender := NIL;
            result^.reply_to := NIL;
            result^.to := NIL;
            result^.cc := NIL;
            result^.bcc := NIL;
            in_reply_to[0] := Nul;
            message_id[0] := Nul;
        END (*WITH*);

        InHeader := TRUE;
        WHILE InHeader DO
            TSReadLine (TS, line);
            IF (line[0] = CtrlZ) OR (line[0] = Nul) THEN
                InHeader := FALSE;
            ELSIF line[0] = ' ' THEN
                (* Ignore continuation line. *)
            ELSIF HeaderMatch (line, "Date") THEN
                DeleteLeading (5);
                Strings.Assign (line, result^.date);
            ELSIF HeaderMatch (line, "From") THEN
                DeleteLeading (5);
                result^.from := ParseAddressList();
            ELSIF HeaderMatch (line, "Sender") THEN
                DeleteLeading (7);
                result^.sender := ParseAddressList();
            ELSIF HeaderMatch (line, "Reply-To") THEN
                DeleteLeading (9);
                result^.reply_to := ParseAddressList();
            ELSIF HeaderMatch (line, "To") THEN
                DeleteLeading (3);
                result^.to := ParseAddressList();
            ELSIF HeaderMatch (line, "Cc") THEN
                DeleteLeading (3);
                result^.cc := ParseAddressList();
            ELSIF HeaderMatch (line, "Bcc") THEN
                DeleteLeading (4);
                result^.bcc := ParseAddressList();
            ELSIF HeaderMatch (line, "Subject") THEN
                DeleteLeading (8);
                Strings.Assign (line, result^.subject);
            ELSIF HeaderMatch (line, "In-Reply-To") THEN
                DeleteLeading (12);
                Strings.Assign (line, result^.in_reply_to);
            ELSIF HeaderMatch (line, "Message-ID") THEN
                DeleteLeading (11);
                Strings.Assign (line, result^.message_id);
            END (*IF*);
        END (*WHILE*);

        (* The standard says that the reply_to and sender       *)
        (* fields should default to the from address if they    *)
        (* are missing, and that the IMAP server has the        *)
        (* responsibility to set this default.                  *)

        IF result^.reply_to = NIL THEN
            result^.reply_to := CopyAddressList (result^.from);
        END (*IF*);
        IF result^.sender = NIL THEN
            result^.sender := CopyAddressList (result^.from);
        END (*IF*);

        RETURN result;

    END ParseEnvelope;

(************************************************************************)

PROCEDURE AppendEnvelope (EnvData: EnvelopePtr;
                               VAR (*INOUT*) buffer: ARRAY OF CHAR);

    (* Appends the ENVELOPE information to buffer. *)

    (********************************************************************)

    PROCEDURE PutField (VAR (*IN*) value: ARRAY OF CHAR;  NILoption: BOOLEAN);

        BEGIN
            IF NILoption AND (value[0] = Nul) THEN
                Strings.Append ('NIL', buffer);
            ELSE
                Strings.Append ('"', buffer);
                Strings.Append (value, buffer);
                Strings.Append ('"', buffer);
            END (*IF*);
        END PutField;

    (********************************************************************)

    PROCEDURE PutAddr (VAR (*IN*) addr: EmailAddress);

        BEGIN
            Strings.Append ('(', buffer);
            PutField (addr.PersonalName, TRUE);
            Strings.Append (' ', buffer);
            PutField (addr.AtDomainList, TRUE);
            Strings.Append (' ', buffer);
            PutField (addr.MailboxName, TRUE);
            Strings.Append (' ', buffer);
            PutField (addr.HostName, TRUE);
            Strings.Append (')', buffer);
        END PutAddr;

    (********************************************************************)

    PROCEDURE PutAddrList (VAR (*IN*) list: EmailAddressList);

        BEGIN
            Strings.Append ('(', buffer);
            IF list = NIL THEN
                Strings.Append ('(NIL NIL NIL NIL)', buffer);
            ELSE
                WHILE list <> NIL DO
                   PutAddr (list^.this);
                   list := list^.next;
                   IF list <> NIL THEN
                       Strings.Append (' ', buffer);
                   END (*IF*);
                END (*WHILE*);
            END (*IF*);
            Strings.Append (')', buffer);
        END PutAddrList;

    (********************************************************************)

    BEGIN
        IF EnvData = NIL THEN
            Strings.Append ('NIL', buffer);
        ELSE
            Strings.Append ('(', buffer);
            PutField (EnvData^.date, TRUE);
            Strings.Append (' ', buffer);
            PutField (EnvData^.subject, TRUE);
            Strings.Append (' ', buffer);
            PutAddrList (EnvData^.from);
            Strings.Append (' ', buffer);
            PutAddrList (EnvData^.sender);
            Strings.Append (' ', buffer);
            PutAddrList (EnvData^.reply_to);
            Strings.Append (' ', buffer);
            PutAddrList (EnvData^.to);
            Strings.Append (' ', buffer);
            PutAddrList (EnvData^.cc);
            Strings.Append (' ', buffer);
            PutAddrList (EnvData^.bcc);
            Strings.Append (' ', buffer);
            PutField (EnvData^.in_reply_to, TRUE);
            Strings.Append (' ', buffer);
            PutField (EnvData^.message_id, TRUE);
            Strings.Append (')', buffer);
            (*DISPOSE (EnvData);*)
        END (*IF*);
    END AppendEnvelope;

(************************************************************************)

<* IF DUMPSD THEN *>

PROCEDURE DumpEnvelope (cid: ChanId;  env: EnvelopePtr);

    (* For use in debugging.  Writes the envelope to a file. *)

    VAR buffer: ARRAY [0..4095] OF CHAR;

    BEGIN
        AppendEnvelope (env, buffer);
        WriteString (cid, buffer);
    END DumpEnvelope;

<* END *>

(************************************************************************)

PROCEDURE ReturnEnvelope (VAR (*IN*) file: FilenameString;
                               VAR (*INOUT*) buffer: ARRAY OF CHAR);

    (* Appends the ENVELOPE information to buffer.  Note that this is   *)
    (* for the top-level information for the entire message, and can    *)
    (* be worked out without working out the MIME structure of the      *)
    (* message.  An ENVELOPE request never asks for the envelope of an  *)
    (* embedded rfc-822 part.  That embedded data is, however, needed   *)
    (* for a FETCH that asks for BODY or BODYSTRUCTURE information.     *)

    VAR EnvData: EnvelopePtr;
        TS: TextStream;

    BEGIN
        TS := OpenForReading (file);
        EnvData := ParseEnvelope(TS, TSStartPosition(TS));
        CloseTS (TS);
        AppendEnvelope (EnvData, buffer);
        DiscardEnvelope (EnvData);
    END ReturnEnvelope;

(************************************************************************)
(*                               BODY INFO                              *)
(*                                                                      *)
(*   A BodyInfo structure records what the client is requesting in      *)
(*   terms of body parts.  To satisfy the request we first parse the    *)
(*   request to get a BodyInfo object, then parse the message to get    *)
(*   a StructureData object; and then we match up the two to work       *)
(*   out what to send back.                                             *)
(*                                                                      *)
(************************************************************************)

PROCEDURE InitBodyInfo (VAR (*OUT*) BI: BodyInfo);

    (* Sets BI to an initially clear state. *)

    BEGIN
        BI := NIL;
    END InitBodyInfo;

(************************************************************************)

PROCEDURE DiscardParamList (VAR (*INOUT*) PL: ParamList);

    (* Disposes of a parameter list. *)

    VAR this: ParamList;

    BEGIN
        WHILE PL <> NIL DO
            this := PL;
            PL := PL^.next;
            DISPOSE (this);
        END (*WHILE*);
    END DiscardParamList;

(************************************************************************)

PROCEDURE DiscardBodyInfo (VAR (*INOUT*) BI: BodyInfo);

    (* Disposes of a BodyInfo structure. *)

    BEGIN
        IF BI <> NIL THEN
            DiscardParamList (BI^.Headers);
            DISPOSE (BI);
        END (*WHILE*);
    END DiscardBodyInfo;

(************************************************************************)

PROCEDURE ParseFetchSection (VAR (*IN*) params: ARRAY OF CHAR;
                             VAR (*OUT*) result: BodyInfo);

    (* Parses the BODY[<section>] (with an optional <<partial>>) part   *)
    (* of the FETCH command parameters.  On entry the caller has        *)
    (* already consumed the leading '['.  Note: this is the command     *)
    (* parsing.  The actual fetching is handled elsewhere.  Here we're  *)
    (* simply figuring out what we're supposed to fetch.                *)

    (********************************************************************)

    PROCEDURE ParseHeaderList;

        (* Puts a list of header names into result^.Headers.  *)

        VAR prev, this: ParamList;
            thisheader: Param;

        BEGIN
            WHILE SPACE(params) DO
                (* skip space characters *)
            END (*WHILE*);
            prev := NIL;  result^.Headers := NIL;
            IF Match (params, "(") THEN
                WHILE (params[0] <> Nul) AND (params[0] <> ")")
                               AND AString (params, thisheader) DO
                    NEW (this);
                    this^.next := NIL;
                    this^.text := thisheader;
                    IF prev = NIL THEN
                        result^.Headers := this;
                    ELSE
                        prev^.next := this;
                    END (*IF*);
                    prev := this;
                    WHILE SPACE(params) DO
                        (* skip space characters *)
                    END (*WHILE*);
                END (*WHILE*);
                EVAL (Match (params, ")"));
            END (*IF*);
        END ParseHeaderList;

    (********************************************************************)

    TYPE CharSet = SET OF CHAR;
    CONST ExtDigits = CharSet {'0'..'9', '.'};

    VAR k: CARDINAL;  ch: CHAR;

    BEGIN
        NEW (result);
        WITH result^ DO

            (* A part specification has the form N.N.N.part, where N    *)
            (* is a number (there can be more or less than three of     *)
            (* them, and part is a string like "MIME" or "HEADER" (see  *)
            (* below for the possibilities).  Here we put the N.N.N bit *)
            (* in section, and a code for the final string in parts.    *)
            (* It is legal to have a null string for section.           *)

            (* We don't bother reporting excess periods, although that  *)
            (* is technically a client syntax error.  We simply discard *)
            (* them silently, so that software elswhere in this module  *)
            (* can treat a period is section as an unambiguous "down    *)
            (* one level" specification.                                *)

            k := 0;
            ch := params[0];
            WHILE ch IN ExtDigits DO
                section[k] := ch;
                INC (k);
                WHILE (ch = '.') AND (params[k] = '.') DO
                    (* Delete excess periods. *)
                    INC (k);
                END (*WHILE*);
                ch := params[k];
            END (*IF*);
            section[k] := Nul;
            IF k > 0 THEN
                Strings.Delete (params, 0, k);
                IF section[k-1] = '.' THEN
                    (* Delete trailing period. *)
                    section[k-1] := Nul;
                END (*IF*);
            END (*IF*);

            result^.Headers := NIL;
            IF Match (params, "HEADER.FIELDS.NOT") THEN
                part := HEADER_FIELDS_NOT;
                INCL (parts, part);
                ParseHeaderList;
            ELSIF Match (params, "HEADER.FIELDS") THEN
                part := HEADER_FIELDS;
                INCL (parts, part);
                ParseHeaderList;
            ELSIF Match (params, "HEADER") THEN
                part := HEADER;
                INCL (parts, part);
                EXCL (parts, HEADER_FIELDS);
                EXCL (parts, HEADER_FIELDS_NOT);
            ELSIF Match (params, "MIME") THEN
                part := MIME;
                INCL (parts, part);
            ELSIF Match (params, "TEXT") THEN
                part := TEXT;
                INCL (parts, part);
            ELSE

                (* Empty section specification implies the entire   *)
                (* message if this part is a message, or all except *)
                (* the MIME header if this part is a part of a      *)
                (* multipart message.                               *)

                part := ALL;
                INCL (parts, part);

            END (*IF*);

            (* Consume the closing bracket, and then check for the  *)
            (* <partial> option.                                    *)

            EVAL (Match (params, ']'));
            IF Match (params, '<') THEN
                partial := TRUE;
                EVAL (Number (params, initialoffset));
                EVAL (Match (params, '.'));
                EVAL (Number (params, amount));
                EVAL (Match (params, '>'));
            ELSE
                initialoffset := 0;
                amount := MAX(CARDINAL);
            END (*IF*);

        END (*WITH*);

    END ParseFetchSection;

(************************************************************************)
(*                             BODY STRUCTURE                           *)
(************************************************************************)

TYPE
    String32 = ARRAY [0..31] OF CHAR;

    ParamPtr = POINTER TO
                   RECORD
                       next: ParamPtr;
                       attribute, value: ARRAY [0..63] OF CHAR;
                   END (*RECORD*);

    (* The BodyLineCount is needed only for bodies of type TEXT.        *)
    (* The TotalLineCount is not always needed, but it must be correct  *)
    (* for all components because it will be needed if we are inside    *)
    (* a body of type MESSAGE/RFC822.                                   *)
    (* The BodyByteCount and TotalByteCount are potentially needed for  *)
    (* any part, depending on what the client wants to fetch.           *)

    (* The StructureData describes a message or section of message      *)
    (* consisting of a header and a body.  (For the purpose of these    *)
    (* calculations, the blank line before the body is counted as part  *)
    (* of the header.)  For a nested component this header is a MIME    *)
    (* header, and otherwise it is a normal message header.  We         *)
    (* distinguish these cases by the Boolean flags in the record.      *)
    (* Nested components of a multipart message have their own data     *)
    (* recorded at the next level down of the tree, in "parts".         *)

    (* A MESSAGE/RFC822 nested component is a special case.  It has a   *)
    (* MIME header and also a normal message header.  To deal with this *)
    (* we store the information in two tree levels.  The first level    *)
    (* has the MIME header (but also has byte counts that are the sums  *)
    (* for the whole message).  The next level down has the actual      *)
    (* message header and message body.  We just have to remember that  *)
    (* those two levels are a single level as far as section numbering  *)
    (* is concerned.  It's possible, of course, that the body is        *)
    (* multipart, in which case we will have at least one further level *)
    (* as is normal for a multipart message.                            *)

    (* Logically, we should have two separate data types, one for the   *)
    (* overall message or an embedded MESSAGE/RFC822 message, and one   *)
    (* for one part of a multipart message.  So far I have been getting *)
    (* by with one record type to cover both cases, but this does       *)
    (* require the trick of recording a MESSAGE/RFC822 part in two tree *)
    (* levels, so that we can keep both the MIME headers and the        *)
    (* message headers.  I have not yet decided whether we could get    *)
    (* a more elegant solution by separating the two cases.  A lot      *)
    (* depends on whether I am correctly distinguishing between         *)
    (* part.MIME and part.HEADER in the FETCH responses.                *)

    StructurePtr = StructureData;

    StructureData = POINTER TO
                        RECORD
                            next: StructurePtr;
                            headerstart, bodystart: FilePos;
                            BodyByteCount, BodyLineCount: CARDINAL;
                            TotalByteCount, TotalLineCount: CARDINAL;
                            ismessage, multipart, is822part: BOOLEAN;
                            NoMore: BOOLEAN;
                            parts: StructurePtr;
                            envelope: EnvelopePtr;
                            firstparam: ParamPtr;

                            Content_type,
                            Content_subtype,
                            Content_ID,
                            Content_description,
                            Content_transfer_encoding: String32;
                        END (*RECORD*);

    (* The possible cases are a lot more restricted than is implied     *)
    (* by the above declarations.  We have the following cases.         *)
    (*                                                                  *)
    (* MESSAGE/RFC822 (which includes the case of the total message).   *)
    (*    starts with header lines (always), then a blank line, then    *)
    (*    the body.  The body may have substructure, depending on what  *)
    (*    is specified in the Content-Type header line.                 *)
    (*                                                                  *)
    (* multipart body                                                   *)
    (*    a sequence of components with no header.  All of the          *)
    (*    structure is in the parts; the only text that belongs to the  *)
    (*    body rather than the parts are the separators, and the short  *)
    (*    filler that can come between the message header and the first *)
    (*    boundary code.                                                *)
    (*                                                                  *)
    (* multipart component                                              *)
    (*    starts with MIME header lines (which could, I suppose, be     *)
    (*    also counted as header lines), then a blank line, then the    *)
    (*    body.  The structure of the body is specified by the MIME     *)
    (*    headers.                                                      *)
    (*                                                                  *)
    (* simple body                                                      *)
    (*    plain text with no substructure.                              *)

(************************************************************************)
(*                       DUMPING THE BODY STRUCTURE                     *)
(************************************************************************)

<* IF DUMPSD THEN *>

PROCEDURE DumpParams (cid: ChanId;  p: ParamPtr);

    BEGIN
        WriteChar (cid, '(');
        WHILE p <> NIL DO
            WriteString (cid, p^.attribute);
            WriteChar (cid, ' ');
            WriteString (cid, p^.value);
            p := p^.next;
            IF p <> NIL THEN
                WriteChar (cid, ' ');
            END (*IF*);
        END (*WHILE*);
        WriteChar (cid, ')');
    END DumpParams;

(************************************************************************)

PROCEDURE WrHex (cid: ChanId;  val, ndigits: CARDINAL);

    BEGIN
        IF ndigits > 1 THEN
            WrHex (cid, val DIV 16, ndigits-1);
            val := val MOD 16;
        END (*IF*);
        IF val < 10 THEN
            WriteChar (cid, CHR(ORD('0') + val));
        ELSE
            WriteChar (cid, CHR(ORD('A') + val - 10));
        END (*IF*);
    END WrHex;

(************************************************************************)

PROCEDURE DumpHex64 (cid: ChanId;  val: CARD64);

    BEGIN
        WrHex (cid, val.high, 4);  WrHex (cid, val.low, 4);
    END DumpHex64;

(********************************************************************)

PROCEDURE WrCard (cid: ChanId;  val: CARDINAL);

    BEGIN
        IF val > 9 THEN
            WrCard (cid, val DIV 10);
            val := val MOD 10;
        END (*IF*);
        WriteChar (cid, CHR(ORD('0')+val));
    END WrCard;

(************************************************************************)

PROCEDURE WrBool (cid: ChanId;  val: BOOLEAN);

    BEGIN
        IF val THEN
            WriteString (cid, "TRUE");
        ELSE
            WriteString (cid, "FALSE");
        END (*IF*);
    END WrBool;

(************************************************************************)

PROCEDURE Indent (cid: ChanId;  level: CARDINAL);

    VAR j: CARDINAL;

    BEGIN
        FOR j := 1 TO 4*level DO
            WriteChar (cid, ' ');
        END (*FOR*);
    END Indent;

(************************************************************************)

PROCEDURE WrLabel (cid: ChanId;  label: ARRAY OF CHAR;  level: CARDINAL);

    (* Writes the label a file that the caller has already opened. *)

    BEGIN
        Indent(cid, level);
        WriteString (cid, label);  WriteString (cid, ": ");
    END WrLabel;

(************************************************************************)

PROCEDURE AppendCard (N: CARDINAL;  VAR (*INOUT*) str: ARRAY OF CHAR);

    VAR strN: ARRAY [0..0] OF CHAR;

    BEGIN
        IF N > 9 THEN
            AppendCard (N DIV 10, str);
            N := N MOD 10;
        END (*IF*);
        strN[0] := CHR(ORD('0')+N);
        Strings.Append (strN, str);
    END AppendCard;

(************************************************************************)

PROCEDURE DumpSD (cid: ChanId;  SD: StructureData;  level: CARDINAL;
                  partnumprefix: ARRAY OF CHAR;  partnum: CARDINAL);

    (* Writes result to a file that the caller has already opened. *)

    VAR partnumlabel: ARRAY [0..63] OF CHAR;
        p: StructurePtr;

    BEGIN
        IF partnum = 0 THEN
            partnumlabel := "";
        ELSE
            Strings.Assign (partnumprefix, partnumlabel);
            IF partnumprefix[0] <> Nul THEN
                Strings.Append (".", partnumlabel);
            END (*IF*);
            AppendCard (partnum, partnumlabel);
            Indent(cid, level);
            WriteString (cid, "PART ");
            WriteString (cid, partnumlabel);
            WriteLn (cid);
        END (*IF*);

        WrLabel (cid, "Content_type", level);
        WriteString (cid, SD^.Content_type);  WriteChar(cid, '/');
        WriteString (cid, SD^.Content_subtype);
        WrLabel (cid, "        Content_ID", level);
        WriteString (cid, SD^.Content_ID);  WriteLn(cid);
        WrLabel (cid, "Content_description", level);
        WriteString (cid, SD^.Content_description);
        WrLabel (cid, "        Content_transfer_encoding", level);
        WriteString (cid, SD^.Content_transfer_encoding);  WriteLn(cid);

        WrLabel (cid, "headerstart", level);
        DumpHex64 (cid, SD^.headerstart);
        WrLabel (cid, "      bodystart", level);
        DumpHex64 (cid, SD^.bodystart);  WriteLn(cid);
        WrLabel (cid, "BodyByteCount", level);
        WrCard (cid, SD^.BodyByteCount);
        WrLabel (cid, "        BodyLineCount", level);
        WrCard (cid, SD^.BodyLineCount);  WriteLn(cid);
        WrLabel (cid, "TotalByteCount", level);
        WrCard (cid, SD^.TotalByteCount);
        WrLabel (cid, "       TotalLineCount", level);
        WrCard (cid, SD^.TotalLineCount);  WriteLn(cid);
        WrLabel (cid, "ismessage", level);
        WrBool (cid, SD^.ismessage);
        WrLabel (cid, "  multipart", level);
        WrBool (cid, SD^.multipart);
        WrLabel (cid, "  is822part", level);
        WrBool (cid, SD^.is822part);
        WrLabel (cid, "  NoMore", level);
        WrBool (cid, SD^.NoMore);  WriteLn (cid);

        WrLabel (cid, "envelope", level);
        DumpEnvelope (cid, SD^.envelope);  WriteLn(cid);
        WrLabel (cid, "params", level);
        DumpParams (cid, SD^.firstparam);  WriteLn(cid);

        (* Now the nested parts. *)

        p := SD^.parts;
        IF SD^.is822part THEN
            Indent (cid, level);
            WriteString(cid, "<The rfc-822 message>");  WriteLn(cid);
            DumpSD (cid, p, level, partnumlabel, 0);
        ELSE
            partnum := 1;  INC(level);
            WHILE p <> NIL DO
                DumpSD (cid, p, level, partnumlabel, partnum);
                p := p^.next;
                INC (partnum);
            END (*WHILE*);
        END (*IF*);

    END DumpSD;

(************************************************************************)

PROCEDURE DumpStructureData (SD: StructureData);

    (* Writes result to file SDout.txt. *)

    CONST fname = "mailroot\imaptest\SDout.txt";

    VAR cid: ChanId;
        res: OpenResults;
        dummy: BOOLEAN;

    BEGIN
        IF FileSys.Exists(fname) THEN
            FileSys.Remove(fname, dummy);
        END (*IF*);
        OpenWrite (cid, fname, text+write, res);
        DumpSD (cid, SD, 0, "", 0);
        Close (cid);
    END DumpStructureData;

<* END *>

(************************************************************************)
(*                     WORKING OUT THE BODY STRUCTURE                   *)
(************************************************************************)

PROCEDURE ParseToken (VAR (*INOUT*) line: ARRAY OF CHAR;
                      VAR (*OUT*) token: ARRAY OF CHAR);

    CONST tspecials =  CharSet{Nul, "(", ")", "<", ">", "@",
                               ",", ";", ":", "\", '"',
                               "/", "[", "]", "?", "="};

    VAR j: CARDINAL;

    BEGIN
        j := 0;
        WHILE (j <= HIGH(line)) AND (j <= HIGH(token))
                   AND NOT (line[j] IN tspecials) DO
            token[j] := line[j];
            INC (j);
        END (*WHILE*);
        IF j <= HIGH(token) THEN
            token[j] := Nul;
        END (*IF*);
        IF j > 0 THEN
            Strings.Delete (line, 0, j);
        END (*IF*)
    END ParseToken;

(********************************************************************)

PROCEDURE ParseValue (VAR (*INOUT*) line: ARRAY OF CHAR;
                      VAR (*OUT*) value: ARRAY OF CHAR);

    (* A value is either a token or a quoted string. *)

    VAR pos: CARDINAL;  found: BOOLEAN;

    BEGIN
        IF line[0] = '"' THEN
            Strings.Delete (line, 0, 1);
            Strings.Assign (line, value);
            IF line[0] <> Nul THEN
                Strings.FindNext ('"', line, 0, found, pos);
                IF found THEN
                    IF pos <= HIGH(value) THEN
                        value[pos] := Nul;
                    END (*IF*);
                    Strings.Delete (line, 0, pos+1);
                ELSE
                    line[0] := Nul;
                END (*IF*);
            END (*IF*);
        ELSE
            ParseToken (line, value);
        END (*IF*);
    END ParseValue;

(********************************************************************)

PROCEDURE DeleteLeading (N: CARDINAL;  TS: TextStream;
                         VAR (*INOUT*) line: ARRAY OF CHAR;
                         VAR (*INOUT*) linecount, bytecount: CARDINAL);

    (* Deletes the leading N characters of line, plus any space or  *)
    (* tab characters that follow.  If this takes us off the end of *)
    (* the line, we read in a new line.                             *)

    VAR length: CARDINAL;

    BEGIN
        length := Strings.Length (line);
        LOOP
            WHILE (N < length)
                     AND ((line[N] = Space) OR (line[N] = Tab)) DO
                INC (N);
            END (*WHILE*);
            IF N = 0 THEN
                EXIT (*LOOP*);
            ELSE
                Strings.Delete (line, 0, N);
                DEC (length, N);
                N := 0;
                IF length = 0 THEN
                    TSReadLine (TS, line);
                    IF line[0] <> CtrlZ THEN
                        INC (linecount);
                        INC (bytecount, LENGTH(line) + 2);
                    END (*IF*);
                    length := Strings.Length (line);
                END (*IF*);
            END (*IF*);
        END (*LOOP*);
    END DeleteLeading;

(********************************************************************)

PROCEDURE DeconstructContentType (TS: TextStream;
                        VAR (*IN*) line: ARRAY OF CHAR;
                        VAR (*OUT*) type, subtype: String32;
                        VAR (*OUT*) params: ParamPtr;
                        VAR (*OUT*) subboundary: ARRAY OF CHAR;
                        VAR (*OUT*) linecount, bytecount: CARDINAL);

    (* At this stage we have found a 'Content-type' header line,    *)
    (* and have deleted the keyword and colon and leading spaces.   *)
    (* Now we analyse the rest of that line, including continuation *)
    (* lines if any.  This procedure does not overshoot to a line   *)
    (* that does not belong to us.  The output variables linecount  *)
    (* and bytecount give the amount of extra input we've read in.  *)
    (* They are returned as zero if we didn't have to get any       *)
    (* continuation lines.                                          *)

    VAR previous, p: ParamPtr;

    BEGIN
        linecount := 0;  bytecount := 0;
        params := NIL;  previous := NIL;
        subboundary[0] := CtrlZ;  subboundary[1] := Nul;
        ParseToken (line, type);
        IF line[0] = '/' THEN
            Strings.Delete (line, 0, 1);
            ParseToken (line, subtype);
        ELSE
            subtype := "";
        END (*IF*);
        DeleteLeading (0, TS, line, linecount, bytecount);
        WHILE line[0] = ';' DO
            DeleteLeading (1, TS, line, linecount, bytecount);
            NEW (p);
            p^.next := NIL;
            ParseToken (line, p^.attribute);
            IF line[0] = '=' THEN
                DeleteLeading (1, TS, line, linecount, bytecount);
                ParseValue (line, p^.value);
            ELSE
                p^.value[0] := Nul;
            END (*IF*);
            IF StringMatch(p^.attribute, "boundary") THEN
                Strings.Assign (p^.value, subboundary);
                Strings.Insert ("--", 0, subboundary);
            END (*IF*);
            IF params = NIL THEN
                params := p;
            ELSE
                previous^.next := p;
            END (*IF*);
            previous := p;
        END (*WHILE*);
    END DeconstructContentType;

(************************************************************************)

PROCEDURE AtBoundary (VAR (*IN*) line, boundary: ARRAY OF CHAR;
                            VAR (*OUT*) boundarysize: CARDINAL): CARDINAL;

    (* Checks line to see whether we are at end of file or end of       *)
    (* section.  The result codes are                                   *)
    (*     0    not at end of section                                   *)
    (*     1    line matches boundary, i.e. end of section              *)
    (*     2    end of file, or line matches boundary'--'               *)
    (* The value of  boundarysize is also set to match the size of the  *)
    (* boundary code, if present, including the delimiting line         *)
    (* terminators which are officially part of the boundary code.  If  *)
    (* we are not at a boundary then boundarysize is returned as 0.     *)

    VAR code, L, M: CARDINAL;
        copy: ARRAY [0..2047] OF CHAR;

    BEGIN
        boundarysize := 0;
        code := 0;
        IF line[0] = CtrlZ THEN
            code := 2;
        ELSE
            L := Strings.Length (line);
            M := Strings.Length (boundary);
            IF M = 0 THEN
                code := 0;
            ELSIF L = M THEN

                IF Strings.Equal (line, boundary) THEN
                    code := 1;
                    boundarysize := L+4;

                    (* Technically, the <CRLF> before the boundary      *)
                    (* code is part of the boundary, as is the <CRLF>   *)
                    (* that terminates the boundary code.  That is why  *)
                    (* we add 4 to the size.                            *)

                END (*IF*);

            ELSIF (L = M+2) AND (line[M] = '-') AND (line[M+1] = '-') THEN

                Strings.Assign (line, copy);
                copy[M] := Nul;
                IF Strings.Equal (copy, boundary) THEN
                    code := 2;
                    boundarysize := L+4;
                END (*IF*);

            END (*IF*);
        END (*IF*);
        RETURN code;
    END AtBoundary;

(************************************************************************)

PROCEDURE DiscardStructureParams (VAR (*INOUT*) q: ParamPtr);

    (* Disposes of the data structure. *)

    VAR qnext: ParamPtr;

    BEGIN
        WHILE q <> NIL DO
            qnext := q^.next;
            DISPOSE (q);
            q := qnext;
        END (*WHILE*);
    END DiscardStructureParams;

(************************************************************************)

PROCEDURE DiscardStructureData (VAR (*INOUT*) SD: StructureData);

    (* Disposes of the data structure. *)

    VAR p, next: StructureData;

    BEGIN
        IF SD <> NIL THEN
            DiscardStructureParams (SD^.firstparam);
            DiscardEnvelope (SD^.envelope);
            p := SD^.parts;
            WHILE p <> NIL DO
                next := p^.next;
                DiscardStructureData (p);
                p := next;
            END (*WHILE*);
            DISPOSE (SD);
        END (*IF*);
    END DiscardStructureData;

(************************************************************************)

PROCEDURE BuildSD (TS: TextStream;  VAR (*IN*) boundary: ARRAY OF CHAR;
                           VAR (*OUT*) boundarysize: CARDINAL;
                           TopLevel, IsAMessage: BOOLEAN): StructureData;

    (* This is the internal version of BuildStructureData.             *)
    (* Parses the headers of a message file and returns the structure. *)
    (* On exit, we have consumed the boundary code if any, and         *)
    (* boundarysize is the number of boundary code characters we have  *)
    (* actually consumed.  (For various reasons, the actual boundary   *)
    (* code can differ from that given in the 'boundary' parameter.)   *)

    (* Note: the boundary code actually belongs to the enclosing       *)
    (* section.  That means that, on exit, we have actually overshot   *)
    (* by 'boundarysize' characters in the text stream.  That doesn't  *)
    (* matter, because the caller expects us to overshoot.             *)

    (* TopLevel is TRUE iff this is the overall message, rather than    *)
    (* an embedded message part.  IsAMessage is TRUE iff this is either *)
    (* the top-level message or an embedded message/rfc822 part.        *)

    VAR linecount, bytecount, totallinecount, totalbytecount: CARDINAL;
        result: StructureData;
        line, subboundary: ARRAY [0..2047] OF CHAR;
        HaveLine: BOOLEAN;

        (* SPECIAL NOTE: The HaveLine flag.                             *)

        (* Because we don't always know whether a header line is        *)
        (* followed by a continuation line, we will sometimes be in the *)
        (* situation where we have read, but not consumed, a lookahead  *)
        (* line.  HaveLine = TRUE iff the 'line' array contains a line  *)
        (* that we haven't yet finished processing.  It must be changed *)
        (* to FALSE by any part of the code that knows that the         *)
        (* processing of that line is complete.                         *)

    (********************************************************************)

    PROCEDURE GetNextLine;

        BEGIN
            IF NOT HaveLine THEN
                TSReadLine (TS, line);
                IF line[0] <> CtrlZ THEN
                    INC (linecount);
                    INC (totallinecount);
                    INC (bytecount, LENGTH(line) + 2);
                    INC (totalbytecount, LENGTH(line) + 2);
                END (*IF*);
                HaveLine := TRUE;
            END (*IF*);
        END GetNextLine;

    (********************************************************************)

    PROCEDURE DeleteLeading (N: CARDINAL);

        (* Deletes the leading N characters of line, plus any space or  *)
        (* tab characters that follow.  If this takes us off the end of *)
        (* the line, we read in a new line.                             *)

        VAR length: CARDINAL;

        BEGIN
            length := Strings.Length (line);
            LOOP
                WHILE (N < length)
                         AND ((line[N] = Space) OR (line[N] = Tab)) DO
                    INC (N);
                END (*WHILE*);
                IF N = 0 THEN
                    EXIT (*LOOP*);
                ELSE
                    Strings.Delete (line, 0, N);
                    DEC (length, N);
                    N := 0;
                    IF length = 0 THEN
                        HaveLine := FALSE;
                        GetNextLine;
                        length := Strings.Length (line);
                    END (*IF*);
                END (*IF*);
            END (*LOOP*);
        END DeleteLeading;

    (********************************************************************)

    PROCEDURE ParseContentType;

        (* At this stage we have found a 'Content-type' header line,    *)
        (* and have deleted the keyword and colon and leading spaces.   *)
        (* Now we analyse the rest of that line, including continuation *)
        (* lines if any.  Does not overshoot to a new input line.       *)

        VAR extralines, extrabytes: CARDINAL;

        BEGIN
            DeconstructContentType (TS, line, result^.Content_type,
                                    result^.Content_subtype,
                                    result^.firstparam, subboundary,
                                    extralines, extrabytes);
            INC (linecount, extralines);
            INC (totallinecount, extralines);
            INC (bytecount, extrabytes);
            INC (totalbytecount, extrabytes);
            HaveLine := FALSE;
        END ParseContentType;

    (********************************************************************)

    PROCEDURE ProcessHeaderSection;

        (* Processes either a MIME header section or an RFC-2822 header *)
        (* section, pulling out the information we need.                *)
        (* We exit with HaveLine = FALSE.                               *)

        VAR InHeader: BOOLEAN;

        BEGIN
            InHeader := TRUE;
            HaveLine := FALSE;
            WHILE InHeader DO
                GetNextLine;
                IF (line[0] = CtrlZ) OR (line[0] = Nul) THEN

                    (* The blank line, if found, counts as part of the header. *)

                    InHeader := FALSE;

                ELSE
                    (* The header lines that interest us all start with *)
                    (* the same substring, so we can save some time by  *)
                    (* doing a preliminary check.                       *)

                    IF Match (line, "Content-") THEN
                        IF HeaderMatch (line, "type") THEN
                            DeleteLeading (5);
                            ParseContentType;
                        ELSIF HeaderMatch (line, "transfer-encoding") THEN
                            DeleteLeading (18);
                            ParseToken (line, result^.Content_transfer_encoding);
                        ELSIF HeaderMatch (line, "ID") THEN
                            DeleteLeading (3);
                            ParseToken (line, result^.Content_ID);
                        ELSIF HeaderMatch (line, "description") THEN
                            DeleteLeading (12);
                            ParseToken (line, result^.Content_description);
                        END (*IF*);
                    END (*IF*);
                END (*IF*);
                HaveLine := FALSE;
            END (*WHILE*);

            (* Insert defaults for missing fields. *)

            IF result^.Content_type[0] = Nul THEN
                result^.Content_type := "text";
                result^.Content_subtype := "plain";
            END (*IF*);
            IF result^.Content_transfer_encoding[0] = Nul THEN
                result^.Content_transfer_encoding := "7bit";
            END (*IF*);

            (* This next bit compensates for a missing charset spec.    *)

            IF result^.firstparam = NIL THEN
                NEW (result^.firstparam);
                WITH result^.firstparam^ DO
                    next := NIL;
                    attribute := "charset";
                    value := "us-ascii";
                END (*WITH*);
            END (*IF*);

        END ProcessHeaderSection;

    (********************************************************************)

    VAR endofsection: CARDINAL;
        tail, part: StructureData;

    BEGIN
        (* Initialise all fields of the result. *)

        NEW (result);
        WITH result^ DO
            next := NIL;  parts := NIL;
            headerstart := TSCurrentPosition (TS);
            bodystart := headerstart;
            firstparam := NIL;
            multipart := FALSE;  ismessage := IsAMessage;
            is822part := FALSE;  NoMore := FALSE;
            envelope := NIL;
            BodyByteCount := 0;  BodyLineCount := 0;
            TotalByteCount := 0;  TotalLineCount := 0;
            Content_type[0] := Nul;
            Content_subtype[0] := Nul;
            Content_ID[0] := Nul;
            Content_description[0] := Nul;
            Content_transfer_encoding[0] := Nul;
        END (*WITH*);
        subboundary[0] := CtrlZ;  subboundary[1] := Nul;

        (* For a message/rfc822 component, but apparently not for the   *)
        (* top level (the standard is ambiguous on this point), we have *)
        (* to construct the envelope.  We are now positioned at the     *)
        (* start of the header that contains both the envelope and      *)
        (* MIME header fields; but, to avoid making the code too        *)
        (* complex and obscure, I am choosing to parse only the         *)
        (* envelope fields here, and then back up in the file to allow  *)
        (* a second pass over the header.                               *)

        IF IsAMessage AND NOT TopLevel THEN
            result^.envelope := ParseEnvelope (TS, result^.headerstart);
            TSSetPosition (TS, result^.headerstart);
        END (*IF*);

        linecount := 0;  bytecount := 0;
        totallinecount := 0; totalbytecount := 0;

        (* Process the message header section, if we're at the top      *)
        (* level, or the MIME header section, if we're in a nested part.*)

        ProcessHeaderSection;

        (* We have now passed the header lines, and are looking at      *)
        (* the body.  Reset the counts.                                 *)

        result^.bodystart := TSCurrentPosition (TS);
        linecount := 0;  bytecount := 0;

        result^.multipart := StringMatch (result^.Content_type, "MULTIPART");
        IF result^.multipart THEN

            (* The boundary code we are now looking for is the one in   *)
            (* subboundary.  (It was set while processing the header.)  *)
            (* Compensate for missing subboundary code.                 *)

            IF subboundary[0] = CtrlZ THEN
                Strings.Assign (boundary, subboundary);
            END (*IF*);

            (* Lines between the header and the first boundary code     *)
            (* should be ignored.  Note that GetNextLine is still       *)
            (* accumulating the line and byte counts.  We count this    *)
            (* ignored part as part of the body of the current message, *)
            (* but of course it won't be counted in the sizes of the    *)
            (* parts.                                                   *)

            REPEAT
                GetNextLine;
                HaveLine := FALSE;
            UNTIL AtBoundary(line, subboundary, boundarysize) > 0;

            tail := NIL;
            REPEAT
                part := BuildSD (TS, subboundary, boundarysize, FALSE, FALSE);

                INC (linecount, part^.TotalLineCount);
                INC (totallinecount, part^.TotalLineCount);
                INC (bytecount, part^.TotalByteCount);
                INC (totalbytecount, part^.TotalByteCount);

                (* The value of boundarysize at this point is the size  *)
                (* of the boundary between parts; this boundary is      *)
                (* part of the multipart structure, so should be        *)
                (* counted in our line and byte counts.                 *)

                IF boundarysize > 0 THEN
                    INC (linecount);
                    INC (totallinecount);
                    INC (bytecount, boundarysize);
                    INC (totalbytecount, boundarysize);
                END (*IF*);

                IF tail = NIL THEN
                    result^.parts := part;
                ELSE
                    tail^.next := part;
                END (*IF*);
                tail := part;

            UNTIL part^.NoMore;

            (* Lines after the last boundary code should also be ignored. *)
            (* The boundary we are now looking for belongs to the         *)
            (* caller, so we'll have to subtract its size from our own    *)
            (* calculated size.  This is done further down below.         *)

            REPEAT
                GetNextLine;
                HaveLine := FALSE;
            UNTIL AtBoundary(line, boundary, boundarysize) > 0;

        ELSIF StringMatch (result^.Content_type, "MESSAGE")
                  AND StringMatch (result^.Content_subtype, "RFC822") THEN

            (* A message/rfc822 component has both a MIME header and    *)
            (* a message header.  I am choosing here to represent it    *)
            (* by two tree levels.  Our result^ record records the size *)
            (* and location of the MIME header, and result^.parts       *)
            (* is for the actual embedded message.  Note that the       *)
            (* message will not be followed by a nested boundary code;  *)
            (* it shares the boundary code we already have.  This is    *)
            (* why we copy the "NoMore" code up (see below).            *)

            part := BuildSD (TS, boundary, boundarysize, FALSE, TRUE);
            result^.is822part := TRUE;
            result^.parts := part;

            linecount := part^.TotalLineCount;
            INC (totallinecount, linecount);
            bytecount := part^.TotalByteCount;
            INC (totalbytecount, bytecount);

            IF boundarysize > 0 THEN
                INC (linecount);
                INC (totallinecount);
                INC (bytecount, boundarysize);
                INC (totalbytecount, boundarysize);
            END (*IF*);
            result^.NoMore := part^.NoMore;

        ELSE
            (* For the non-multipart case, all we have to do is count   *)
            (* the lines and characters.  Note that we have reset the   *)
            (* counts before entering this part of the code.            *)

            REPEAT
                GetNextLine;
                endofsection := AtBoundary (line, boundary, boundarysize);
                HaveLine := FALSE;
            UNTIL endofsection > 0;
            result^.NoMore := endofsection = 2;

        END (*IF*);

        (* Correct our size measurements for the final boundary *)
        (* overshoot (An end of file gives boundarysize = 0.)   *)

        IF boundarysize > 0 THEN
            DEC (linecount);
            DEC (totallinecount);
            DEC (bytecount, boundarysize);
            DEC (totalbytecount, boundarysize);
        END (*IF*);

        result^.BodyLineCount := linecount;
        result^.TotalLineCount := totallinecount;
        result^.BodyByteCount := bytecount;
        result^.TotalByteCount := totalbytecount;

        RETURN result;

    END BuildSD;

(************************************************************************)

PROCEDURE BuildStructureData (TS: TextStream;
                              VAR (*INOUT*) result: StructureData);

    (* Parses the headers of a message file and returns the structure. *)

    (* The result parameter is an INOUT parameter because I am         *)
    (* allowing for the possibility that the result has already been   *)
    (* worked out on a previous call.                                  *)

    VAR dummy: CARDINAL;  boundary: ARRAY [0..1] OF CHAR;

    BEGIN
        IF result = NIL THEN
            boundary[0] := CtrlZ;  boundary[1] := Nul;
            result := BuildSD (TS, boundary, dummy, TRUE, TRUE);
            <* IF DUMPSD THEN *>
               DumpStructureData (result);
            <* END *>
        END (*IF*);
    END BuildStructureData;

(************************************************************************)

PROCEDURE AppendBodyStructure (SD: StructureData;  RC: ReplyCxt;
                               VAR (*INOUT*) buffer: ARRAY OF CHAR);

    (* Appends the StructureData information to buffer.  The parameter  *)
    (* RC permits us to send partial data if buffer is about to overflow. *)

    (********************************************************************)

    PROCEDURE PutField (VAR (*IN*) value: ARRAY OF CHAR);

        BEGIN
            IF value[0] = Nul THEN
                Strings.Append ('NIL', buffer);
            ELSE
                Strings.Append ('"', buffer);
                Strings.Append (value, buffer);
                Strings.Append ('"', buffer);
            END (*IF*);
        END PutField;

    (********************************************************************)

    VAR tempbuf: ARRAY [0..31] OF CHAR;
        p: StructureData;
        q: ParamPtr;

    BEGIN
        IF SD = NIL THEN
            Strings.Append ('NIL', buffer);
        ELSE
            Strings.Append ('(', buffer);
            IF SD^.multipart THEN

                p := SD^.parts;
                WHILE p <> NIL DO
                    AppendBodyStructure (p, RC, buffer);
                    p := p^.next;
                END (*WHILE*);
                Strings.Append (' ', buffer);
                PutField (SD^.Content_subtype);

            ELSE

                (* body type *)

                PutField (SD^.Content_type);

                (* body subtype *)

                Strings.Append (' ', buffer);
                PutField (SD^.Content_subtype);

                (* body parameter parenthesized list *)

                Strings.Append (' (', buffer);
                q := SD^.firstparam;
                WHILE q <> NIL DO
                    PutField (q^.attribute);
                    Strings.Append (' ', buffer);
                    PutField (q^.value);
                    q := q^.next;
                    IF q <> NIL THEN
                        Strings.Append (' ', buffer);
                    END (*IF*);
                END (*WHILE*);
                Strings.Append (') ', buffer);

                (* body id *)

                PutField (SD^.Content_ID);

                (* body description *)

                Strings.Append (' ', buffer);
                PutField (SD^.Content_description);

                (* body encoding *)

                Strings.Append (' ', buffer);
                PutField (SD^.Content_transfer_encoding);

                (* body size *)

                Strings.Append (' ', buffer);
                CardinalToStringLJ (SD^.BodyByteCount, tempbuf);
                Strings.Append (tempbuf, buffer);

                (* Special cases: types TEXT and MESSAGE/RFC822 have some       *)
                (* extra fields beyond the basic ones above.                    *)

                IF StringMatch (SD^.Content_type, "TEXT") THEN

                    Strings.Append (' ', buffer);
                    CardinalToStringLJ (SD^.BodyLineCount, tempbuf);
                    Strings.Append (tempbuf, buffer);

                ELSIF StringMatch (SD^.Content_type, "MESSAGE")
                            AND StringMatch (SD^.Content_subtype, "RFC822") THEN

                    (* THIS CASE STILL NEEDS TO BE TESTED. *)
                    (*   A body type of type MESSAGE and subtype RFC822
                         contains, immediately after the basic fields, the
                         envelope structure, body structure, and size in
                         text lines of the encapsulated message.             *)

                    Strings.Append (' ', buffer);
                    AppendEnvelope (SD^.parts^.envelope, buffer);
                    PartialReply (RC, buffer);
                    buffer[0] := Nul;
                    Strings.Append (' ', buffer);
                    AppendBodyStructure (SD^.parts, RC, buffer);
                    PartialReply (RC, buffer);
                    buffer[0] := Nul;
                    Strings.Append (' ', buffer);
                    CardinalToStringLJ (SD^.parts^.TotalLineCount, tempbuf);
                    Strings.Append (tempbuf, buffer);

                END (*IF*);
            END (*IF*);

            Strings.Append (')', buffer);
        END (*IF*);

    END AppendBodyStructure;

(************************************************************************)

PROCEDURE ReportBodyStructure (VAR (*IN*) file: FilenameString;
                               RC: ReplyCxt;
                               VAR (*INOUT*) buffer: ARRAY OF CHAR;
                               VAR (*INOUT*) SD: StructureData);

    (* Creates the BODYSTRUCTURE reply for a fetch, and appends *)
    (* it to buffer.  The RC parameter permits us to send a     *)
    (* partial reply when the buffer is about to overflow.      *)
    (* The SD parameter allows us to work out the structure     *)
    (* once and preserve the answer over several calls, but in  *)
    (* the present version we destroy the structure data after  *)
    (* using it as a guard against memory leaks.                *)

    VAR TS: TextStream;

    BEGIN
        TS := OpenForReading (file);
        BuildStructureData (TS, SD);
        CloseTS (TS);
        AppendBodyStructure (SD, RC, buffer);
        DiscardStructureData (SD);
    END ReportBodyStructure;

(************************************************************************)
(*                   EXTRACTING SECTIONS OF A MESSAGE                   *)
(************************************************************************)

PROCEDURE SkipToSection (SD: StructureData;
                                   section: ARRAY OF CHAR): StructureData;

    (* The section number is either an empty string or a sequence of    *)
    (* numbers separated by dots, in the form 'section.subsection.etc'. *)
    (* We advance through SD until we come to the desired section.      *)
    (* On exit, the result points to a substructure of the original SD, *)
    (* depending on the "section" parameter.  We return NIL for a       *)
    (* nonexistent section.                                             *)

    VAR sectionnum, k: CARDINAL;  result: StructureData;

    BEGIN
        result := SD;
        WHILE (result <> NIL) AND (section[0] <> Nul) DO
            sectionnum := 0;  k := 0;
            WHILE section[k] IN Digits DO
                sectionnum := 10*sectionnum + (ORD(section[k]) - ORD('0'));
                INC (k);
            END (*WHILE*);
            IF k > 0 THEN
                IF section[k] = '.' THEN
                    INC (k);
                END (*IF*);
                Strings.Delete (section, 0, k);
            END (*IF*);

            (* Move to the desired section.  Note: if we're not now in  *)
            (* a multipart component then we must already be in the     *)
            (* right section, because there aren't any other sections.  *)

            IF result^.multipart THEN
                result := result^.parts;
                WHILE sectionnum > 1 DO
                    IF result <> NIL THEN
                        result := result^.next;
                    END (*IF*);
                    DEC (sectionnum);
                END (*WHILE*);
            END (*IF*);

            (* If we are now at a '.' we have to go down a level. This  *)
            (* is possible only if we are now in a MESSAGE/RFC822 part. *)
            (* We store the structure data in such a way that one       *)
            (* level down in the numbering means two levels down in     *)
            (* the tree.                                                *)

            IF section[0] = '.' THEN
                Strings.Delete (section, 0, 1);
                IF result <> NIL THEN
                    result := result^.parts;
                END (*IF*);
                IF result <> NIL THEN
                    result := result^.parts;
                END (*IF*);
            END (*IF*);

        END (*WHILE*);

        RETURN result;

    END SkipToSection;

(************************************************************************)

PROCEDURE SendHeaderFields (info: BodyInfo;  boundary: ARRAY OF CHAR;
                               TS: TextStream;
                               offset, limit: CARDINAL;
                               RC: ReplyCxt; sendthem: BOOLEAN): CARDINAL;

    (* This function is called only when info^.part is HEADER_FIELDS    *)
    (* or HEADER_FIELDS_NOT.  If sendthem is FALSE, returns the number  *)
    (* of characters that should be sent.  IF sendthem is TRUE, sends   *)
    (* those characters.  We assume that the caller has already set     *)
    (* the text stream position to the start of the header.             *)

    CONST buffersize = 2048;

    VAR size, boundarysize, donecode, excess, L: CARDINAL;
        InHeader, PastOffset, IncludeIt: BOOLEAN;
        line: ARRAY [0..buffersize-1] OF CHAR;

    BEGIN
        size := 0;
        InHeader := TRUE;  IncludeIt := FALSE;
        PastOffset := offset = 0;
        WHILE InHeader DO
            TSReadLine (TS, line);
            donecode := AtBoundary (line, boundary, boundarysize);
            IF donecode > 0 THEN
                InHeader := FALSE;
            ELSE
                InHeader := line[0] <> Nul;
                IF NOT InHeader THEN
                    INC (size, 2);
                    IncludeIt := FALSE;
                    IF sendthem THEN
                        Reply (RC, "");
                    END (*IF*);

                (* We deal with continuation lines by simply leaving    *)
                (* the IncludeIt flag at its previous value.            *)

                ELSIF (line[0] <> Space) AND (line[0] <> Tab) THEN

                    IncludeIt := FALSE;
                    IF (info^.part = HEADER) OR (info^.part = ALL) THEN
                        IncludeIt := TRUE;
                    ELSIF info^.part = HEADER_FIELDS THEN
                        IncludeIt := HeaderListMatch (line, info^.Headers);
                    ELSIF info^.part = HEADER_FIELDS_NOT THEN
                        IncludeIt := NOT HeaderListMatch (line, info^.Headers);
                    END (*IF*);
                END (*IF*);

                IF IncludeIt THEN
                    L := Strings.Length(line);
                    INC (size, L + 2);
                    IF PastOffset THEN
                        IF size > limit THEN
                            excess := size - limit;
                            IF excess <= L THEN
                                line[L-excess] := Nul;
                                excess := 0;
                            ELSE
                                line[0] := Nul;
                            END (*IF*);
                            size := limit;
                            InHeader := FALSE;
                        ELSE
                            excess := 0;
                        END (*IF*);
                        IF sendthem AND (excess = 0) THEN
                            Reply (RC, line);
                        END (*IF*);
                    ELSE
                        (* We're still looking for the starting point. *)

                        IF size >= offset THEN

                            (* Restart the size calculation. *)

                            PastOffset := TRUE;
                            IF sendthem AND (size >= offset+2) THEN
                                Strings.Delete (line, 0, size-offset-2);
                                Reply (RC, line);
                            END (*IF*);
                            DEC (size, offset);
                        END (*IF*);

                    END (*IF*);
                END (*IF*);

            END (*IF*);
        END (*WHILE*);

        RETURN size;

    END SendHeaderFields;

(************************************************************************)

PROCEDURE SendFragmentedFilePart (RC: ReplyCxt;  TS0: TextStream;
                                   info: BodyInfo;
                                      SD: StructureData);

    (* Sends to the output channel that contains the subset of TS0      *)
    (* specified by the 'info' parameter.  This procedure is invoked    *)
    (* only in the special case where info^.part is equal to either     *)
    (* HEADER_FIELDS or HEADER_FIELDS_NOT.  These two cases require     *)
    (* special treatment because what we are going to send is not       *)
    (* necessarily a contiguous part of the file, and two passes are    *)
    (* needed to do the job properly.                                   *)

    CONST buffersize = 2048;

    VAR bytestosend: CARDINAL;   start: StructureData;
        sizebuff: ARRAY [0..15] OF CHAR;
        AtHeader: BOOLEAN;
        boundary: ARRAY [0..buffersize-1] OF CHAR;

    BEGIN
        (* Locate the beginning of the desired section. *)

        boundary[0] := CtrlZ;  boundary[1] := Nul;
        start := SkipToSection (SD, info^.section);
        AtHeader := (SD <> NIL) AND (SD^.ismessage);

        (* At this stage SD^.headerstart says where the desired section *)
        (* starts, and SD^.BodyByteCount and SD^.TotalByteCount give    *)
        (* the size of the body and the complete part, as appropriate.  *)
        (* Do an initial pass over the header to work out the size of   *)
        (* the requested information.                                   *)

        IF AtHeader THEN
            TSSetPosition (TS0, SD^.headerstart);
            bytestosend := SendHeaderFields (info, boundary, TS0,
                                       info^.initialoffset, info^.amount,
                                       RC, FALSE);
        ELSE
            bytestosend := 0;
        END (*IF*);

        (* Go back to the beginning of the desired section. *)

        TSSetPosition (TS0, start^.headerstart);

        (* Start sending the required body section. *)

        PartialReply (RC, " {");
        CardinalToStringLJ (bytestosend, sizebuff);
        PartialReply (RC, sizebuff);
        PartialReply (RC, "}");
        CommitReply (RC);

        (* Send the specified header lines.      *)

        LogMessage (RC, "  ... content suppressed in log ... )");
        SuppressLogging (RC);
        IF bytestosend > 0 THEN
            TSSetPosition (TS0, SD^.headerstart);
            EVAL (SendHeaderFields (info, boundary, TS0,
                                       info^.initialoffset, info^.amount,
                                       RC, TRUE));
        END (*IF*);

    END SendFragmentedFilePart;

(************************************************************************)

PROCEDURE SendContiguousFilePart (RC: ReplyCxt;  TS0: TextStream;
                                   info: BodyInfo;  SD: StructureData);

    (* Sends to the output channel that contains the subset of TS0      *)
    (* specified by the 'info' parameter.  This procedure is invoked    *)
    (* in all cases except where info^.part is equal to one of          *)
    (* HEADER_FIELDS or HEADER_FIELDS_NOT.                              *)

    CONST buffersize = 2048;

    VAR togo, amount, actual, bytestosend: CARDINAL;
        sizebuff: ARRAY [0..15] OF CHAR;
        start: FilePos;
        line, boundary: ARRAY [0..buffersize-1] OF CHAR;

    VAR bytessent: CARDINAL;

    BEGIN
        (* Locate the beginning of the desired section. *)

        boundary[0] := CtrlZ;  boundary[1] := Nul;
        SD := SkipToSection (SD, info^.section);
        IF SD = NIL THEN
            bytestosend := 0;
        ELSE
            start := SD^.headerstart;

            (* At this stage SD^.start says where the desired section       *)
            (* starts, and SD^.BodyByteCount and SD^.TotalByteCount give    *)
            (* the size of the body and the complete part, as appropriate.  *)

            (* To make sense of what is happening, we really need to        *)
            (* enumerate the possible cases.  I've done that above, where   *)
            (* the StructureData type is defined.                           *)

            IF SD^.is822part THEN

                (* Embedded rfc822 messages are special in that SD points   *)
                (* to the MIME header (but SD^.TotalByteCount still counts  *)
                (* the whole message/rfc822 part), but SD^.parts points to  *)
                (* the structure data for the message itself.               *)

                IF (info^.part = HEADER) OR (info^.part = TEXT) THEN
                    SD := SD^.parts;
                END (*IF*);

            END (*IF*);

            (* Strictly speaking we should now permit HEADER only for the   *)
            (* total message or an embedded message/rfc822 part, and MIME   *)
            (* only for a part not at the top level; but we'll be tolerant  *)
            (* if the client uses the wrong term.                           *)

            CASE info^.part OF
                 | ALL:
                            bytestosend := SD^.TotalByteCount;
                 | MIME, HEADER:
                            bytestosend := ORDL(Diff64(SD^.headerstart, SD^.bodystart));
                 | TEXT:    (* go to the body start *)
                            start := SD^.bodystart;
                            bytestosend := SD^.BodyByteCount;
                 | ELSE     (* other cases should not occur *)
                            bytestosend := SD^.TotalByteCount;
            END (*CASE*);
        END (*IF*);

        IF info^.partial THEN

            (* May need to retain only part of the file.  Up until now, *)
            (* the value of bytestosend is the maximum number of bytes  *)
            (* available in the specified section.  If we move the      *)
            (* start point forward, we must adjust the available byte   *)
            (* count accordingly.                                       *)

            Add64 (start, info^.initialoffset);
            IF bytestosend > info^.initialoffset THEN
                DEC (bytestosend, info^.initialoffset);
            ELSE
                bytestosend := 0;
            END (*IF*);

            (* Set bytestosend equal to the maximum of the amount       *)
            (* available and the amount requested.                      *)

            IF info^.amount < bytestosend THEN
                bytestosend := info^.amount;
            END (*IF*);

        END (*IF*);

        (* Start sending the required body section. *)

        PartialReply (RC, " {");
        CardinalToStringLJ (bytestosend, sizebuff);
        PartialReply (RC, sizebuff);
        PartialReply (RC, "}");
        CommitReply (RC);

        TSSetPosition (TS0, start);
        LogMessage (RC, "  ... content suppressed in log ... )");
        SuppressLogging (RC);

        (* Copy the data from the current position. *)

        bytessent := 0;
        togo := bytestosend;
        WHILE togo > 0 DO
            amount := buffersize;
            IF togo < buffersize THEN
                amount := togo;
            END (*IF*);
            TSReadRaw (TS0, line, amount, actual);
            IF actual = 0 THEN
                togo := 0;
            ELSE
                IF actual < buffersize THEN
                    line[actual] := Nul;
                END (*IF*);
                PartialReply (RC, line);
                DEC (togo, actual);
                INC (bytessent, actual);
            END (*IF*);
        END (*WHILE*);

    END SendContiguousFilePart;

(************************************************************************)

PROCEDURE SendFilePart (RC: ReplyCxt;  file: FilenameString;
                                 info: BodyInfo;  SD: StructureData);

    (* Sends to the output channel that contains the subset of 'file'   *)
    (* specified by the 'info' parameter.                               *)

    VAR TS: TextStream;

    BEGIN
        TS := OpenForReading (file);
        IF (info^.part = HEADER_FIELDS) OR (info^.part = HEADER_FIELDS_NOT) THEN
            SendFragmentedFilePart (RC, TS, info, SD);
        ELSE
            SendContiguousFilePart (RC, TS, info, SD);
        END (*IF*);
        CloseTS (TS);
    END SendFilePart;

(************************************************************************)

PROCEDURE SendOneSection (RC: ReplyCxt;  VAR (*IN*) file: FilenameString;
                                      VAR (*INOUT*) info: BodyInfo;
                                      SD: StructureData);

    (* Sends back the specified part of the file.  This is a response   *)
    (* to a FETCH BODY[<section>]<<partial>>, where <section> is an     *)
    (* optional section number followed optionally by the specification *)
    (* TEXT or the specification of which header fields are wanted.     *)

    CONST
        HeaderSections = BodyParts {HEADER_FIELDS, HEADER_FIELDS_NOT, HEADER, MIME};

    VAR offsetbuff: ARRAY [0..15] OF CHAR;
        label: ARRAY [0..63] OF CHAR;
        label1: ARRAY [0..21] OF CHAR;

    BEGIN
        (* Construct a label to use in the reply.  *)

        Strings.Assign (info^.section, label);
        CASE info^.part OF
             | ALL:     Strings.Assign ("", label1);
             | HEADER,
               HEADER_FIELDS,
               HEADER_FIELDS_NOT:
                        Strings.Assign ("HEADER", label1);
             | MIME:    Strings.Assign ("MIME", label1);
             | TEXT:    Strings.Assign ("TEXT", label1);
             | ELSE
                        Strings.Assign ("", label1);
        END (*CASE*);

        IF (label[0] <> Nul) AND (label1[0] <> Nul) THEN
            Strings.Append ('.', label);
        END (*IF*);
        Strings.Append (label1, label);

        PartialReply (RC, "BODY[");
        PartialReply (RC, label);
        PartialReply (RC, "]");
        IF info^.partial THEN
            PartialReply (RC, "<");
            CardinalToStringLJ (info^.initialoffset, offsetbuff);
            PartialReply (RC, offsetbuff);
            PartialReply (RC, ">");
        END (*IF*);
        SendFilePart (RC, file, info, SD);

    END SendOneSection;

(************************************************************************)

PROCEDURE SendSection (RC: ReplyCxt;  VAR (*IN*) file: FilenameString;
                                      VAR (*INOUT*) info: BodyInfo;
                                      VAR (*INOUT*) SD: StructureData);

    (* Sends back the specified part of the file.  This is a response   *)
    (* to a FETCH BODY[<section>]<<partial>>, where <section> is an     *)
    (* optional section number followed optionally by the specification *)
    (* TEXT or the specification of which header fields are wanted.     *)

    VAR TS: TextStream;

    BEGIN
        IF info <> NIL THEN

            (* Create the structure data if it doesn't already exist. *)

            IF SD = NIL THEN
                TS := OpenForReading (file);
                BuildStructureData (TS, SD);
                CloseTS (TS);
            END (*IF*);

            SendOneSection (RC, file, info, SD);

        END (*IF*);
        DiscardStructureData (SD);

    END SendSection;

(************************************************************************)
(*                           INITIALISATION                             *)
(************************************************************************)

BEGIN
    NextName := "00000000";
    CreateLock (NextNameLock);
END Messages.

