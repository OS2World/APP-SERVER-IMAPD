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

IMPLEMENTATION MODULE Parser;

        (********************************************************)
        (*                                                      *)
        (*         Parser for some of the IMAP4 syntax          *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            7 March 2001                    *)
        (*  Last edited:        2 October 2014                  *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (********************************************************)


IMPORT Strings, SysClock;

FROM Names IMPORT
    (* type *)  ArgPointer;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;


(************************************************************************)
(*  General rule: for parsing procedures having 'input' and 'result'    *)
(*  parameters, a function result of TRUE means that the token found    *)
(*  has been deleted from the head of 'input' and moved to 'result'.    *)
(*  If the result is FALSE then 'input' is left unchanged and 'result'  *)
(*  contains no meaningful result.                                      *)
(************************************************************************)

TYPE
    CharSet = SET OF CHAR;
    Months = ARRAY [1..13] OF ARRAY [0..2] OF CHAR;

CONST
    Digits = CharSet {'0'..'9'};
    Nul = CHR(0);
    Month = Months{"Jan","Feb","Mar","Apr","May","Jun",
                         "Jul","Aug","Sep","Oct","Nov","Dec","   "};

(************************************************************************)

PROCEDURE SPACE (VAR (*INOUT*) input: ARRAY OF CHAR): BOOLEAN;

    (* Space character. *)

    BEGIN
        IF input[0] = ' ' THEN
            Strings.Delete (input, 0, 1);
            RETURN TRUE;
        ELSE
            RETURN FALSE;
        END (*IF*);
    END SPACE;

(************************************************************************)

PROCEDURE StripLeadingSpaces (VAR (*INOUT*) buffer: ARRAY OF CHAR);

    (* Deletes leading spaces from buffer. *)

    VAR k: CARDINAL;

    BEGIN
        k := 0;
        WHILE (k <= HIGH(buffer)) AND (buffer[k] = ' ') DO
            INC (k);
        END (*WHILE*);
        IF k > 0 THEN
            Strings.Delete (buffer, 0, k);
        END (*IF*);
    END StripLeadingSpaces;

(************************************************************************)

PROCEDURE Number (VAR (*INOUT*) input: ARRAY OF CHAR;
                   VAR (*OUT*) result: CARDINAL): BOOLEAN;

    (* Numeric string. *)

    VAR j: CARDINAL;

    BEGIN
        result := 0;  j := 0;
        WHILE input[j] IN Digits DO
            result := 10*result + (ORD(input[j]) - ORD('0'));
            INC (j);
        END (*WHILE*);
        IF j > 0 THEN
            Strings.Delete (input, 0, j);
        END (*IF*);
        RETURN j > 0;
    END Number;

(************************************************************************)

PROCEDURE StringOf (Allowed: CharSet;  VAR (*INOUT*) input: ARRAY OF CHAR;
                   VAR (*OUT*) result: ARRAY OF CHAR): BOOLEAN;

    (* Arbitrary string of characters from the set Allowed. *)

    VAR k: CARDINAL;

    BEGIN
        k := 0;

        WHILE input[k] IN Allowed DO
            result[k] := input[k];
            INC(k);
        END (*WHILE*);

        (* Terminate the result string, and delete the part     *)
        (* of the input that we have consumed.                  *)

        IF k <= HIGH(result) THEN
            result[k] := Nul;
        END (*IF*);
        IF k > 0 THEN
            Strings.Delete (input, 0, k);
        END (*IF*);
        RETURN TRUE;

    END StringOf;

(************************************************************************)

PROCEDURE Atom (Wild: BOOLEAN;  VAR (*INOUT*) input: ARRAY OF CHAR;
                   VAR (*OUT*) result: ARRAY OF CHAR): BOOLEAN;

    (* Atom, with wildcards allowed iff Wild=TRUE. *)

    CONST
        NonNull7 = CharSet {CHR(0)..CHR(127)};
        ControlChars = CharSet {CHR(0)..CHR(31), CHR(127)};
        QuotedSpecials = CharSet {'"', '\'};
        ListWildcards = CharSet {'%', '*'};
        AtomSpecials = CharSet {'(', ')', '{', ' '}
                         + ControlChars + ListWildcards + QuotedSpecials;

    VAR Included: CharSet;

    BEGIN
        Included := NonNull7 - AtomSpecials;
        IF Wild THEN Included := Included + ListWildcards END(*IF*);
        RETURN StringOf (Included, input, result);
    END Atom;

(************************************************************************)

PROCEDURE String (VAR (*INOUT*) input: ARRAY OF CHAR;
                   VAR (*OUT*) result: ARRAY OF CHAR): BOOLEAN;

    (* String.  *)

    VAR j, k, count: CARDINAL;  ch: CHAR;

    BEGIN
        j := 1;  k := 0;
        IF input[0] = '{' THEN

            (* Literal with octet count *)

            count := 0;
            WHILE input[j] IN Digits DO
                count := 10*count + (ORD(input[j]) - ORD('0'));
                INC (j);
            END (*WHILE*);
            IF input[j] <> '}' THEN
                RETURN FALSE;
            END (*IF*);
            INC (j);
            LOOP
                IF count = 0 THEN EXIT(*LOOP*) END(*IF*);
                IF (j > HIGH(input)) OR (input[j] = Nul)
                                 OR (k > HIGH(result)) THEN
                    RETURN FALSE;
                END (*IF*);
                result[k] := input[j];
                INC (j);  INC (k);  DEC(count);
            END (*LOOP*);

        ELSIF input[0] = '"' THEN

            (* Quoted string *)

            LOOP
                IF input[j] = Nul THEN RETURN FALSE
                ELSIF input[j] = '\' THEN
                    INC(j);
                    ch := input[j];  INC(j);
                    IF (ch = '"') OR (ch = '\') THEN
                        result[k] := input[j];
                        INC(k);
                    ELSE
                        RETURN FALSE;
                    END (*IF*);
                ELSIF input[j] = '"' THEN
                    INC(j);  EXIT (*LOOP*);
                ELSE
                    result[k] := input[j];
                    INC(j);  INC(k);
                END (*IF*);
            END (*LOOP*);

        ELSE
            RETURN FALSE;
        END (*IF*);

        (* Terminate the result string, and delete the part     *)
        (* of the input that we have consumed.                  *)

        IF k <= HIGH(result) THEN
            result[k] := Nul;
        END (*IF*);
        IF j > 0 THEN
            Strings.Delete (input, 0, j);
        END (*IF*);
        RETURN TRUE;

    END String;

(************************************************************************)

PROCEDURE AStringW (Wild: BOOLEAN;  VAR (*INOUT*) input: ARRAY OF CHAR;
                   VAR (*OUT*) result: ARRAY OF CHAR): BOOLEAN;

    (* Atom or string, with wildcards allowed in atom iff Wild=TRUE. *)

    BEGIN
        IF (input[0] = '{') OR (input[0] = '"') THEN
            RETURN String (input, result);
        ELSE
            RETURN Atom (Wild, input, result);
        END (*IF*);
    END AStringW;

(************************************************************************)

PROCEDURE AString (VAR (*INOUT*) input: ARRAY OF CHAR;
                   VAR (*OUT*) result: ARRAY OF CHAR): BOOLEAN;

    (* Atom or string. *)

    BEGIN
        RETURN AStringW (FALSE, input, result);
    END AString;

(************************************************************************)

PROCEDURE DecodeFlags (VAR (*INOUT*) buffer: ARRAY OF CHAR): MessageFlagSet;

    (* Converts a character string to a MessageFlagSet.  On exit,  *)
    (* buffer contains anything left after removing the options.   *)

    VAR flags: MessageFlagSet;

    BEGIN
        flags := MessageFlagSet{};
        StripLeadingSpaces (buffer);
        WHILE buffer[0] = '\' DO
            IF Match (buffer, "\Seen") THEN
                INCL (flags, seen);
            ELSIF Match (buffer, "\Answered") THEN
                INCL (flags, answered);
            ELSIF Match (buffer, "\Flagged") THEN
                INCL (flags, flagged);
            ELSIF Match (buffer, "\Deleted") THEN
                INCL (flags, deleted);
            ELSIF Match (buffer, "\Draft") THEN
                INCL (flags, draft);
            ELSIF Match (buffer, "\Recent") THEN
                INCL (flags, recent);
            ELSE
                Strings.Delete (buffer, 0, 1);
            END (*IF*);
            StripLeadingSpaces (buffer);
        END (*WHILE*);
        RETURN flags;
    END DecodeFlags;

(************************************************************************)

PROCEDURE FlagList (VAR (*INOUT*) input: ARRAY OF CHAR;
                     VAR (*OUT*) flags: MessageFlagSet): BOOLEAN;

    (* Parenthesised list of APPEND codes. *)

    BEGIN
        flags := MessageFlagSet {};
        IF input[0] = '(' THEN
            Strings.Delete (input, 0, 1);
            flags := DecodeFlags (input);
            IF input[0] = ')' THEN
                Strings.Delete (input, 0, 1);
                RETURN TRUE;
            ELSE
                RETURN FALSE;
            END (*IF*);
        ELSE
            RETURN FALSE;
        END (*IF*);
    END FlagList;

(************************************************************************)

PROCEDURE CopyChars (VAR (*IN*) input: ARRAY OF CHAR;
                      VAR (*INOUT*) inpos: CARDINAL;
                      VAR (*OUT*) output: ARRAY OF CHAR;
                      VAR (*INOUT*) outpos: CARDINAL;
                      accept: CharSet;
                      min, max: CARDINAL): BOOLEAN;

    (* Copy at least min and and at most max chars from accept from     *)
    (* input to output.  Iff successful, inpos and outpos updated.      *)

    VAR ipos, opos, count: CARDINAL;
        success: BOOLEAN;

    BEGIN
        success := TRUE;
        ipos := inpos;  opos := outpos;
        count := 0;
        LOOP
            IF count = max THEN
                EXIT (*LOOP*);
            ELSIF input[ipos] IN accept THEN
                output[opos] := input[ipos];
                INC (ipos);  INC (opos);  INC (count);
            ELSIF count < min THEN
                success := FALSE;
                EXIT (*LOOP*);
            END (*IF*);
        END (*LOOP*);
        IF success THEN
            inpos := ipos;  outpos := opos;
            output[opos] := Nul;
        END (*IF*);
        RETURN success;
    END CopyChars;

(************************************************************************)

PROCEDURE GetMonth (input: ArgPointer;
                               VAR (*INOUT*) inpos: CARDINAL): CARDINAL;

    (* Converts a month name (a three-character string) to numeric.  *)
    (* Returns 13 if no match is found.                              *)

    (********************************************************************)

    PROCEDURE MatchMonth (month: CARDINAL): BOOLEAN;

        VAR k: CARDINAL;
            success: BOOLEAN;

        BEGIN
            k := 0;
            WHILE (k <= 2) AND (CAP(input^[inpos+k]) = CAP(Month[month][k])) DO
                INC (k);
            END (*WHILE*);
            success := k > 2;
            IF success THEN
                INC (inpos, 3);
            END (*IF*);
            RETURN success;
        END MatchMonth;

    (********************************************************************)

    VAR month: CARDINAL;

    BEGIN
        month := 1;
        WHILE (month < 13) AND NOT MatchMonth (month) DO
            INC (month);
        END (*LOOP*);
        RETURN month;
    END GetMonth;

(************************************************************************)

PROCEDURE CopyMonth (VAR (*IN*) input: ARRAY OF CHAR;
                      VAR (*INOUT*) inpos: CARDINAL;
                      VAR (*OUT*) output: ARRAY OF CHAR;
                      VAR (*INOUT*) outpos: CARDINAL): BOOLEAN;

    (* Copy a month name (a three-character string).  *)

    (********************************************************************)

    PROCEDURE MatchMonth (month: CARDINAL): BOOLEAN;

        VAR k: CARDINAL;
            success: BOOLEAN;

        BEGIN
            k := 0;
            WHILE (k <= 2) AND (CAP(input[inpos+k]) = CAP(Month[month][k])) DO
                output[outpos+k] := Month[month][k];
                INC (k);
            END (*WHILE*);
            success := k > 2;
            IF success THEN
                INC (inpos, 3);  INC (outpos, 3);
                output[outpos] := Nul;
            END (*IF*);
            RETURN success;
        END MatchMonth;

    (********************************************************************)

    VAR month: CARDINAL;

    BEGIN
        month := 1;
        WHILE (month < 13) AND NOT MatchMonth (month) DO
            INC (month);
        END (*LOOP*);
        RETURN month < 13;
    END CopyMonth;

(************************************************************************)

PROCEDURE CopyDigits (VAR (*IN*) input: ARRAY OF CHAR;
                      VAR (*INOUT*) inpos: CARDINAL;
                      VAR (*OUT*) output: ARRAY OF CHAR;
                      VAR (*INOUT*) outpos: CARDINAL;
                      min, max: CARDINAL): BOOLEAN;

    (* Copy at least min and and at most max decimal digits from        *)
    (* input to output.  Iff successful, inpos and outpos updated.      *)

    BEGIN
        RETURN CopyChars (input, inpos, output, outpos, Digits, min, max);
    END CopyDigits;

(************************************************************************)

PROCEDURE Date (VAR (*INOUT*) input: ARRAY OF CHAR;
                   VAR (*OUT*) result: ARRAY OF CHAR): BOOLEAN;

    (* Day-month-year or Day month year string. *)

    VAR inpos, outpos: CARDINAL;
        quoted, success: BOOLEAN;

    BEGIN
        inpos := 0;  outpos := 0;
        quoted := input[0] = '"';
        IF quoted THEN
            INC (inpos);
        END (*IF*);
        IF input[inpos] = ' ' THEN
            INC (inpos);
        END (*IF*);
        success := CopyDigits (input, inpos, result, outpos, 1, 2)
            AND CopyChars (input, inpos, result, outpos, CharSet{'-',' '}, 1, 1)
            AND CopyMonth (input, inpos, result, outpos)
            AND CopyChars (input, inpos, result, outpos, CharSet{'-',' '}, 1, 1)
            AND CopyDigits (input, inpos, result, outpos, 4, 4);
        IF success AND quoted THEN
            success := input[inpos] = '"';
            INC (inpos);
        END (*IF*);
        IF success THEN
            Strings.Delete (input, 0, inpos);
        END (*IF*);
        RETURN success;
    END Date;

(************************************************************************)

PROCEDURE PackedTime (VAR (*INOUT*) input: ARRAY OF CHAR;
                                     VAR (*OUT*) time: CARDINAL): BOOLEAN;

    (* HH:MM:SS string, with result converted to the packed format      *)
    (* used by the file system.                                         *)

    VAR hours, mins, secs: CARDINAL;  success: BOOLEAN;

    BEGIN
        mins := 0;  secs := 0;
        success := Number (input, hours) AND Match (input, ':')
                       AND Number (input, mins) AND Match (input, ':')
                       AND Number (input, secs);
        IF success THEN
            time := 32*(64*hours + mins) + secs DIV 2;
        ELSE
            time := 0;
        END (*IF*);
        RETURN success;
    END PackedTime;

(************************************************************************)

PROCEDURE PackDate (datestring: ArgPointer): CARDINAL;

    (* Converts a textual date (in dd-mmm-yyyy or dd mmm yyyy format)   *)
    (* into the packed format used by the file system.                  *)

    (********************************************************************)

    PROCEDURE GetCard (VAR (*INOUT*) pos: CARDINAL): CARDINAL;

        (* Reads a cardinal number from datestring. *)

        VAR result: CARDINAL;

        BEGIN
            result := 0;
            WHILE datestring^[pos] IN Digits DO
                result := 10*result + (ORD(datestring^[pos]) - ORD('0'));
                INC (pos);
            END (*WHILE*);
            RETURN result;
        END GetCard;

    (********************************************************************)

    VAR day, month, year: CARDINAL;
        pos: CARDINAL;

    BEGIN
        pos := 0;
        day := GetCard(pos) MOD 32;
        IF (datestring^[pos] = '-') OR (datestring^[pos] = ' ') THEN
            INC (pos);
        END (*IF*);
        month := GetMonth(datestring, pos);
        IF (datestring^[pos] = '-') OR (datestring^[pos] = ' ') THEN
            INC (pos);
        END (*IF*);
        year := GetCard(pos);
        IF year < 80 THEN
            INC (year, 2000);
        ELSIF year < 100 THEN
            INC (year, 1900);
        ELSIF year < 1980 THEN
            year := 1980;
        ELSIF year > 2107 THEN
            year := 2107;
        END (*IF*);
        RETURN 32*(16*(year-1980) + month) + day;
    END PackDate;

(************************************************************************)

PROCEDURE TZcorrect (VAR (*INOUT*) date, time: CARDINAL;
                                                    TZhr, TZmin: INTEGER);

    (* Adjusts date and time by the amount by which (TZhr,TZmin)        *)
    (* differs from our own time zone.                                  *)

    (* TESTING NEEDED HERE: the sign of the adjustment is not at all    *)
    (* obvious.                                                         *)

    (********************************************************************)

    TYPE MonthArray = ARRAY [1..12] OF CARDINAL;
    CONST LastDayOf = MonthArray {31,28,31,30,31,30,31,31,30,31,30,31};

    (********************************************************************)

    PROCEDURE DecDate;

        (* Reduces 'date' by one day. *)

        VAR year, month, day: CARDINAL;

        BEGIN
            day := date MOD 32;
            month := date DIV 32;
            year := month DIV 16;
            month := month MOD 16;

            IF day > 1 THEN
                DEC (day);
            ELSIF month > 1 THEN
                DEC (month);
                day := LastDayOf[month];
                IF (month = 2) AND (year MOD 4 = 0) THEN
                    INC (day);
                END (*IF*);
            ELSE
                month := 12;  day := 31;
                IF year > 0 THEN
                    DEC (year);
                END (*IF*);
            END (*IF*);

            date := 32*(16*year + month) + day;
        END DecDate;

    (********************************************************************)

    PROCEDURE IncDate;

        (* Advances 'date' by one day. *)

        VAR year, month, day: CARDINAL;

        BEGIN
            day := date MOD 32;
            month := date DIV 32;
            year := month DIV 16;
            month := month MOD 16;

            IF day < LastDayOf[month] THEN
                INC (day);
            ELSIF (month = 2) AND (day = 28) AND (year MOD 4 = 0) THEN
                day := 29;
            ELSIF month < 12 THEN
                INC (month);
                day := 1;
            ELSE
                month := 1;  day := 1;
                IF year < 127 THEN
                    INC (year);
                END (*IF*);
            END (*IF*);

            date := 32*(16*year + month) + day;
        END IncDate;

    (********************************************************************)

    VAR now: SysClock.DateTime;  adjust: INTEGER;
        aa, hour, min, sec2: CARDINAL;

    BEGIN
        SysClock.GetClock (now);
        adjust := 60*TZhr + TZmin;
        IF now.zone <> -1 THEN
            INC (adjust, now.zone);
        END (*IF*);
        min := time DIV 32;  sec2 := time MOD 32;
        hour := min DIV 64;  min := min MOD 64;
        aa := ABS(adjust);
        IF adjust > 0 THEN
            INC (min, aa);
            IF min > 59 THEN
                INC (hour, min DIV 60);
                min := min MOD 60;
                IF hour > 23 THEN
                    DEC (hour, 24);
                    IncDate;
                END (*IF*);
            ELSE
                INC (min, aa);
            END (*IF*);
            time := 32*(64*hour + min) + sec2;
        ELSE
            (* Adjust the minutes. *)
            IF min < aa MOD 60 THEN
                INC (min, 60);
                INC (aa, 60);
            END (*IF*);
            DEC (min, aa MOD 60);
            (* Now the hours. *)
            aa := aa DIV 60;
            IF hour < aa THEN
                INC (hour, 24);
                DecDate;
            END (*IF*);
            DEC (hour, aa);
        END (*IF*);

        time := 32*(64*hour + min) + sec2;

    END TZcorrect;

(************************************************************************)

PROCEDURE DateTime (VAR (*INOUT*) input: ARRAY OF CHAR;
                       VAR (*OUT*) date, time: CARDINAL): BOOLEAN;

    (* Parses a date/time string of the form "dd-mmm-yyyy hh:mm:ss TZ", *)
    (* where TZ is a four-digit time zone optionally preceded by '+' or *)
    (* '-'.  Returns a packed date and time, in the format used by the  *)
    (* file system, and corrected to our local time zone.               *)

    (********************************************************************)

    PROCEDURE TZstring (VAR (*OUT*) hour, min: INTEGER): BOOLEAN;

        (* Picks up the time zone information from the input. *)

        VAR count: CARDINAL;  negative, success: BOOLEAN;

        (****************************************************************)

        PROCEDURE TwoDigit (VAR (*OUT*) value: INTEGER): BOOLEAN;

            (* Picks up a two-digit number. *)

            BEGIN
                value := 0;
                IF input[count] IN Digits THEN
                    value := ORD(input[count]) - ORD('0');
                    INC (count);
                ELSE
                    RETURN FALSE;
                END (*IF*);
                IF input[count] IN Digits THEN
                    value := 10*value;
                    INC (value, ORD(input[count]) - ORD('0'));
                    INC (count);
                ELSE
                    RETURN FALSE;
                END (*IF*);
                RETURN TRUE;
            END TwoDigit;

        (****************************************************************)

        BEGIN
            hour := 0;  min := 0;  negative := FALSE;  count := 0;
            IF input[0] = '+' THEN
                count := 1;
            ELSIF input[0] = '-' THEN
                negative := TRUE;
                count := 1;
            END (*IF*);
            success := TwoDigit(hour) AND TwoDigit(min);
            IF success AND negative THEN
                hour := -hour;  min := -min;
            END (*IF*);
            RETURN success;
        END TZstring;

    (********************************************************************)

    VAR quoted, success: BOOLEAN;
        bufptr: ArgPointer;
        TZhr, TZmin: INTEGER;

    BEGIN
        quoted := Match (input, '"');

        (* Parse and convert date. *)

        NEW (bufptr);
        success := Date (input, bufptr^);
        IF success THEN
            date := PackDate (bufptr);
        ELSE
            date := 0;
        END (*IF*);
        DISPOSE (bufptr);

        (* Parse and convert time. *)

        success := success AND SPACE (input) AND PackedTime (input, time);
        IF NOT success THEN
            time := 0;
        END (*IF*);

        (* Time zone. *)

        TZhr := 0;  TZmin := 0;
        success := success AND SPACE(input) AND TZstring (TZhr, TZmin);

        IF success AND quoted THEN
            success := Match (input, '"');
        END (*IF*);

        IF success THEN
            TZcorrect (date, time, TZhr, TZmin);
        END (*IF*);

        RETURN success;

    END DateTime;

(************************************************************************)

PROCEDURE Literal (VAR (*INOUT*) input: ARRAY OF CHAR;
                                    VAR (*OUT*) count: CARDINAL): BOOLEAN;

    (* Literal string with count.  In this case we calculate the count, *)
    (* but leave it to the caller to pick up the actual literal string, *)
    (* which starts on the next input line.                             *)

    VAR j: CARDINAL;

    BEGIN
        IF input[0] = '{' THEN

            (* Literal with octet count *)

            count := 0;  j := 1;
            WHILE input[j] IN Digits DO
                count := 10*count + (ORD(input[j]) - ORD('0'));
                INC (j);
            END (*WHILE*);
            IF input[j] = '}' THEN
                RETURN TRUE;
            ELSE
                RETURN FALSE;
            END (*IF*);
        ELSE
            RETURN FALSE;
        END (*IF*);
    END Literal;

(************************************************************************)

PROCEDURE Match (VAR (*INOUT*) input: ARRAY OF CHAR;  string: ARRAY OF CHAR): BOOLEAN;

    (* Match of a string, with character case not significant. *)

    VAR success: BOOLEAN;  j: CARDINAL;

    BEGIN
        success := TRUE;  j := 0;
        LOOP
            IF (j > HIGH(string)) OR (string[j] = Nul) THEN
                EXIT (*LOOP*);
            END(*IF*);
            IF CAP(input[j]) <> CAP(string[j]) THEN
                success := FALSE;
                EXIT (*LOOP*);
            END (*IF*);
            INC (j);
        END (*LOOP*);
        IF success AND (j > 0) THEN
            Strings.Delete (input, 0, j);
        END (*IF*);
        RETURN success;
    END Match;

(************************************************************************)

PROCEDURE MatchNoDelete (VAR (*IN*) input: ARRAY OF CHAR;
                                   string: ARRAY OF CHAR): BOOLEAN;

    (* Match of a string, with character case not significant.  Unlike *)
    (* most of the procedures in this module, this one does NOT        *)
    (* alter the input string.                                         *)

    VAR success: BOOLEAN;  j: CARDINAL;

    BEGIN
        success := TRUE;  j := 0;
        LOOP
            IF (j > HIGH(string)) OR (string[j] = Nul) THEN
                EXIT (*LOOP*);
            END(*IF*);
            IF CAP(input[j]) <> CAP(string[j]) THEN
                success := FALSE;
                EXIT (*LOOP*);
            END (*IF*);
            INC (j);
        END (*LOOP*);
        RETURN success;
    END MatchNoDelete;

(************************************************************************)

END Parser.

