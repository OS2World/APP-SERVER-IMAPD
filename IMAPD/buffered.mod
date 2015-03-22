(**************************************************************************)
(*                                                                        *)
(*  Support modules for network applications                              *)
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

IMPLEMENTATION MODULE BufferedTextStreams;

        (********************************************************)
        (*                                                      *)
        (*        Text buffers for line-oriented file input     *)
        (*                                                      *)
        (*          (At present we support only input)          *)
        (*                                                      *)
        (*   This module maintains a read-ahead buffer for each *)
        (*   open file.  It supports only sequential input.     *)
        (*   For semi-sequential with some random access, use   *)
        (*   module TextBuffers instead.                        *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            27 April 2005                   *)
        (*  Last edited:        23 July 2012                    *)
        (*  Status:             Complete, untested              *)
        (*                                                      *)
        (********************************************************)


FROM SYSTEM IMPORT LOC, ADDRESS, ADR;

IMPORT Strings;

FROM FileOps IMPORT
    (* const*)  NoSuchChannel,
    (* type *)  ChanId, FilePos,
    (* proc *)  OpenOldFile, CloseFile, ReadRaw,
                StartPosition, CurrentPosition, SetPosition;

FROM LONGLONG IMPORT
    (* proc *)  Sub64;

FROM Heap IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

FROM LowLevel IMPORT
    (* proc *)  Copy, AddOffset;

(************************************************************************)

CONST
    BufferSize = 4096;
    Nul = CHR(0);
    CR = CHR(13);  LF = CHR(10);
    CtrlZ = CHR(26);

TYPE
    BufferType = ARRAY [0..BufferSize] OF CHAR;

    (* A TextStream record contains a data buffer, which is the         *)
    (* lookahead data of the currently opened file.  The fields are     *)
    (*    cid            channel ID of the opened file                  *)
    (*    count          the number of data bytes in the buffer         *)
    (*    offset         offset within the data buffer to the next      *)
    (*                    character to be read in a sequential read.    *)
    (*    data           the actual cached data                         *)
    (* The data array is one byte longer than we need, with the last    *)
    (* character permanently set to Nul.  This prevents string searches *)
    (* from running off the end of the array.                           *)

    TextStream = POINTER TO TSrecord;
    TSrecord = RECORD
                   cid: ChanId;
                   count: CARDINAL;
                   offset: CARDINAL;
                   data: BufferType;
               END (*RECORD*);

VAR
    LineFeed: ARRAY [0..0] OF CHAR;

(************************************************************************)
(*                           OPEN/CLOSE/ETC                             *)
(************************************************************************)

PROCEDURE OpenForReading (name: ARRAY OF CHAR): TextStream;

    (* Opens an existing file for read-only access, and returns its     *)
    (* buffer ID.                                                       *)

    VAR result: TextStream;

    BEGIN
        NEW (result);
        WITH result^ DO
            cid := OpenOldFile (name, FALSE, FALSE);
            count := 0;
            offset := 0;
            data[BufferSize] := Nul;
            IF cid <> NoSuchChannel THEN
                ReadRaw (cid, data, BufferSize, count);
                data[count] := Nul;
            END (*IF*);
        END (*WITH*);
        RETURN result;
    END OpenForReading;

(************************************************************************)

PROCEDURE TSFileOpened (TS: TextStream): BOOLEAN;

    (* Use this to check whether an 'open' operation succeeded. *)

    BEGIN
        RETURN (TS <> NIL) AND (TS^.cid <> NoSuchChannel);
    END TSFileOpened;

(************************************************************************)

PROCEDURE CloseTS (VAR (*INOUT*) TS: TextStream);

    (* Closes a file. *)

    BEGIN
        IF TS <> NIL THEN
            CloseFile (TS^.cid);
            DEALLOCATE (TS, SIZE(TSrecord));
        END (*IF*);
    END CloseTS;

(************************************************************************)
(*                           LOADING THE DATA                           *)
(************************************************************************)

PROCEDURE ReloadBuffer (TS: TextStream);

    (* Reads some more data from the file.  If TS^.count = 0 after   *)
    (* this operation, we must have reached the end of the file.     *)

    BEGIN
        WITH TS^ DO
            IF cid = NoSuchChannel THEN
                count := 0;
            ELSE
                ReadRaw (cid, data, BufferSize, count);
            END (*IF*);
            data[count] := Nul;
            offset := 0;
        END (*WITH*);
    END ReloadBuffer;

(************************************************************************)
(*                         FILE POSITION/SIZE                           *)
(************************************************************************)

PROCEDURE TSCurrentPosition (TS: TextStream): FilePos;

    (* Returns the current position within the file. *)

    VAR result: FilePos;

    BEGIN
        result := CurrentPosition (TS^.cid);
        Sub64 (result, TS^.count - TS^.offset);
        RETURN result;
    END TSCurrentPosition;

(************************************************************************)

PROCEDURE TSStartPosition (TS: TextStream): FilePos;

    (* Returns the start-of-file position. *)

    BEGIN
        RETURN StartPosition (TS^.cid);
    END TSStartPosition;

(************************************************************************)

PROCEDURE TSSetPosition (TS: TextStream;  position: FilePos);

    (* Sets the current position within the file. *)

    BEGIN
        SetPosition (TS^.cid, position);
        ReloadBuffer (TS);
    END TSSetPosition;

(************************************************************************)
(*                              INPUT                                   *)
(************************************************************************)

PROCEDURE TSReadRaw (TS: TextStream;  VAR (*OUT*) data: ARRAY OF LOC;
                   limit: CARDINAL;  VAR (*OUT*) NumberRead: CARDINAL);

    (* Reads a buffer-full of information from a file. *)

    VAR ToGo, amount, available: CARDINAL;
        source, destination: ADDRESS;

    BEGIN
        NumberRead := 0;
        source := NIL;
        destination := ADR(data);
        ToGo := limit;
        IF ToGo > HIGH(data) THEN
            ToGo := HIGH(data) + 1;
        END (*IF*);
        WHILE ToGo > 0 DO
            IF TS^.count = 0 THEN
                ReloadBuffer (TS);
                IF TS^.count = 0 THEN
                    ToGo := 0;
                END (*IF*);
            END (*IF*);
            amount := ToGo;
            WITH TS^ DO
                available := count - offset;
                IF available > 0 THEN
                    source := ADR(data[offset]);
                END (*IF*);
            END (*WITH*);
            IF amount > available THEN
                amount := available;
                TS^.count := 0;
            END (*IF*);
            IF amount > 0 THEN
                Copy (source, destination, amount);
                DEC (ToGo, amount);
                INC (NumberRead, amount);
                WITH TS^ DO
                    INC (offset, amount);
                END (*WITH*);
                destination := AddOffset (destination, amount);
            END (*IF*);
        END (*WHILE*);

    END TSReadRaw;

(************************************************************************)

PROCEDURE TSReadLine (TS: TextStream;  VAR (*OUT*) line: ARRAY OF CHAR);

    (* Reads a line of text from a file.  Assumption: a line ends with  *)
    (* CRLF.  To avoid tortuous logic, I take the LF as end of line and *)
    (* skip the CR.  At end of file we return with line[0] = Ctrl/Z.    *)

    VAR length, pos, extra, space: CARDINAL;
        found: BOOLEAN;
        source, destination: ADDRESS;

    BEGIN
        space := HIGH(line) + 1;
        destination := ADR(line);
        REPEAT
            found := FALSE;
            WITH TS^ DO
                IF count = 0 THEN
                    ReloadBuffer (TS);
                    IF count = 0 THEN
                        line[0] := CtrlZ;
                        DEC (space);
                        found := TRUE;
                    END (*IF*);
                END (*IF*);
                IF NOT found THEN
                    source := ADR(data[offset]);
                    Strings.FindNext (LineFeed, data, offset, found, pos);
                    IF found THEN
                        extra := 1;
                    ELSE
                        pos := count;
                        extra := 0;
                    END (*IF*);
                    length := pos - offset;
                    IF (length > 0) AND (data[pos-1] = CR) THEN
                        DEC (length);  INC (extra);
                    END (*IF*);
                    IF length > space THEN
                        INC (extra, length-space);
                        length := space;
                    END (*IF*);
                    IF length > 0 THEN
                        Copy (source, destination, length);
                        DEC (space, length);
                        destination := AddOffset (destination, length);
                    END (*IF*);
                    INC (length, extra);
                    IF found THEN
                        IF length > 0 THEN
                            INC (offset, length);
                        END (*IF*);
                    ELSE
                        count := 0;
                    END (*IF*);
                END (*IF*);
            END (*WITH*);
        UNTIL found;

        IF space > 0 THEN
            line[HIGH(line)+1-space] := Nul;
        END (*IF*);

    END TSReadLine;

(************************************************************************)

BEGIN
    LineFeed[0] := LF;
END BufferedTextStreams.

