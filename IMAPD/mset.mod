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

IMPLEMENTATION MODULE MSet;

        (********************************************************)
        (*                                                      *)
        (*                     Message sets                     *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            13 March 2001                   *)
        (*  Last edited:        8 February 2005                 *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (********************************************************)


IMPORT Strings;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

(************************************************************************)

TYPE
    MessageSet = POINTER TO SetRecord;
    SetRecord = RECORD
                    next: MessageSet;
                    from, to: CARDINAL;
                END (*RECORD*);

(************************************************************************)

PROCEDURE GetSet (VAR (*INOUT*) input: ARRAY OF CHAR): MessageSet;

    (* Parses a "set" specification at the head of input, deletes those  *)
    (* characters from input.                                            *)

    TYPE CharSet = SET OF CHAR;
    CONST
        Digits = CharSet {'0'..'9'};
        ExtendedDigits = CharSet {'*'} + Digits;

    VAR k: CARDINAL;

    (********************************************************************)

    PROCEDURE GetNum(): CARDINAL;

        (* Picks up a number starting from input[k], updates k. *)

        VAR result: CARDINAL;

        BEGIN
            IF input[k] = '*' THEN
                result := MAX(CARDINAL);
                INC (k);
            ELSE
                result := 0;
                WHILE input[k] IN Digits DO
                    result := 10*result + (ORD(input[k]) - ORD('0'));
                    INC (k);
                END (*WHILE*);
            END (*IF*);
            RETURN result;
        END GetNum;

    (********************************************************************)

    VAR head, before, this, after: MessageSet;  temp: CARDINAL;

    BEGIN
        head := NIL;  k := 0;
        LOOP
            IF input[k] IN ExtendedDigits THEN

                (* Pick up a single number or a to:from pair. *)

                NEW (this);
                this^.next := NIL;
                this^.from := GetNum();
                IF input[k] = ':' THEN
                    INC (k);
                    this^.to := GetNum();
                    IF this^.to < this^.from THEN
                        temp := this^.from;
                        this^.from := this^.to;
                        this^.to := temp;
                    END (*IF*);
                ELSE
                    this^.to := this^.from;
                END (*IF*);

                (* Find the insertion point for the new record. *)

                before := NIL;
                after := head;
                WHILE (after <> NIL) AND (after^.to < this^.to) DO
                    before := after;  after := after^.next;
                END (*WHILE*);
                this^.next := after;

                (* Check for a merge with the "before" element. *)

                IF before = NIL THEN
                    head := this;
                ELSIF this^.from = before^.to + 1 THEN
                    before^.to := this^.to;
                    DISPOSE (this);
                    this := before;
                ELSE
                    before^.next := this;
                END (*IF*);

                (* Check for a merge with the "after" element. *)

                IF (after <> NIL) AND (after^.from <= this^.to + 1) THEN
                    IF after^.to > this^.to THEN
                        this^.to := after^.to;
                    END (*IF*);
                    this^.next := after^.next;
                    DISPOSE (after);
                END (*IF*);

                (* Keep looping if we find a comma. *)

                IF input[k] = ',' THEN
                    INC (k);
                ELSE
                    EXIT (*LOOP*);
                END (*IF*);
            ELSE
                EXIT (*LOOP*);
            END (*IF*);
        END (*LOOP*);
        WHILE (k <= HIGH(input)) AND (input[k] = ' ') DO
            INC (k);
        END (*WHILE*);
        IF k > 0 THEN
            Strings.Delete (input, 0, k);
        END (*IF*);
        RETURN head;
    END GetSet;

(************************************************************************)

PROCEDURE NonEmptySet (S: MessageSet): BOOLEAN;

    (* Returns TRUE iff S is not an empty set. *)

    BEGIN
        RETURN S <> NIL;
    END NonEmptySet;

(************************************************************************)

PROCEDURE NextMember (VAR (*INOUT*) S: MessageSet): CARDINAL;

    (* Returns the first member of the set, deletes that member. *)

    VAR result: CARDINAL;
        next: MessageSet;

    BEGIN
        IF S = NIL THEN
            result := 0;
        ELSE
            result := S^.from;
            IF result >= S^.to THEN
                next := S^.next;
                DISPOSE (S);
                S := next;
            ELSE
                INC (S^.from);
            END (*IF*);
        END (*IF*);
        RETURN result;
    END NextMember;

(************************************************************************)

PROCEDURE NextMemberFrom (VAR (*INOUT*) S: MessageSet;
                                        startfrom: CARDINAL): CARDINAL;

    (* Returns the first member of the set whose value is at least      *)
    (* equal to startfrom, deletes that member and everything           *)
    (* preceding it.  Returns 0 if there is no such member.             *)

    VAR result: CARDINAL;
        next: MessageSet;

    BEGIN
        WHILE (S <> NIL) AND (S^.to < startfrom) DO
            next := S^.next;
            DISPOSE (S);
            S := next;
        END (*WHILE*);

        IF S = NIL THEN
            result := 0;
        ELSE
            IF startfrom < S^.from THEN
                result := S^.from;
            ELSE
                result := startfrom;
            END (*IF*);

            IF result >= S^.to THEN
                next := S^.next;
                DISPOSE (S);
                S := next;
            ELSE
                S^.from := result + 1;
            END (*IF*);

        END (*IF*);

        RETURN result;

    END NextMemberFrom;

(************************************************************************)

PROCEDURE DiscardSet (VAR (*INOUT*) S: MessageSet);

    (* Deletes the set from memory. *)

    VAR next: MessageSet;

    BEGIN
        WHILE S <> NIL DO
            next := S^.next;
            DISPOSE (S);
            S := next;
        END (*WHILE*);
    END DiscardSet;

(************************************************************************)

END MSet.

