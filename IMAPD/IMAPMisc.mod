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

IMPLEMENTATION MODULE IMAPMisc;

        (********************************************************)
        (*                                                      *)
        (*    Miscellaneous procedures for the IMAP4 server     *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            19 March 2001                   *)
        (*  Last edited:        25 August 2012                  *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (********************************************************)


FROM INIData IMPORT
    (* type *)  HINI,
    (* proc *)  OpenINIFile;

(*************************************************************************)

CONST
    Nul = CHR(0);

VAR
    UseTNI: BOOLEAN;

(*************************************************************************)

PROCEDURE CompareStr (first, second: ARRAY OF CHAR): INTEGER;

    (* Compares two strings, with alphabetic case ignored.  *)
    (* Returns >0 if first > second, and so on.             *)

    VAR ch1, ch2: CHAR;  k: CARDINAL;

    BEGIN
        k := 0;
        LOOP
            IF (k > HIGH(first)) OR (first[k] = Nul) THEN
                IF (k > HIGH(second)) OR (second[k] = Nul) THEN
                    RETURN 0;
                ELSE
                    RETURN -1;
                END (*IF*);
            ELSIF (k > HIGH(second)) OR (second[k] = Nul) THEN
                RETURN +1;
            END (*IF*);
            ch1 := CAP(first[k]);  ch2 := CAP(second[k]);
            IF ch1 > ch2 THEN RETURN +1
            ELSIF ch1 < ch2 THEN RETURN -1
            END (*IF*);
            INC (k);
        END (*LOOP*);
    END CompareStr;

(*************************************************************************)

PROCEDURE SetTNImode (TNImode: BOOLEAN);

    (* If parameter is TRUE, specifies use of a TNI file. *)

    BEGIN
        UseTNI := TNImode;
    END SetTNImode;

(*************************************************************************)

PROCEDURE OpenOurINIFile(): HINI;

    (* Opens WEASEL.INI or WEASEL.TNI, returns a handle to it. *)

    VAR name: ARRAY [0..10] OF CHAR;

    BEGIN
        IF UseTNI THEN
            name := "Weasel.tni";
        ELSE
            name := "Weasel.ini";
        END (*IF*);
        RETURN OpenINIFile (name, UseTNI);
    END OpenOurINIFile;

(*************************************************************************)

BEGIN
    UseTNI := FALSE;
END IMAPMisc.

