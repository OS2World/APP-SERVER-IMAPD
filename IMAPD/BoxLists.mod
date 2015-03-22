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

IMPLEMENTATION MODULE BoxLists;

        (********************************************************)
        (*                                                      *)
        (*           User data for the IMAP4 server             *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            8 March 2001                    *)
        (*  Last edited:        25 August 2012                  *)
        (*  Status:             OK but incomplete               *)
        (*                                                      *)
        (*    I am not yet completely convinced that the        *)
        (*    'mother' and 'childcount' information is being    *)
        (*    correctly maintained when we merge two lists.     *)
        (*    (The risk is that we'll copy a pointer to a       *)
        (*    record that has been discarded because it was     *)
        (*    a duplicate.)  More checking needed on this point.*)
        (*                                                      *)
        (*    Resolution: I might have solved this problem      *)
        (*    now, but more testing needed.                     *)
        (*                                                      *)
        (********************************************************)


IMPORT Strings, FileSys;

FROM SYSTEM IMPORT
    (* type *)  CARD32;

FROM LogCtx IMPORT
    (* var  *)  WCtx;

FROM MSet IMPORT
    (* type *)  MessageSet;

FROM Boxes IMPORT
    (* proc *)  ConsumeCard;

FROM Replies IMPORT
    (* type *)  ReplyCxt,
    (* proc *)  Reply3Untagged, Reply5Untagged;

FROM INIData IMPORT
    (* type *)  HINI,
    (* proc *)  INIValid, CloseINIFile, INIGet, INIPut;

FROM FileOps IMPORT
    (* const*)  NoSuchChannel,
    (* type *)  ChanId, DirectoryEntry,
    (* proc *)  OpenOldFile, OpenNewFile, CloseFile, ReadLine,
                FWriteChar, FWriteString,  FWriteLJCard, FWriteLn,
                FirstDirEntry, NextDirEntry, DirSearchDone;

FROM Parser IMPORT
    (* proc *)  StripLeadingSpaces, Match;

FROM WildCard IMPORT
    (* type *)  CharSet,
    (* proc *)  WildMatchS;

FROM SBuffers IMPORT
    (* type *)  SBuffer,
    (* proc *)  SendLine;

FROM IMAPMisc IMPORT
    (* proc *)  CompareStr, OpenOurINIFile;

FROM Inet2Misc IMPORT
    (* proc *)  ToLower, StringMatch;

FROM Conversions IMPORT
    (* proc *)  CardinalToStringLJ;

FROM TransLog IMPORT
    (* type *)  TransactionLogID,
    (* proc *)  CreateLogID, DiscardLogID, LogTransactionL;

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
    (* Possible LIST responses.  Note that Subscribed is not, strictly  *)
    (* speaking, a LIST response, and it will not be included when we   *)
    (* produce such a response; but it is convenient to include it as   *)
    (* an option here so that we have a mechanism of saving the         *)
    (* "subscribed" information in the boxlist.                         *)

    ListOptions = (Noinferiors, Noselect, Marked, Unmarked, Subscribed);
    ListOptionSet = SET OF ListOptions;

    (* A BoxPtr is a pointer to a single mailbox record.  *)

    BoxPtr = POINTER TO BoxRecord;

    (* A list of all mailboxes for one user.  The 'dir' field is the    *)
    (* full pathname of the root directory of mailboxes.  The head      *)
    (* pointer points to the head of a linear list of all mailboxes for *)
    (* this user; we do not explicitly store the tree structure, but    *)
    (* instead rely on the '/' delimiters in the boxname field to show  *)
    (* the hierarchy.  Our sole concession to the nonlinearity is to    *)
    (* record a 'mother' and 'childcount' field for each mailbox.       *)

    ListOfBoxes = POINTER TO
                      RECORD
                          dir: FilenameString;
                          xhead: BoxPtr;
                      END (*RECORD*);

    (* BoxRecord field names:                                           *)
    (*                                                                  *)
    (*   next         next box in the linear list of boxes              *)
    (*   UIDvalidity  UID validity code for this box                    *)
    (*   deleted      this box has been deleted.  This is a transient   *)
    (*                 condition that is cleared whenever we rescan the *)
    (*                 list of boxes                                    *)
    (*   listoptions  see definition of ListOptions above               *)
    (*   mother       parent of this box                                *)
    (*   childcount   the number of subdirectories of this box          *)
    (*   boxname      name of the mailbox, relative to the mailbox root *)
    (*   fullname     file name of the mailbox as a complete pathname   *)

    BoxRecord = RECORD
                    next: BoxPtr;
                    UIDvalidity: CARDINAL;
                    deleted: BOOLEAN;
                    listoptions: ListOptionSet;
                    childcount: CARDINAL;
                    mother: BoxPtr;
                    boxname, fullname: FilenameString;
                END (*RECORD*);

(************************************************************************)

VAR
    (* Transaction log id for error messages. *)

    StarID: TransactionLogID;

    (* Next UID validity code to assign, and a critical section lock. *)

    NextUIDValidity: CARD32;
    UIDVLock: Lock;

(************************************************************************)
(*                      OPERATIONS ON BOXLISTS                          *)
(************************************************************************)

PROCEDURE WriteMasterBoxListFile (L: ListOfBoxes);  FORWARD;

(************************************************************************)

PROCEDURE DiscardBoxList (VAR (*INOUT*) BP: BoxPtr);

    (* Discards the linear list whose head is BP.  *)

    VAR next: BoxPtr;

    BEGIN
        WHILE BP <> NIL DO
            next := BP^.next;
            DISPOSE (BP);
            BP := next;
        END (*WHILE*);
    END DiscardBoxList;

(************************************************************************)

PROCEDURE CreateBoxList (VAR (*IN*) dir: FilenameString): ListOfBoxes;

    (* Creates a new boxlist. *)

    VAR BL: ListOfBoxes;

    BEGIN
        NEW (BL);
        BL^.dir := dir;
        BL^.xhead := NIL;
        RETURN BL;
    END CreateBoxList;

(************************************************************************)

PROCEDURE CloseBoxList (VAR (*INOUT*) L: ListOfBoxes);

    (* Discards the entire tree whose root is L, doing whatever tidying *)
    (* up is necessary.                                                 *)

    BEGIN
        WriteMasterBoxListFile (L);
        DiscardBoxList (L^.xhead);
        DISPOSE (L);
    END CloseBoxList;

(************************************************************************)

PROCEDURE HeadSplit (VAR (*OUT*) head: ARRAY OF CHAR;
                     VAR (*INOUT*) path: ARRAY OF CHAR);

    (* Finds the first '/' in path.  Returns head as the part before    *)
    (* the '/', and path as the part after.  If there is no '/',        *)
    (* returns with head = original path, and path as the empty string. *)

    VAR pos: CARDINAL;  found: BOOLEAN;

    BEGIN
        Strings.Assign (path, head);
        Strings.FindNext ('/', path, 0, found, pos);
        IF found THEN
            head[pos] := Nul;
            Strings.Delete (path, 0, pos+1);
        ELSE
            path[0] := Nul;
        END (*IF*);
    END HeadSplit;

(************************************************************************)

PROCEDURE BoxPtrOf (L: ListOfBoxes;
                    VAR (*IN*) boxname: ARRAY OF CHAR): BoxPtr;

    (* Returns a pointer to the mailbox in the list whose head is L.    *)
    (* Returns NIL if no box found.                                     *)

    VAR q: BoxPtr;

    BEGIN
        IF L = NIL THEN
            q := NIL;
        ELSE
            q := L^.xhead;
        END (*IF*);
        WHILE (q <> NIL) AND NOT StringMatch (q^.boxname, boxname) DO
            q := q^.next;
        END (*WHILE*);
        RETURN q;
    END BoxPtrOf;

(************************************************************************)

PROCEDURE FindBox (VAR (*INOUT*) L: ListOfBoxes;
                              VAR (*IN*) boxname: ARRAY OF CHAR): BoxPtr;

    (* Returns a pointer to the mailbox in the list whose head is L.    *)
    (* A possible side-effect is the updating of list L.                *)
    (* Returns NIL if no box found.                                     *)

    VAR result: BoxPtr;

    BEGIN
        result := BoxPtrOf (L, boxname);
        IF result = NIL THEN

            (* Allow for a second chance, for the case where this       *)
            (* operation has preceded the building of the boxlist.      *)

            UpdateBoxList (L);
            result := BoxPtrOf (L, boxname);
        END (*IF*);
        RETURN result;
    END FindBox;

(************************************************************************)

PROCEDURE BoxExists (box: BoxPtr): BOOLEAN;

    (* TRUE iff the box is not NIL. *)

    BEGIN
        RETURN (box <> NIL) AND NOT box^.deleted;
    END BoxExists;

(************************************************************************)

PROCEDURE DirectoryOf (BP: BoxPtr;  VAR (*OUT*) directory: FilenameString);

    (* Sets directory to the full directory name of this mailbox. *)

    BEGIN
        IF BP = NIL THEN
            directory := "";
        ELSE
            directory := BP^.fullname;
        END (*IF*);
    END DirectoryOf;

(************************************************************************)
(*                         LISTING MAILBOXES                            *)
(************************************************************************)

PROCEDURE FormatListOptions (opts: ListOptionSet;  includesubscribed: BOOLEAN;
                             VAR (*OUT*) buffer: ARRAY OF CHAR);

    (* Produces a character-string representation of opts.  The buffer  *)
    (* must have space for at least 41 characters, or 53 characters if  *)
    (* includesubscribed is TRUE.                                       *)

    VAR k: CARDINAL;

    BEGIN
        buffer[0] := Nul;
        IF Noinferiors IN opts THEN
            Strings.Append ("\Noinferiors ", buffer);
        END (*IF*);
        IF Noselect IN opts THEN
            Strings.Append ("\Noselect ", buffer);
        END (*IF*);
        IF Marked IN opts THEN
            Strings.Append ("\Marked ", buffer);
        END (*IF*);
        IF Unmarked IN opts THEN
            Strings.Append ("\Unmarked ", buffer);
        END (*IF*);
        IF includesubscribed AND (Subscribed IN opts) THEN
            Strings.Append ("\Subscribed ", buffer);
        END (*IF*);

        (* Compensate for a one-space overrun. *)

        k := LENGTH(buffer);
        IF k > 0 THEN
            buffer[k-1] := Nul;
        END (*IF*);

    END FormatListOptions;

(************************************************************************)

PROCEDURE DecodeListOptions (VAR (*INOUT*) buffer: ARRAY OF CHAR): ListOptionSet;

    (* Converts a character string to a ListOptionSet.  On exit, buffer *)
    (* contains anything left after removing the options.               *)

    VAR result: ListOptionSet;

    BEGIN
        result := ListOptionSet{};
        StripLeadingSpaces (buffer);
        WHILE buffer[0] = '\' DO
            IF Match (buffer, "\Noinferiors") THEN
                INCL (result, Noinferiors);
            ELSIF Match (buffer, "\Noselect") THEN
                INCL (result, Noselect);
            ELSIF Match (buffer, "\Marked") THEN
                INCL (result, Marked);
            ELSIF Match (buffer, "\Unmarked") THEN
                INCL (result, Unmarked);
            ELSIF Match (buffer, "\Subscribed") THEN
                INCL (result, Subscribed);
            END (*IF*);
            StripLeadingSpaces (buffer);
        END (*WHILE*);
        RETURN result;
    END DecodeListOptions;

(************************************************************************)

PROCEDURE SortFlatList (VAR (*INOUT*) head: BoxPtr);

    (* Sorts the list by boxname, as a linear list.  *)

    VAR previous, this, next: BoxPtr;
        changed: BOOLEAN;

    BEGIN
        (* Sort the list by boxname.  Since it's typically short, and   *)
        (* probably sorted to begin with, a ripple sort will be OK.     *)

        IF head <> NIL THEN
            REPEAT
                changed := FALSE;
                previous := NIL;  this := head;  next := this^.next;
                WHILE next <> NIL DO
                    IF CompareStr (this^.boxname, next^.boxname) > 0 THEN
                        IF previous = NIL THEN
                            head := next;
                        ELSE
                            previous^.next := next;
                        END (*IF*);
                        this^.next := next^.next;
                        next^.next := this;
                        changed := TRUE;
                        previous := next;
                    ELSE
                        previous := this;
                        this := next;
                    END (*IF*);
                    next := this^.next;
                END (*WHILE*);
            UNTIL NOT changed;
        END (*IF*);

    END SortFlatList;

(************************************************************************)

PROCEDURE ReadMasterBoxListFile (dir: FilenameString): BoxPtr;

    (* Reads the list of mailboxes from the 'boxlist' file.  The        *)
    (* resulting records have their boxname and fullname fields filled  *)
    (* in, and also their UIDvalidity, deleted, and listoptions fields; *)
    (* but the parent/child information is unresolved at this stage.    *)

    CONST Nul = CHR(0);  CtrlZ = CHR(26);

    VAR head, previous: BoxPtr;
        filename, line: FilenameString;
        cid: ChanId;
        UIDvalidity: CARDINAL;
        listoptions: ListOptionSet;
        node: BoxPtr;

    BEGIN
        previous := NIL;
        head := NIL;
        Strings.Assign (dir, filename);
        Strings.Append ('\boxlist', filename);
        cid := OpenOldFile (filename, FALSE, FALSE);
        IF cid <> NoSuchChannel THEN
            LOOP
                ReadLine (cid, line);
                IF line[0] = CtrlZ THEN
                    EXIT (*LOOP*);
                ELSIF line[0] <> Nul THEN
                    UIDvalidity := ConsumeCard (line);
                    listoptions := DecodeListOptions (line);
                    IF line[0] = Nul THEN
                        LogTransactionL (StarID, "Empty mailbox name encountered");
                    ELSE
                        NEW (node);
                        node^.next := NIL;
                        node^.UIDvalidity := UIDvalidity;
                        node^.deleted := FALSE;
                        node^.listoptions := listoptions;
                        node^.mother := NIL;
                        node^.childcount := 0;
                        node^.boxname := line;
                        node^.fullname := dir;
                        Strings.Append ('\', node^.fullname);
                        Strings.Append (line, node^.fullname);
                        IF previous = NIL THEN
                            head := node;
                        ELSE
                            previous^.next := node;
                        END (*IF*);
                        previous := node;
                    END (*IF*);
                END (*IF*);
            END (*LOOP*);
            CloseFile (cid);
        END (*IF*);

        SortFlatList (head);
        RETURN head;

    END ReadMasterBoxListFile;

(************************************************************************)

PROCEDURE WriteMasterBoxListFile (L: ListOfBoxes);

    (* If possible, writes the information in list to the boxlist file  *)
    (* in directory dir.                                                *)

    VAR cid: ChanId;  dummy: BOOLEAN;
        node: BoxPtr;
        tempname, finalname, subdir: FilenameString;

    BEGIN
        Strings.Assign (L^.dir, finalname);
        Strings.Append ("\boxlist", finalname);
        Strings.Assign (finalname, tempname);
        Strings.Append (".tmp", tempname);
        cid := OpenNewFile (tempname, FALSE);
        node := L^.xhead;
        WHILE node <> NIL DO
            IF NOT node^.deleted THEN
                FWriteLJCard (cid, node^.UIDvalidity);
                FWriteChar (cid, ' ');
                FormatListOptions (node^.listoptions, TRUE, subdir);
                IF subdir[0] <> Nul THEN
                    FWriteString (cid, subdir);
                    FWriteChar (cid, ' ');
                END (*IF*);
                FWriteString (cid, node^.boxname);
                FWriteLn (cid);
            END (*IF*);
            node := node^.next;
        END (*WHILE*);
        CloseFile (cid);
        FileSys.Remove (finalname, dummy);
        FileSys.Rename (tempname, finalname, dummy);
    END WriteMasterBoxListFile;

(************************************************************************)

PROCEDURE RescanDirectory (dir, prefix: FilenameString;
                           VAR (*OUT*) tail: BoxPtr;
                                       parent: BoxPtr): BoxPtr;

    (* Creates a linear list of subdirectories of the given directory.  *)
    (* Also sets 'tail' to the tail of the list we have created.  This  *)
    (* is the only procedure that works out the parent/child links.     *)

    VAR head, previous, newtail: BoxPtr;
        success: BOOLEAN;  D: DirectoryEntry;
        mask, subdir, subprefix: FilenameString;

    BEGIN
        head := NIL;  tail := NIL;
        Strings.Assign (dir, mask);
        Strings.Append ('\*', mask);
        success := FirstDirEntry (mask, TRUE, FALSE, D);
        WHILE success DO

            (* Remember to ignore the '.' and '..' entries. *)

            IF (D.name[0] <> '.') OR (
                      (D.name[1] <> Nul)
                       AND ((D.name[1] <> '.') OR (D.name[2] <> Nul))
                                     ) THEN
                previous := tail;
                NEW (tail);
                tail^.next := NIL;
                tail^.UIDvalidity := 0;
                tail^.deleted := FALSE;
                tail^.listoptions := ListOptionSet{};
                tail^.mother := parent;
                tail^.childcount := 0;
                tail^.boxname := prefix;
                Strings.Append (D.name, tail^.boxname);
                tail^.fullname := dir;
                Strings.Append ('\', tail^.fullname);
                Strings.Append (D.name, tail^.fullname);
                IF parent <> NIL THEN
                    INC (parent^.childcount);
                END (*IF*);
                IF previous = NIL THEN
                    head := tail;
                ELSE
                    previous^.next := tail;
                END (*IF*);

                (* Now deal with the subdirectories of this directory. *)

                subdir := dir;
                Strings.Append ('\', subdir);
                Strings.Append (D.name, subdir);
                subprefix := prefix;
                Strings.Append (D.name, subprefix);
                Strings.Append ('/', subprefix);
                tail^.next := RescanDirectory (subdir, subprefix, newtail, tail);
                IF newtail <> NIL THEN
                    tail := newtail;
                END (*IF*);

            END (*IF*);
            success := NextDirEntry (D);
        END (*WHILE*);

        DirSearchDone (D);
        RETURN head;

    END RescanDirectory;

(************************************************************************)

PROCEDURE MergeLists (VAR (*INOUT*) main, sub: BoxPtr;
                                          VAR (*INOUT*) changed: BOOLEAN);

    (* Combines two lists to produce a single one.  On return main      *)
    (* contains all the information and sub is empty.  Parameter        *)
    (* changed is TRUE if the operation alters main, or if it was       *)
    (* already TRUE on entry.  We assume that the main list is the one  *)
    (* with our previous knowledge about the mail folders, and that the *)
    (* sub list is the one with reliable parent pointers.               *)

    VAR mprev, mcurrent, mnext, scurrent: BoxPtr;
        test: INTEGER;

    BEGIN
        mprev := NIL;  mcurrent := main;
        scurrent := sub;
        changed := FALSE;
        WHILE scurrent <> NIL DO

            (* Remove entries with the 'deleted' flag set.  *)

            WHILE (mcurrent <> NIL) AND (mcurrent^.deleted) DO
                mnext := mcurrent^.next;
                IF mprev = NIL THEN
                    main := mnext;
                ELSE
                    mprev^.next := mnext;
                END (*IF*);
                DISPOSE (mcurrent);
                mcurrent := mnext;
                changed := TRUE;
            END (*WHILE*);

            IF mcurrent = NIL THEN
                test := +1;
            ELSE
                test := CompareStr (mcurrent^.boxname,
                                        scurrent^.boxname);
            END (*IF*);

            IF test < 0 THEN

                (* main has a record not in sub.  Note that this *)
                (* means a deleted directory.                    *)

                INCL (mcurrent^.listoptions, Noselect);
                mcurrent^.deleted := TRUE;
                mprev := mcurrent;  mcurrent := mcurrent^.next;

            ELSIF test = 0 THEN

                (* Matching records.  We retain the copy in the sub list *)
                (* since it is the sub list that is correctly threaded,  *)
                (* but we retain the listoptions and UIDvalidity values  *)
                (* from the main list.                                   *)

                scurrent^.listoptions := scurrent^.listoptions
                                            + mcurrent^.listoptions;
                IF mcurrent^.UIDvalidity <> 0 THEN
                    scurrent^.UIDvalidity := mcurrent^.UIDvalidity;
                END (*IF*);
                sub := scurrent^.next;
                scurrent^.next := mcurrent^.next;
                DISPOSE (mcurrent);
                IF mprev = NIL THEN
                    main := scurrent;
                ELSE
                    mprev^.next := scurrent;
                END (*IF*);

                (* Now move on to the next entry in the main list. *)

                mprev := scurrent;  mcurrent := scurrent^.next;
                scurrent := sub;

            ELSE

                (* sub list has a record not in main list. *)

                IF mprev = NIL THEN
                    main := scurrent;
                    scurrent := NIL;
                ELSE
                    mprev^.next := scurrent;
                    sub := scurrent^.next;
                    mprev := scurrent;
                    scurrent := sub;
                    mprev^.next := mcurrent;
                END (*IF*);
                changed := TRUE;

            END (*IF*);

        END (*WHILE*);

    END MergeLists;

(************************************************************************)

PROCEDURE AssignUIDvalidityCodes (p: BoxPtr): BOOLEAN;

    (* Runs through the list ensuring that all entries have nonzero  *)
    (* UIDvalidity entries.  Returnes TRUE if we have to assign      *)
    (* any new codes.                                                *)

    VAR changed: BOOLEAN;  hini: HINI;
        app: ARRAY [0..4] OF CHAR;

    BEGIN
        changed := FALSE;
        Obtain (UIDVLock);
        WHILE p <> NIL DO
            IF p^.UIDvalidity = 0 THEN
                p^.UIDvalidity := NextUIDValidity;
                INC (NextUIDValidity);
                changed := TRUE;
            END (*IF*);
            p := p^.next;
        END (*WHILE*);
        IF changed THEN
            hini := OpenOurINIFile();
            IF INIValid(hini) THEN
                app := "$SYS";
                INIPut (hini, app, "NextUIDValidity", NextUIDValidity);
                CloseINIFile (hini);
            END (*IF*);
        END (*IF*);
        Release (UIDVLock);
        RETURN changed;
    END AssignUIDvalidityCodes;

(************************************************************************)

PROCEDURE UpdateBoxList (VAR (*INOUT*) BL: ListOfBoxes);

    (* Brings our information about this list up to date. *)

    VAR p, q, tail: BoxPtr;  changed: BOOLEAN;

    BEGIN
        (* Phase 1: reconcile the current set of subdirectories with    *)
        (* the information in the 'boxlist' file.                       *)

        p := ReadMasterBoxListFile (BL^.dir);
        q := RescanDirectory (BL^.dir, "", tail, NIL);
        SortFlatList (q);
        changed := FALSE;
        MergeLists (p, q, changed);
        changed := AssignUIDvalidityCodes(p) OR changed;

        (* Phase 2: update the information in BL from this. *)

        MergeLists (BL^.xhead, p, changed);
        IF changed THEN
            WriteMasterBoxListFile (BL);
        END (*IF*);

    END UpdateBoxList;

(************************************************************************)
(*                          OPERATIONS ON USERS                         *)
(************************************************************************)

PROCEDURE ListTo (RC: ReplyCxt;  L: ListOfBoxes;
                  VAR (*IN*) dir, mask: FilenameString;  IncludeAll: BOOLEAN);

    (* Sends the list to the client defined by RC.                      *)
    (* The mask parameter may include wildcards.  In the mask, '*'      *)
    (* matches anything including the hierarchy delimiter, and '%' is   *)
    (* like '*' except that it doesn't match the hierarchy delimiter.   *)

    VAR q: BoxPtr;
        optionbuffer: ARRAY [0..63] OF CHAR;
        fullname: FilenameString;

    BEGIN
        IF L = NIL THEN
            q := NIL;
        ELSE
            q := L^.xhead;
        END (*IF*);
        ToLower (mask);
        WHILE q <> NIL DO
            Strings.Assign (dir, fullname);
            IF dir[0] <> Nul THEN
                Strings.Append ('/', fullname);
            END (*IF*);
            Strings.Append (q^.boxname, fullname);
            IF (IncludeAll OR (Subscribed IN q^.listoptions))
                       AND WildMatchS (fullname, mask, CharSet{'/'}) THEN
                FormatListOptions (q^.listoptions, FALSE, optionbuffer);
                Reply5Untagged (RC, 'LIST (', optionbuffer,
                                                 ') "/" "', fullname, '"');
            END (*IF*);
            q := q^.next;
        END (*WHILE*);
    END ListTo;

(************************************************************************)

PROCEDURE UIDValidityResponse (RC: ReplyCxt;  box: BoxPtr);

    (* Sends an untagged OK UIDVALIDITY response to the client. *)

    VAR number: ARRAY [0..15] OF CHAR;

    BEGIN
        IF box <> NIL THEN
            CardinalToStringLJ (box^.UIDvalidity, number);
            Reply3Untagged (RC, 'OK [UIDVALIDITY ', number, ']');
        END (*IF*);
    END UIDValidityResponse;

(************************************************************************)

PROCEDURE UIDValidityCode (box: BoxPtr): CARDINAL;

    (* Returns the UIDVALIDITY code for this mailbox.  The result is    *)
    (* zero if the box cannot be found.                                 *)

    BEGIN
        IF box = NIL THEN
            RETURN 0;
        ELSE
            RETURN box^.UIDvalidity;
        END (*IF*);
    END UIDValidityCode;

(************************************************************************)
(*                      OPERATIONS ON MAILBOXES                         *)
(************************************************************************)

PROCEDURE DoSubscribe (M: BoxPtr): BOOLEAN;

    (* Subscribes to this box. *)

    BEGIN
        IF (M = NIL) OR M^.deleted THEN
            RETURN FALSE;
        ELSE
            INCL (M^.listoptions, Subscribed);
            RETURN TRUE;
        END (*IF*);
    END DoSubscribe;

(************************************************************************)

PROCEDURE DoUnsubscribe (M: BoxPtr): BOOLEAN;

    (* Subscribes to this box. *)

    BEGIN
        IF (M = NIL) OR M^.deleted THEN
            RETURN FALSE;
        ELSE
            EXCL (M^.listoptions, Subscribed);
            RETURN TRUE;
        END (*IF*);
    END DoUnsubscribe;

(************************************************************************)

PROCEDURE DeleteAllMessages (dir: FilenameString);

    (* Deletes all files in this directory. *)

    VAR mask, file: FilenameString;
        D: DirectoryEntry;
        found, dummy: BOOLEAN;

    BEGIN
        mask := dir;
        Strings.Append ("\*", mask);
        found := FirstDirEntry (mask, FALSE, TRUE, D);
        WHILE found DO

            (* Remember to ignore the '.' and '..' entries. *)

            IF (D.name[0] <> '.') OR (
                      (D.name[1] <> Nul)
                       AND ((D.name[1] <> '.') OR (D.name[2] <> Nul))
                                     ) THEN
                file := dir;
                Strings.Append ("\", file);
                Strings.Append (D.name, file);
                FileSys.Remove (file, dummy);
            END (*IF*);
            found := NextDirEntry (D);
        END (*WHILE*);
        DirSearchDone (D);
    END DeleteAllMessages;

(************************************************************************)

PROCEDURE RemoveBox (L: ListOfBoxes;
                     VAR (*IN*) boxname: ARRAY OF CHAR): BOOLEAN;

    (* Deletes a mailbox, including deleting all the files it contains. *)
    (* If it has subdirectories, it acquires the \Noselect attribute.   *)
    (* If it has subdirectories and already has the \Noselect attribute *)
    (* the deletion attempt will fail.                                  *)

    VAR B: BoxPtr;

    BEGIN
        B := FindBox (L, boxname);
        IF (B = NIL) OR (B^.deleted) THEN
            RETURN FALSE;
        ELSIF B^.childcount > 0 THEN
            IF Noselect IN B^.listoptions THEN
                RETURN FALSE;
            ELSE
                INCL (B^.listoptions, Noselect);
                DeleteAllMessages (B^.fullname);
                RETURN TRUE;
            END (*IF*);
        ELSE
            DeleteAllMessages (B^.fullname);
            IF (B^.mother <> NIL) AND (B^.mother^.childcount > 0) THEN
                DEC (B^.mother^.childcount);
            END (*IF*);
            B^.deleted := TRUE;
            RETURN FileSys.RemoveDirectory (B^.fullname);
        END (*IF*);
    END RemoveBox;

(************************************************************************)

PROCEDURE DoRename (L: ListOfBoxes;
                    VAR (*INOUT*) oldname, newname: ARRAY OF CHAR);

    (* Renames a mailbox, and all inferior boxes, keeping the flags.    *)
    (* We assume that the caller is taking care of the file system      *)
    (* directory restructuring; this procedure is purely concerned with *)
    (* updating our internal list of mailboxes.                         *)

    VAR p: BoxPtr;
        test, newboxname: FilenameString;

    BEGIN
         p := L^.xhead;
         WHILE p <> NIL DO
             test := p^.boxname;

             IF Match (test, oldname) THEN
                 Strings.Assign (newname, newboxname);
                 IF test[0] <> Nul THEN

                     (* This is an inferior box. *)

                     Strings.Append ('/', newboxname);
                     Strings.Append (test, newboxname);
                 END (*IF*);

                 p^.boxname := newboxname;
                 p^.fullname := L^.dir;
                 Strings.Append ('\', p^.fullname);
                 Strings.Append (newboxname, p^.fullname);

             END (*IF*);

             p := p^.next;

         END (*WHILE*);

    END DoRename;

(************************************************************************)
(*                           INITIALISATION                             *)
(************************************************************************)

PROCEDURE InitUIDValidity;

    (* Sets the value of variable NextUIDValidity. *)

    VAR hini: HINI;
        app: ARRAY [0..4] OF CHAR;

    BEGIN
        hini := OpenOurINIFile();
        IF INIValid(hini) THEN
            app := "$SYS";
            IF NOT INIGet (hini, app, "NextUIDValidity", NextUIDValidity) THEN
                NextUIDValidity := 1;
            END (*IF*);
            CloseINIFile (hini);
        END (*IF*);
    END InitUIDValidity;

(************************************************************************)

BEGIN
    StarID := CreateLogID (WCtx, "*      ");
    CreateLock (UIDVLock);
FINALLY
    DiscardLogID (StarID);
    DestroyLock (UIDVLock);
END BoxLists.

