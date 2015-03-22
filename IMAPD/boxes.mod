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

IMPLEMENTATION MODULE Boxes;

        (********************************************************)
        (*                                                      *)
        (*                      Mailboxes                       *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            6 March 2003                    *)
        (*  Last edited:        12 January 2014                 *)
        (*  Status:             Almost completely implemented   *)
        (*                                                      *)
        (*       Possible bug: we might not be correctly        *)
        (*       maintaining the parent/child relationship      *)
        (*       when we merge folder listings.  I believe,     *)
        (*       though, that this is now fixed.                *)
        (*                                                      *)
        (*     NOTE: The IMAP4rev1 standard allows for          *)
        (*     mailbox names in a modified UTF-7 code, but this *)
        (*     server does not implement that feature.  For     *)
        (*     now, we support only mailbox names in 7-bit      *)
        (*     ASCII code.  In fact this might turn out to be   *)
        (*     the correct strategy even in the long term.  The *)
        (*     clients refer to the boxes using that code, so   *)
        (*     in effect we have complete UTF support by just   *)
        (*     using the coded names at the server end.         *)
        (*                                                      *)
        (********************************************************)

(********************************************************************************)
(*                                                                              *)
(* SOME RELEVANT RULES FROM THE STANDARD:                                       *)
(*                                                                              *)
(*  Server implementations are permitted to send an untagged response           *)
(*  (except for EXPUNGE) while there is no command in progress.                 *)
(*                                                                              *)
(*  5.2  A server MUST send mailbox size updates automatically                  *)
(*   if a mailbox size change is observed during the processing of a            *)
(*   command.  A server SHOULD send message flag updates automatically,         *)
(*                                                                              *)
(*  6.1.2 example:                                                              *)
(*               C: a047 NOOP                                                   *)
(*               S: * 22 EXPUNGE                                                *)
(*               S: * 23 EXISTS                                                 *)
(*               S: * 3 RECENT                                                  *)
(*               S: * 14 FETCH (FLAGS (\Seen \Deleted))                         *)
(*               S: a047 OK NOOP completed                                      *)
(*                                                                              *)
(*  7. Server implementations that offer multiple simultaneous access to the    *)
(*  same mailbox SHOULD also send appropriate unilateral untagged FETCH and     *)
(*  EXPUNGE responses if another agent changes the state of any message flags   *)
(*  or expunges any messages.                                                   *)
(*                                                                              *)
(*  This seems to contradict 5.2, so I'm not sure what to do about EXPUNGE.     *)
(*  Section 7.4.1 seems to say quite explicitly that EXPUNGE responses MUST NOT *)
(*  be sent when no command is in progress, nor when responding to FETCH,       *)
(*  STORE, or SEARCH.  From this I conclude that it's best to send EXPUNGE      *)
(*  responses only in response to NOOP or EXPUNGE commands.                     *)
(*                                                                              *)
(*  (I have now checked the entire standard for these details.)                 *)
(*                                                                              *)
(*------------------------------------------------------------------------------*)
(*                                                                              *)
(*  The following commands might change the mailbox contents:                   *)
(*     A. Already dealt with                                                    *)
(*           APPEND, STORE, COPY                                                *)
(*                                                                              *)
(*------------------------------------------------------------------------------*)
(*                                                                              *)
(* Commands that I have to come back to:                                        *)
(*     FETCH (implement the missing options)                                    *)
(*     NOOP  (mostly for the EXPUNGE response, if wanted)                       *)
(*                                                                              *)
(********************************************************************************)

IMPORT Strings, FileSys, OS2;

FROM SYSTEM IMPORT
    (* type *)  CARD32,
    (* proc *)  ADR;

FROM MSet IMPORT
    (* type *)  MessageSet,
    (* proc *)  GetSet, NonEmptySet, NextMemberFrom, DiscardSet;

FROM Messages IMPORT
    (* type *)  BodyInfo, StructureData,
    (* proc *)  InitBodyInfo, DiscardBodyInfo, SizeOfMessage, SendSection,
                FindInHeader, FindInText, PackedDate, PackedDateTime, HeaderDate,
                ReportBodyStructure, DiscardStructureData, ReturnEnvelope,
                ParseFetchSection;

FROM FileOps IMPORT
    (* const*)  NoSuchChannel,
    (* type *)  ChanId, DirectoryEntry,
    (* proc *)  OpenOldFile, OpenNewFile, CloseFile, ReadLine,
                FWriteChar, FWriteString, FWriteLJCard, FWriteLn,
                FirstDirEntry, NextDirEntry, DirSearchDone, HideFile,
                CopyFile, DeleteFile, OpenNewFile1, OpenNewHiddenFile;

FROM Replies IMPORT
    (* type *)  ReplyCxt,
    (* proc *)  Reply, PartialReply, CommitReply, PartialReplyUntagged, ReplyUntagged,
                Reply2Untagged, Reply3Untagged, AcceptLine,
                SuppressLogging, RestoreLogging;

FROM Parser IMPORT
    (* type *)  MessageFlag, MessageFlagSet,
    (* proc *)  Match, Number, SPACE, AString, Atom, Date, PackDate,
                StripLeadingSpaces, DecodeFlags;

FROM IMAPMisc IMPORT
    (* proc *)  CompareStr;

FROM Inet2Misc IMPORT
    (* proc *)  EVAL, ConvertCard;

FROM Conversions IMPORT
    (* proc *)  CardinalToStringLJ, Card64ToStringLJ;

FROM MyClock IMPORT
    (* proc *)  PackedDateTimeToString;

FROM Names IMPORT
    (* type *)  FilenameString, ArgPointer;

FROM Types IMPORT
    (* type *)  CARD64;

FROM TransLog IMPORT
    (* type *)  TransactionLogID,
    (* proc *)  LogTransactionL;

FROM TaskControl IMPORT
    (* type *)  Lock,
    (* proc *)  CreateLock, DestroyLock, Obtain, Release;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

(************************************************************************)

CONST
    Nul = CHR(0);

TYPE
    EightChar = ARRAY [0..7] OF CHAR;
    ItemPtr = POINTER TO ItemRecord;
    ListOfItems = ItemPtr;

    (********************************************************************)
    (*                                                                  *)
    (* Mail items in a mailbox.  The fields are:                        *)
    (*                                                                  *)
    (*    next    next item in the linear list of items in this box     *)
    (*    seqno   sequence number of this message                       *)
    (*    UID     unique identifier of this message                     *)
    (*    flags   flags that apply to this message                      *)
    (*    absent  a file that no longer exists in the mailbox,          *)
    (*              although we still have an internal record of it     *)
    (*    shortname the EightChar significant part of the filename      *)
    (*    structure the MIME structure of the message                   *)
    (*                                                                  *)
    (* According to the standard, the sequence numbers must be in       *)
    (* numerical order with no gaps, starting from 1, which means that  *)
    (* the sequence numbers change when items are removed from the      *)
    (* mailbox.  The UID, on the other hand, must persist across        *)
    (* sessions.  Inevitably there will be gaps in the UID numbers, as  *)
    (* messages are moved or deleted, but sorting the list by UID and   *)
    (* sorting it by sequence number must yield the same result.        *)
    (*                                                                  *)
    (* The structure field is NIL when the item list is first           *)
    (* constructed.  It is never needed except when processing a        *)
    (* FETCH instruction, at which point the MIME structure is parsed.  *)
    (* In the present version the structure data are then discarded,    *)
    (* to save memory.  It is possible that in a future version I will  *)
    (* save the structure for a possible next FETCH operation; this is  *)
    (* a matter of trading space for time.                              *)
    (*                                                                  *)
    (********************************************************************)

    ItemRecord = RECORD
                    next: ItemPtr;
                    seqno: CARDINAL;
                    UID: CARD32;
                    flags: MessageFlagSet;
                    absent: BOOLEAN;
                    shortname: EightChar;
                    structure: StructureData;
                END (*RECORD*);

    (********************************************************************)
    (*                                                                  *)
    (* Difference between 'absent' and the flag \Deleted:               *)
    (*                                                                  *)
    (* The absent field is a temporary work variable that we use to     *)
    (* detect a message file that has disappeared (probably because     *)
    (* a non-IMAP agent has removed it).  In the present version such   *)
    (* entries occur only transiently during a directory scan, after    *)
    (* which we remove the record from our master list.                 *)
    (*                                                                  *)
    (* The \Deleted flag means that an IMAP client has marked the       *)
    (* message for deletion; but the message file still exists until    *)
    (* an EXPUNGE makes the deletion permanent.                         *)
    (*                                                                  *)
    (********************************************************************)

    MailboxUser = POINTER TO ClientRecord;

    (********************************************************************)
    (*                                                                  *)
    (* Summary information for a mailbox.  The fields are:              *)
    (*                                                                  *)
    (*   changed     the itemlist has changed since the last time       *)
    (*                  it was written back to disk                     *)
    (*   IsInbox     this is the INBOX mailbox for this user            *)
    (*   next        next mailbox in the list of all open mailboxes     *)
    (*   access      critical section lock for access to this box       *)
    (*   voyeurs     head of a list of clients who have opened this     *)
    (*                  mailbox                                         *)
    (*   RWusers     number of voyeurs who have opened it read-write    *)
    (*   count       the number of messages now in the mailbox          *)
    (*   recent      the number of messages that have arrived since the *)
    (*                  last time we built the 'itemlist' file          *)
    (*   unseen      the number of messages for which                   *)
    (*                  the \Seen flag is not set                       *)
    (*   firstunseen sequence number of the first message for which     *)
    (*                  the \Seen flag is not set                       *)
    (*   NextUID     the UID to be assigned to the next message to be   *)
    (*                  found in this mailbox                           *)
    (*   NextUIDLock critical section protection for NextUID            *)
    (*   fdir        directory where message files are kept             *)
    (*   itemlist    head of a linear list of items                     *)
    (*                                                                  *)
    (********************************************************************)

    Mailbox = POINTER TO MailboxRecord;
    MailboxRecord = RECORD
                        changed: BOOLEAN;
                        IsInbox: BOOLEAN;
                        next: Mailbox;
                        access: Lock;
                        voyeurs: MailboxUser;
                        RWusers: CARDINAL;
                        count, recent, unseen, firstunseen: CARDINAL;
                        NextUID: CARD32;
                        NextUIDLock: Lock;
                        fdir: FilenameString;
                        itemlist: ItemPtr;
                    END (*RECORD*);

    (********************************************************************)
    (*                                                                  *)
    (* The record to which a MailboxUser pointer points.  This is how   *)
    (* we obtain access to a mailbox after opening it.                  *)
    (*                                                                  *)
    (*   next        next user who has opened the same mailbox          *)
    (*   box         the mailbox we have opened                         *)
    (*   readonly    TRUE iff we have opened this box read-only         *)
    (*   RC          reply context (log id, SBuffer id)                 *)
    (*   count       the value that box^.count had the last time        *)
    (*                  we reported its value to this client            *)
    (*                                                                  *)
    (********************************************************************)

    ClientRecord = RECORD
                       next: MailboxUser;
                       box: Mailbox;
                       readonly: BOOLEAN;
                       RC: ReplyCxt;
                       count: CARDINAL;
                   END (*RECORD*);

(************************************************************************)

VAR
    (* String used in creating a unique file name. *)

    NextName: EightChar;
    NextNameLock: Lock;

    (* A list of all currently open mailboxes. *)

    MasterBoxList: RECORD
                       access: Lock;
                       head: Mailbox;
                   END (*RECORD*);

(************************************************************************)
(*        THE EightChar REPRESENTATION OF A MESSAGE FILE NAME           *)
(*                                                                      *)
(*  For efficiency, we require that all message file names be of the    *)
(*  form *.MSG, where the * part is at most eight characters long.      *)
(************************************************************************)

PROCEDURE StringToEightChar (VAR (*IN*) str: ARRAY OF CHAR;
                             VAR (*OUT*) result: EightChar);

    (* Strips the ".MSG" part from the string. *)

    VAR pos: CARDINAL;  found: BOOLEAN;

    BEGIN
        Strings.FindNext ('.', str, 0, found, pos);
        IF NOT found THEN
            pos := Strings.Length(str);
        END (*IF*);
        IF pos > 8 THEN
            pos := 8;
        END (*IF*);
        Strings.Extract (str, 0, pos, result);

    END StringToEightChar;

(************************************************************************)

PROCEDURE MakeFullName (VAR (*IN*) dir: ARRAY OF CHAR;
                        shortname: EightChar;
                        VAR (*OUT*) result: ARRAY OF CHAR);

    BEGIN
        Strings.Assign (dir, result);
        Strings.Append ("\", result);
        Strings.Append (shortname, result);
        Strings.Append (".MSG", result);
    END MakeFullName;

(************************************************************************)
(*                       SORTING A LIST OF ITEMS                        *)
(************************************************************************)

PROCEDURE SortList (head: ItemPtr): ItemPtr;

    (* Sorts the list using the UID as the primary key and the name as  *)
    (* a secondary key.  Special case: a UID of 0 means that this is a  *)
    (* new message, and we always sort such messages to the end of the  *)
    (* list.                                                            *)

    VAR previous, this, next: ItemPtr;
        mustswap, changed: BOOLEAN;

    BEGIN
        (* Sort the list by UID/name.  Since it's typically short, and  *)
        (* probably sorted to begin with, a ripple sort will be OK.     *)

        IF head <> NIL THEN
            changed := FALSE;
            REPEAT
                previous := NIL;  this := head;  next := head^.next;
                WHILE next <> NIL DO
                    IF this^.UID = next^.UID THEN
                        mustswap := CompareStr (this^.shortname, next^.shortname) > 0;
                    ELSIF this^.UID = 0 THEN
                        mustswap := TRUE;
                    ELSE
                        mustswap := this^.UID > next^.UID;
                    END (*IF*);
                    IF mustswap THEN
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

        RETURN head;

    END SortList;

(************************************************************************)
(*                     READING THE ITEMLIST FILE                        *)
(************************************************************************)

PROCEDURE ConsumeCard (VAR (*INOUT*) buffer: ARRAY OF CHAR): CARDINAL;

    (* Converts a character string to a number, also stripping spaces.  *)
    (* On exit, buffer contains anything left after removing the        *)
    (* characters we have used.                                         *)

    TYPE CharSet = SET OF CHAR;
    CONST Digits = CharSet {'0'..'9'};

    VAR k, result: CARDINAL;

    BEGIN
        StripLeadingSpaces (buffer);
        result := 0;  k := 0;
        WHILE (k <= HIGH(buffer)) AND (buffer[k] IN Digits) DO
            result := 10*result + (ORD(buffer[k]) - ORD('0'));
            INC (k);
        END (*WHILE*);
        IF k > 0 THEN
            Strings.Delete (buffer, 0, k);
        END (*IF*);
        StripLeadingSpaces (buffer);
        RETURN result;
    END ConsumeCard;

(************************************************************************)

PROCEDURE ReadItemListFile (VAR (*IN*) dir: FilenameString;
                               VAR (*OUT*) nextUID: CARD32;
                               VAR (*OUT*) unseen: CARDINAL;
                               VAR (*OUT*) firstunseen: CARDINAL): ItemPtr;

    (* Reads the list of files from the 'itemlist' file.  Directory     *)
    (* dir is where the itemlist file is.  This is normally the same as *)
    (* where the files are, but can be different in the special case of *)
    (* the INBOX directory.  We set the 'absent' flag for each entry,   *)
    (* in the expectation that that flag will be cleared once we do a   *)
    (* comparison with the directory listing.                           *)

    CONST Nul = CHR(0);  CtrlZ = CHR(26);

    VAR head, previous, this: ItemPtr;
        filename, line: FilenameString;
        cid: ChanId;
        count: CARDINAL;

    BEGIN
        head := NIL;  previous := NIL;  count := 0;
        nextUID := 1;  unseen := 0;  firstunseen := 0;
        Strings.Assign (dir, filename);
        Strings.Append ('\itemlist', filename);
        cid := OpenOldFile (filename, FALSE, FALSE);
        IF cid <> NoSuchChannel THEN
            LOOP
                ReadLine (cid, line);
                IF line[0] = CtrlZ THEN
                    EXIT (*LOOP*);
                ELSIF line[0] <> Nul THEN
                    INC (count);
                    NEW (this);
                    IF this = NIL THEN
                        EXIT (*LOOP*);
                    END (*IF*);
                    this^.next := NIL;
                    this^.structure := NIL;
                    this^.flags := MessageFlagSet {};
                    this^.absent := TRUE;
                    this^.seqno := count;
                    this^.UID := ConsumeCard (line);
                    this^.flags := this^.flags + DecodeFlags(line);
                    IF line[0] = Nul THEN

                        (* Assume this is the "nextUID" entry. *)

                        nextUID := this^.UID;
                        DISPOSE (this);
                        DEC (count);

                    ELSE
                        StringToEightChar (line, this^.shortname);
                        IF NOT (seen IN this^.flags) THEN
                            INC (unseen);
                            IF firstunseen = 0 THEN
                                firstunseen := this^.seqno;
                            END (*IF*);
                        END (*IF*);
                        IF previous = NIL THEN
                            head := this;
                        ELSE
                            previous^.next := this;
                        END (*IF*);
                        previous := this;
                    END (*IF*);
                END (*IF*);
            END (*LOOP*);
            CloseFile (cid);
        END (*IF*);

        RETURN SortList(head);

    END ReadItemListFile;

(************************************************************************)
(*                     WRITING THE ITEMLIST FILE                        *)
(************************************************************************)

PROCEDURE DeleteLastSpace (VAR (*INOUT*) buffer: ARRAY OF CHAR);

    (* If buffer finishes with a space character, remove it.   *)

    VAR L: CARDINAL;

    BEGIN
        L := Strings.Length (buffer);
        IF L > 0 THEN
            DEC (L);
            IF buffer[L] = ' ' THEN
                buffer[L] := Nul;
            END (*IF*);
        END (*IF*);
    END DeleteLastSpace;

(************************************************************************)

PROCEDURE EncodeFlags (flags: MessageFlagSet;  VAR (*OUT*) buffer: ARRAY OF CHAR;
                                        IgnoreRecent: BOOLEAN);

    (* Writes the flags to the buffer, with a space character      *)
    (* after each flag except the last one.                        *)

    BEGIN
        buffer[0] := Nul;
        IF seen IN flags THEN
            Strings.Append ('\Seen ', buffer);
        END (*IF*);
        IF answered IN flags THEN
            Strings.Append ('\Answered ', buffer);
        END (*IF*);
        IF flagged IN flags THEN
            Strings.Append ('\Flagged ', buffer);
        END (*IF*);
        IF deleted IN flags THEN
            Strings.Append ('\Deleted ', buffer);
        END (*IF*);
        IF draft IN flags THEN
            Strings.Append ('\Draft ', buffer);
        END (*IF*);
        IF (recent IN flags) AND NOT IgnoreRecent THEN
            Strings.Append ('\Recent ', buffer);
        END (*IF*);

        (* Correct for a one-space overshoot. *)

        DeleteLastSpace (buffer);

    END EncodeFlags;

(************************************************************************)

PROCEDURE WriteFlags (cid: ChanId;  item: ItemPtr;  IgnoreRecent: BOOLEAN);

    (* Writes the flags to the output file, with a space character      *)
    (* after each flag.                                                 *)

    VAR buffer: ARRAY [0..127] OF CHAR;

    BEGIN
        EncodeFlags (item^.flags, buffer, IgnoreRecent);
        FWriteString (cid, buffer);
        FWriteChar (cid, ' ');
    END WriteFlags;

(************************************************************************)

PROCEDURE WriteItemListFile (M: Mailbox);

    (* If possible, writes the information in list to the itemlist file *)
    (* for this mailbox.  We assume that we already have exclusive      *)
    (* access to the mailbox.                                           *)

    VAR cid: ChanId;  dummy: BOOLEAN;
        tempname, finalname: FilenameString;
        list: ItemPtr;

    BEGIN
        (* In this version, ignore exclusive access issue for now,      *)
        (* except to the extent that we lock the nextUID lock while     *)
        (* writing the file.  In fact I believe that exclusive access   *)
        (* is guaranteed by the caller, but I need to check this.       *)

        Obtain (M^.NextUIDLock);

        Strings.Assign (M^.fdir, finalname);
        IF M^.IsInbox THEN
            Strings.Append ("\INBOX", finalname);
        END (*IF*);
        Strings.Append ("\itemlist", finalname);
        Strings.Assign (finalname, tempname);
        Strings.Append (".tmp", tempname);
        cid := OpenNewFile (tempname, FALSE);
        FWriteLJCard (cid, M^.NextUID);
        FWriteLn (cid);
        list := M^.itemlist;
        WHILE list <> NIL DO
            IF NOT list^.absent THEN
                FWriteLJCard (cid, list^.UID);
                FWriteChar (cid, ' ');
                WriteFlags (cid, list, TRUE);
                FWriteString (cid, list^.shortname);
                FWriteLn (cid);
            END (*IF*);
            list := list^.next;
        END (*WHILE*);
        CloseFile (cid);
        FileSys.Remove (finalname, dummy);
        FileSys.Rename (tempname, finalname, dummy);

        Release (M^.NextUIDLock);
        M^.changed := FALSE;

    END WriteItemListFile;

(************************************************************************)
(*                    UPDATING THE LIST OF ITEMS                        *)
(************************************************************************)

PROCEDURE RescanDirectory (dir: FilenameString): ItemPtr;

    (* Creates a list of message file names in the given directory. *)

    VAR head, previous, this: ItemPtr;
        success: BOOLEAN;  D: DirectoryEntry;
        mask: FilenameString;
        count: CARDINAL;

    BEGIN
        head := NIL;  previous := NIL;  count := 0;
        Strings.Assign (dir, mask);
        Strings.Append ('\*.MSG', mask);
        success := FirstDirEntry (mask, FALSE, FALSE, D);
        WHILE success DO
            INC (count);
            NEW (this);
            this^.next := NIL;
            this^.flags := MessageFlagSet {recent};
            this^.absent := FALSE;
            this^.seqno := count;
            this^.UID := 0;
            StringToEightChar (D.name, this^.shortname);
            this^.structure := NIL;
            IF previous = NIL THEN
                head := this;
            ELSE
                previous^.next := this;
            END (*IF*);
            previous := this;
            success := NextDirEntry (D);
        END (*WHILE*);

        DirSearchDone (D);
        RETURN SortList(head);

    END RescanDirectory;

(************************************************************************)

PROCEDURE DeleteKnown (main: ItemPtr;  VAR (*INOUT*) sub: ItemPtr);

    (* Deletes from sub files that are already in main.  *)

    VAR mcurrent, sprevious, scurrent, snext: ItemPtr;
        match: BOOLEAN;

    BEGIN
        sprevious := NIL;  scurrent := sub;
        WHILE scurrent <> NIL DO

            (* Outer loop works through the 'sub' list. *)

            mcurrent := main;
            match := FALSE;
            WHILE mcurrent <> NIL DO

                (* Inner loop works through the main list.  Note that   *)
                (* the 'absent' flag is cleared when we find a match.   *)

                match := CompareStr (mcurrent^.shortname, scurrent^.shortname) = 0;

                IF match THEN
                    mcurrent^.absent := FALSE;
                    snext := scurrent^.next;
                    IF sprevious = NIL THEN
                        sub := snext;
                    ELSE
                        sprevious^.next := snext;
                    END (*IF*);
                    DiscardStructureData (scurrent^.structure);
                    DISPOSE (scurrent);
                    scurrent := snext;
                    mcurrent := NIL;   (* force inner loop termination *)
                ELSE
                    mcurrent := mcurrent^.next;
                END (*IF*);

            END (*WHILE*);

            IF NOT match THEN
                sprevious := scurrent;  scurrent := scurrent^.next;
            END (*IF*);

        END (*WHILE*);

    END DeleteKnown;

(************************************************************************)

PROCEDURE AppendToList (M: Mailbox;  q: ItemPtr): CARDINAL;

    (* Removes from M^.itemlist any items that still have their absent  *)
    (* flag set.  Then adds the items in list q to the mailbox, and     *)
    (* assigns UID codes to them.  M^.changed is set to TRUE unless     *)
    (* there was nothing to do.  The function result is the sequence    *)
    (* number of the item that was the last in the mailbox BEFORE the   *)
    (* list q items were added.                                         *)

    VAR prev, current, next: ItemPtr;
        lastseqno: CARDINAL;

    BEGIN
        M^.changed := FALSE;

        (* Go through M^.itemlist, counting the items and removing the  *)
        (* obsolete entries.                                            *)

        M^.count := 0;
        prev := NIL;
        current := M^.itemlist;
        WHILE current <> NIL DO
            next := current^.next;
            IF current^.absent THEN
                IF prev = NIL THEN
                    M^.itemlist := next;
                ELSE
                    prev^.next := next;
                END (*IF*);
                M^.changed := TRUE;
                DiscardStructureData (current^.structure);
                DISPOSE (current);
            ELSE
                INC (M^.count);
                prev := current;
            END (*IF*);
            current := next;
        END (*WHILE*);

        (* Now prev^ is the last item on the list. *)

        M^.changed := M^.changed OR (q <> NIL);
        IF prev = NIL THEN
            M^.count := 0;
            M^.itemlist := q;
            lastseqno := 0;
        ELSE
            lastseqno := prev^.seqno;
            prev^.next := q;
            IF (M^.firstunseen = 0) AND (q <> NIL) THEN
                M^.firstunseen := lastseqno + 1;
            END (*IF*);
        END (*IF*);
        RETURN lastseqno;
    END AppendToList;

(************************************************************************)

PROCEDURE DiscardItemList (VAR (*INOUT*) head: ListOfItems);

    (* Discards the list.  We assume that we already have exclusive     *)
    (* access to this mailbox.                                          *)

    VAR next: ListOfItems;

    BEGIN
        WHILE head <> NIL DO
            next := head^.next;
            DiscardStructureData (head^.structure);
            DISPOSE (head);
            head := next;
        END (*WHILE*);
    END DiscardItemList;

(************************************************************************)

PROCEDURE UpdateItemList (M: Mailbox;  q: ItemPtr);

    (* Updates M by removing from M^.itemlist any files that aren't in  *)
    (* the q list, and then adding any files in the q list that were    *)
    (* not already in M^.itemlist.  The reason why this is not the same *)
    (* as simply replacing M^.itemlist by q is that in the process we   *)
    (* recompute flags and sequence numbers.                            *)

    VAR seqnum: CARDINAL;

    BEGIN
        DeleteKnown (M^.itemlist, q);
        M^.recent := 0;
        seqnum := AppendToList (M, q);
        IF q <> NIL THEN
            Obtain (M^.NextUIDLock);
            WHILE q <> NIL DO
                INC (M^.count);
                INC (seqnum);
                q^.seqno := seqnum;
                q^.UID := M^.NextUID;
                INC (M^.NextUID);
                EXCL (q^.flags, deleted);
                INCL (q^.flags, recent);
                INC (M^.recent);
                INC (M^.unseen);
                q := q^.next;
            END (*WHILE*);
            Release (M^.NextUIDLock);
        END (*IF*);
        M^.count := seqnum;
    END UpdateItemList;

(************************************************************************)

PROCEDURE CreateItemList (M: Mailbox);

    (* Brings our information about this list up to date.  We assume    *)
    (* that we already have exclusive access to this mailbox.           *)

    VAR q: ItemPtr;
        dir: FilenameString;

    BEGIN
        (* The INBOX directory is a special case, because for it we     *)
        (* keep the itemlist in the INBOX directory, but the mail items *)
        (* are in the user's root directory.                            *)

        dir := M^.fdir;
        IF M^.IsInbox THEN
            Strings.Append ("\INBOX", dir);
        END (*IF*);

        (* Merge the current set of items with      *)
        (* the information in the 'itemlist' file.  *)

        Obtain (M^.NextUIDLock);
        M^.itemlist := ReadItemListFile (dir, M^.NextUID,
                                             M^.unseen, M^.firstunseen);
        Release (M^.NextUIDLock);
        IF M^.IsInbox THEN
            dir[LENGTH(dir)-6] := Nul;
        END (*IF*);
        q := RescanDirectory (dir);
        UpdateItemList (M, q);
        IF M^.changed THEN
            WriteItemListFile (M);
        END (*IF*);

    END CreateItemList;

(************************************************************************)
(*                   OPENING AND CLOSING A MAILBOX                      *)
(************************************************************************)

PROCEDURE OpenMailbox (RC: ReplyCxt;  VAR (*IN*) dir: FilenameString;
                                 IsInbox, ReadOnly: BOOLEAN): MailboxUser;

    (* Loads our internal record of the mail items in this directory,   *)
    (* or adds a new user if the data were already loaded.              *)

    VAR previous: Mailbox;  M: Mailbox;  MBU: MailboxUser;
        fdir: FilenameString;

    BEGIN
        Obtain (MasterBoxList.access);
        previous := NIL;  M := MasterBoxList.head;
        WHILE (M <> NIL) AND NOT Strings.Equal(M^.fdir, dir) DO
            previous := M;
            M := M^.next;
        END (*WHILE*);

        IF M = NIL THEN

            (* There are no existing instances of this box open. *)

            NEW (M);
            M^.changed := FALSE;
            M^.IsInbox := IsInbox;
            M^.next := NIL;
            M^.voyeurs := NIL;
            fdir := dir;

            (* The Inbox is a special case.  The ItemList file goes in  *)
            (* the directory whose name is INBOX, but the messages      *)
            (* files go in the parent of that directory.                *)

            IF IsInbox THEN
                fdir[LENGTH(fdir)-6] := Nul;
            END (*IF*);
            M^.fdir := fdir;
            M^.RWusers := 0;
            CreateLock (M^.access);
            CreateLock (M^.NextUIDLock);
            CreateItemList (M);
            IF previous = NIL THEN
                MasterBoxList.head := M;
            ELSE
                previous^.next := M;
            END (*IF*);
        END (*IF*);

        (* Mailbox found (or created).  Create a new MailboxUser record *)
        (* and link it to the mailbox.                                  *)

        NEW (MBU);
        Obtain (M^.access);
        MBU^.next := M^.voyeurs;
        M^.voyeurs := MBU;
        MBU^.box := M;
        MBU^.RC := RC;
        MBU^.readonly := ReadOnly;
        MBU^.count := 0;
        IF NOT ReadOnly THEN
            INC (M^.RWusers);
        END (*IF*);
        Release (M^.access);

        Release (MasterBoxList.access);

        RETURN MBU;

    END OpenMailbox;

(************************************************************************)

PROCEDURE RefreshItemList (MBU: MailboxUser);

    (* Resynchronizes the in-memory and on-disk information about this  *)
    (* mailbox.                                                         *)

    VAR q: ItemPtr;  M: Mailbox;  HadChanged: BOOLEAN;

    BEGIN
        IF MBU <> NIL THEN
            M := MBU^.box;
            IF M <> NIL THEN
                Obtain (M^.access);
                HadChanged := M^.changed;

                (* Recheck the directory for any changes. *)

                q := RescanDirectory (M^.fdir);
                UpdateItemList (M, q);

                (* Write the itemlist back to disk if it has changed. *)

                IF HadChanged OR M^.changed THEN
                    WriteItemListFile (M);
                END (*IF*);

                Release (M^.access);
            END (*IF*);
        END (*IF*);

    END RefreshItemList;

(************************************************************************)

PROCEDURE RemoveDeleted (RC: ReplyCxt;  M: Mailbox;  ReportBack: BOOLEAN);

    (* Deletes all messages with the \Deleted flag set, sending back an *)
    (* untagged EXPUNGE response iff ReportBack is TRUE.                *)
    (* Assumption: we already have exclusive access to the mailbox.     *)

    VAR previous, p, q: ItemPtr;  dummy: BOOLEAN;
        buffer: ARRAY [0..1023] OF CHAR;
        fullname: FilenameString;

    BEGIN
        previous := NIL;  p := M^.itemlist;
        WHILE p <> NIL DO
            IF deleted IN p^.flags THEN
                MakeFullName (M^.fdir, p^.shortname, fullname);
                FileSys.Remove (fullname, dummy);
                IF ReportBack THEN
                     CardinalToStringLJ (p^.seqno, buffer);
                     Strings.Append (" EXPUNGE", buffer);
                     ReplyUntagged (RC, buffer);
                END (*IF*);
                q := p^.next;
                IF previous = NIL THEN
                    M^.itemlist := q;
                ELSE
                    previous^.next := q;
                END (*IF*);
                DiscardStructureData (p^.structure);
                DISPOSE (p);
                p := q;
                WHILE q <> NIL DO
                    DEC (q^.seqno);
                    q := q^.next;
                END (*WHILE*);
            ELSE
                previous := p;
                p := p^.next;
            END (*IF*);
        END (*WHILE*);
    END RemoveDeleted;

(************************************************************************)

PROCEDURE CloseMailbox (VAR (*INOUT*) M: Mailbox);

    (* Disposes of the mailbox record, and does whatever tidying up     *)
    (* that is needed.  We assume that any MailboxUser records for this *)
    (* box have already been detached from it.                          *)

    BEGIN
        IF M <> NIL THEN
            IF M^.changed THEN
                WriteItemListFile (M);
            END (*IF*);
            DiscardItemList (M^.itemlist);
            DestroyLock (M^.NextUIDLock);
            DestroyLock (M^.access);
            DISPOSE (M);
        END (*IF*);
    END CloseMailbox;

(************************************************************************)

PROCEDURE DetachBox (M: Mailbox);

    (* Removes a mailbox from the master list of boxes.  *)

    VAR previous, current: Mailbox;

    BEGIN
        previous := NIL;
        Obtain (MasterBoxList.access);
        current := MasterBoxList.head;
        WHILE (current <> NIL) AND (current <> M) DO
            previous := current;  current := current^.next;
        END (*WHILE*);

        (* Detach this box record. *)

        IF current <> NIL THEN
            IF previous = NIL THEN
                MasterBoxList.head := current^.next;
            ELSE
                previous^.next := current^.next;
            END (*IF*);
        END (*IF*);

        Release (MasterBoxList.access);

    END DetachBox;

(************************************************************************)

PROCEDURE CloseMailboxUser (VAR (*INOUT*) MBU: MailboxUser);

    (* Deregisters this user as a user of the mailbox, and closes the   *)
    (* mailbox if this was the last user of that mailbox.               *)

    VAR previous, current: MailboxUser;
        M: Mailbox;

    BEGIN
        IF MBU <> NIL THEN
            M := MBU^.box;
            previous := NIL;
            IF M <> NIL THEN
                Obtain (M^.access);
                current := M^.voyeurs;
                WHILE (current <> NIL) AND (current <> MBU) DO
                    previous := current;  current := current^.next;
                END (*WHILE*);

                (* Detach this user record.  We will dispose of it *)
                (* before leaving this procedure.                  *)

                IF current <> NIL THEN
                    IF NOT MBU^.readonly THEN
                        IF M^.RWusers = 1 THEN
                            RemoveDeleted (NIL, M, FALSE);
                        END (*IF*);
                        DEC (M^.RWusers);
                    END (*IF*);
                    IF previous = NIL THEN
                        M^.voyeurs := current^.next;
                    ELSE
                        previous^.next := current^.next;
                    END (*IF*);
                END (*IF*);

                (* If there are no users left, close the mailbox. *)

                IF M^.voyeurs = NIL THEN
                    DetachBox (M);
                    Release (M^.access);
                    CloseMailbox (M);
                END (*IF*);
            END (*IF*);

            IF M <> NIL THEN
                Release (M^.access);
            END (*IF*);

            DISPOSE (MBU);

        END (*IF*);

    END CloseMailboxUser;

(************************************************************************)
(*                OBTAINING EXCLUSIVE ACCESS TO A MAILBOX               *)
(************************************************************************)

PROCEDURE LockMailbox (MBU: MailboxUser);

    (* Obtains exclusive access to the mailbox. *)

    BEGIN
        IF (MBU <> NIL) AND (MBU^.box <> NIL) THEN
            Obtain (MBU^.box^.access);
        END (*IF*);
    END LockMailbox;

(************************************************************************)

PROCEDURE UnlockMailbox (MBU: MailboxUser);

    (* Releases exclusive access to the mailbox. *)

    BEGIN
        IF (MBU <> NIL) AND (MBU^.box <> NIL) THEN
            Release (MBU^.box^.access);
        END (*IF*);
    END UnlockMailbox;

(************************************************************************)
(*                          STATUS REPLIES                              *)
(************************************************************************)

PROCEDURE StatusResponse (MBU: MailboxUser);

    (* Replies with the current status of the mailbox.  It looks as if  *)
    (* this procedure is invoked only by NOOP, but I have not checked   *)
    (* this.                                                            *)
    (* Assumption: we already have exclusive access to the mailbox.     *)

    VAR M: Mailbox;  buffer: ARRAY [0..511] OF CHAR;

    BEGIN
        IF MBU = NIL THEN
            M := NIL;
        ELSE
            M := MBU^.box;
        END (*IF*);
        IF M <> NIL THEN

            CardinalToStringLJ (M^.count, buffer);
            Strings.Append (' EXISTS', buffer);
            ReplyUntagged (MBU^.RC, buffer);
            MBU^.count := M^.count;

            CardinalToStringLJ (M^.recent, buffer);
            Strings.Append (' RECENT', buffer);
            ReplyUntagged (MBU^.RC, buffer);
            IF M^.recent <> 0 THEN
                M^.changed := TRUE;
                M^.recent := 0;
            END (*IF*);

        END (*IF*);
    END StatusResponse;

(************************************************************************)

PROCEDURE StatusUpdates (M: Mailbox);

    (* Sends untagged replies to all clients who have this mailbox      *)
    (* open, to say what has changed since this client last received    *)
    (* a status update.                                                 *)
    (* Assumption: we already have exclusive access to M.               *)

    VAR MBU: MailboxUser;  value: CARDINAL;
        buffer: ARRAY [0..511] OF CHAR;

    BEGIN
        MBU := M^.voyeurs;
        WHILE MBU <> NIL DO

            value := M^.count;
            IF MBU^.count <> value THEN
                MBU^.count := value;
                CardinalToStringLJ (value, buffer);
                Strings.Append (' EXISTS', buffer);
                ReplyUntagged (MBU^.RC, buffer);
            END (*IF*);

            value := M^.recent;
            IF value <> 0 THEN
                CardinalToStringLJ (value, buffer);
                Strings.Append (' RECENT', buffer);
                ReplyUntagged (MBU^.RC, buffer);
                M^.recent := 0;
                M^.changed := TRUE;
            END (*IF*);

            MBU := MBU^.next;

        END (*WHILE*);

    END StatusUpdates;

(************************************************************************)

PROCEDURE ReportFlags (M: Mailbox;  p: ItemPtr;  skip: MailboxUser);

    (* Sends an untagged FETCH response to all clients EXCEPT skip who  *)
    (* have this mailbox open, to report the flags of item p^.          *)
    (* Assumption: we already have exclusive access to M.               *)

    VAR MBU: MailboxUser;
        buffer, tempbuf: ARRAY [0..511] OF CHAR;

    BEGIN
        MBU := M^.voyeurs;
        WHILE MBU <> NIL DO

            IF MBU <> skip THEN
                CardinalToStringLJ (p^.seqno, buffer);
                Strings.Append (" FETCH (FLAGS (", buffer);
                EncodeFlags (p^.flags, tempbuf, FALSE);
                Strings.Append (tempbuf, buffer);
                Strings.Append ("))", buffer);
                ReplyUntagged (MBU^.RC, buffer);
                EXCL (p^.flags, recent);
            END (*IF*);

            MBU := MBU^.next;

        END (*WHILE*);

    END ReportFlags;

(************************************************************************)

PROCEDURE SelectBoxReplies (MBU: MailboxUser);

    (* Responses to the SELECT command.    *)

    VAR M: Mailbox;  buffer: ARRAY [0..511] OF CHAR;

    BEGIN
        IF MBU = NIL THEN
            M := NIL;
        ELSE
            M := MBU^.box;
        END (*IF*);

        IF M <> NIL THEN

            ReplyUntagged (MBU^.RC, "FLAGS (\Seen \Answered \Flagged \Deleted \Draft)");
            Obtain (M^.access);
            StatusResponse (MBU);
            IF M^.firstunseen <> 0 THEN
                CardinalToStringLJ (M^.firstunseen, buffer);
                Reply3Untagged (MBU^.RC, "OK [UNSEEN ", buffer, "] first unseen");
            END (*IF*);
            Release (M^.access);

            (* There is also an OPTIONAL OK untagged response: *)
            (*         PERMANENTFLAGS                          *)
            (* but for now I'm not implementing that.          *)

        END (*IF*);
    END SelectBoxReplies;

(************************************************************************)

PROCEDURE AppendCard (N: CARDINAL;  VAR (*INOUT*) str: ARRAY OF CHAR);

    (* Converts N to decimal and appends it to str. *)

    VAR pos: CARDINAL;

    BEGIN
        pos := Strings.Length (str);
        ConvertCard (N, str, pos);
        str[pos] := Nul;
    END AppendCard;

(************************************************************************)

PROCEDURE DoStatusReply (MBU: MailboxUser;  UIDV: CARDINAL;
                        VAR (*IN*) boxname, args: ARRAY OF CHAR): BOOLEAN;

    (* Responds to the STATUS command, where args is the parenthesised  *)
    (* list that was supplied with that command.   UIDV is the UID      *)
    (* validity code for this mailbox; the caller has already looked it *)
    (* up in case we need it.                                           *)

    VAR M: Mailbox;  success: BOOLEAN;
        buffer: ARRAY [0..2047] OF CHAR;

    BEGIN
        IF MBU = NIL THEN
            M := NIL;
        ELSE
            M := MBU^.box;
        END (*IF*);

        success := M <> NIL;
        IF success THEN
            Obtain (M^.access);
            IF Match (args, '(') THEN
                Strings.Assign ('STATUS ', buffer);
                Strings.Append (boxname, buffer);
                Strings.Append (' (', buffer);
                WHILE success DO
                    IF Match (args, "MESSAGES") THEN
                        Strings.Append ("MESSAGES ", buffer);
                        AppendCard (M^.count, buffer);
                        MBU^.count := M^.count;
                    ELSIF Match (args, "RECENT") THEN
                        Strings.Append ("RECENT ", buffer);
                        AppendCard (M^.recent, buffer);
                        M^.recent := 0;
                        M^.changed := TRUE;
                    ELSIF Match (args, "UIDNEXT") THEN
                        Strings.Append ("UIDNEXT ", buffer);
                        AppendCard (M^.NextUID, buffer);
                    ELSIF Match (args, "UIDVALIDITY") THEN
                        Strings.Append ("UIDVALIDITY ", buffer);
                        AppendCard (UIDV, buffer);
                    ELSIF Match (args, "UNSEEN") THEN
                        Strings.Append ("UNSEEN ", buffer);
                        AppendCard (M^.unseen, buffer);
                    ELSE
                        success := FALSE;
                    END (*IF*);
                    Strings.Append (" ", buffer);
                    StripLeadingSpaces (args);
                END (*WHILE*);
                success := Match (args, ')');
                IF success THEN
                    buffer[Strings.Length(buffer)-2] := ')';
                    buffer[Strings.Length(buffer)-1] := Nul;
                    ReplyUntagged (MBU^.RC, buffer);
                END (*IF*);
            END (*IF*);
            Release (M^.access);
        END (*IF*);
        RETURN success;
    END DoStatusReply;

(************************************************************************)
(*                        SELECTING FROM A SET                          *)
(************************************************************************)

PROCEDURE NextInSet (VAR (*INOUT*) p: ItemPtr;
                     VAR (*INOUT*) set: MessageSet;  UseUID: BOOLEAN);

    (* Steps p forward to the next mailbox item that is in the set, and *)
    (* removes that member from the set.  Returns NIL if no next item.  *)

    VAR messageno: CARDINAL;

    BEGIN
        IF p = NIL THEN
            messageno := 0;
        ELSIF UseUID THEN
            messageno := NextMemberFrom (set, p^.UID);
        ELSE
            messageno := NextMemberFrom (set, p^.seqno);
        END (*IF*);
        IF messageno = 0 THEN
            p := NIL;
        END (*IF*);
        IF UseUID THEN
            WHILE (p <> NIL) AND (p^.UID < messageno) DO
                p := p^.next;
            END (*WHILE*);
        ELSE
            WHILE (p <> NIL) AND (p^.seqno < messageno) DO
                p := p^.next;
            END (*WHILE*);
        END (*IF*);
    END NextInSet;

(************************************************************************)
(*                              FETCHING                                *)
(************************************************************************)

TYPE
    (* A FetchAttr structure records what has been requested in a FETCH *)
    (* instruction.  Notice that all of these fields, with the          *)
    (* exception of BodySection, are Boolean flags rather than values.  *)
    (* The BodySection part contains more complicated detail such as    *)
    (* whether we are fetching parts of headers, multipart components,  *)
    (* etc.  That detail is put together by the Messages module, not    *)
    (* the current module, but of course we have the responsibility of  *)
    (* discarding it once it is no longer needed.                       *)

    FetchAttr = RECORD
                    (* Requested components. *)

                    BODY: BOOLEAN;
                    BodySection: BodyInfo;
                    BODYSTRUCTURE: BOOLEAN;
                    ENVELOPE: BOOLEAN;
                    FLAGS: BOOLEAN;
                    INTERNALDATE: BOOLEAN;
                    RFC822_SIZE: BOOLEAN;
                    UID: BOOLEAN;

                    (* Modifiers. *)

                    SuppressSeen: BOOLEAN;

                END (*RECORD*);

(************************************************************************)

PROCEDURE ClearAttr (VAR (*OUT*) Attr: FetchAttr);

    (* Clears all fetch attributes. *)

    BEGIN
        WITH Attr DO
            ENVELOPE := FALSE;
            FLAGS := FALSE;
            INTERNALDATE := FALSE;
            RFC822_SIZE := FALSE;
            BODY := FALSE;
            BODYSTRUCTURE := FALSE;
            UID := FALSE;
            SuppressSeen := FALSE;
            InitBodyInfo (BodySection);
        END (*WITH*);
    END ClearAttr;

(************************************************************************)

PROCEDURE DiscardFetchAttrs (VAR (*INOUT*) Attr: FetchAttr);

    (* Disposes of the dynamic part of Attr. *)

    BEGIN
        DiscardBodyInfo (Attr.BodySection);
    END DiscardFetchAttrs;

(************************************************************************)

PROCEDURE ParseFetchAttrs (params: ARRAY OF CHAR;
                       LogID: TransactionLogID;  VAR (*OUT*) Attr: FetchAttr);

    (* Parses the arguments of a FETCH instruction - there are many     *)
    (* possibilities - with the results returned in Attr.               *)

    (********************************************************************)

    PROCEDURE AddNextAttr;

        (* Picks up the next item from params. *)

        BEGIN
            IF Match (params, "ENVELOPE") THEN
                Attr.ENVELOPE := TRUE;
            ELSIF Match (params, "FLAGS") THEN
                Attr.FLAGS := TRUE;
            ELSIF Match (params, "INTERNALDATE") THEN
                Attr.INTERNALDATE := TRUE;
            ELSIF Match (params, "RFC822.SIZE") THEN
                Attr.RFC822_SIZE := TRUE;
            ELSIF Match (params, "UID") THEN
                Attr.UID := TRUE;
            ELSIF Match (params, "BODY.PEEK") THEN
                Attr.SuppressSeen := TRUE;
                IF Match (params, "[") THEN
                    Attr.BODY := TRUE;
                    ParseFetchSection (params, Attr.BodySection);
                ELSE
                    Attr.BODYSTRUCTURE := TRUE;
                END (*IF*);
            ELSIF Match (params, "BODYSTRUCTURE") THEN
                Attr.BODYSTRUCTURE := TRUE;
            ELSIF Match (params, "BODY") THEN
                IF Match (params, "[") THEN
                    Attr.BODY := TRUE;
                    ParseFetchSection (params, Attr.BodySection);
                ELSE
                    Attr.BODYSTRUCTURE := TRUE;
                END (*IF*);
            ELSE
                LogTransactionL (LogID, "Unknown FETCH option");
                params[0] := Nul;
            END (*IF*);

        END AddNextAttr;

    (********************************************************************)

    BEGIN
        IF Match (params, "ALL") THEN
            Strings.Assign ("(FLAGS INTERNALDATE RFC822.SIZE ENVELOPE)", params);
        ELSIF Match (params, "FULL") THEN
            Strings.Assign ("(FLAGS INTERNALDATE RFC822.SIZE ENVELOPE BODY)", params);
        ELSIF Match (params, "FAST") THEN
            Strings.Assign ("(FLAGS INTERNALDATE RFC822.SIZE)", params);
        END (*IF*);

        IF Match (params, "(") THEN
            WHILE (params[0] <> Nul) AND NOT Match(params, ")") DO
                WHILE SPACE(params) DO
                    (* Consume the space. *)
                END (*WHILE*);
                AddNextAttr;
            END (*WHILE*);
        ELSE
            AddNextAttr;
        END (*IF*);

    END ParseFetchAttrs;

(************************************************************************)
(*                                                                      *)
(*  Here are the FETCH options we have not yet implemented.             *)
(*  (Actually, most or perhaps all of them are now implemented, but     *)
(*  my documentation on this is not up to date.)                        *)
(*                                                                      *)
(*  Requests:                                                           *)
(*                                                                      *)
(*
      BODY           Non-extensible form of BODYSTRUCTURE.
      BODY[<section>]<<partial>>
            (Done except for the <partial> option.)
            [But has to be completely re-done because of a design change.]

      BODYSTRUCTURE  The [MIME-IMB] body structure of the message.  This
                     is computed by the server by parsing the [MIME-IMB]
                     header fields in the [RFC-822] header and
                     [MIME-IMB] headers.
            [NOW OK, I BELIEVE]

      ENVELOPE       The envelope structure of the message.  This is
                     computed by the server by parsing the [RFC-822]
                     header into the component parts, defaulting various
                     fields as necessary.
            [NOW OK, I BELIEVE.]

      RFC822         Functionally equivalent to BODY[], differing in the
                     syntax of the resulting untagged FETCH data (RFC822
                     is returned).
      RFC822.HEADER  Functionally equivalent to BODY.PEEK[HEADER],
                     differing in the syntax of the resulting untagged
                     FETCH data (RFC822.HEADER is returned).
      RFC822.TEXT    Functionally equivalent to BODY[TEXT], differing in
                     the syntax of the resulting untagged FETCH data
                     (RFC822.TEXT is returned).
*)
(*  Responses:                                                          *)
(*                                                                      *)
(*
      BODY           A form of BODYSTRUCTURE without extension data.
      BODY[<section>]<<origin_octet>>
              (Done, but origin not implemented.)
      BODYSTRUCTURE  A parenthesized list that describes the [MIME-IMB]
                     body structure of a message.  This is computed by
                     the server by parsing the [MIME-IMB] header fields,
                     defaulting various fields as necessary.
                     For example, a simple text message of 48 lines and
                     2279 octets can have a body structure of: ("TEXT"
                     "PLAIN" ("CHARSET" "US-ASCII") NIL NIL "7BIT" 2279
                     48)
                     Multiple parts are indicated by parenthesis
                     nesting.  Instead of a body type as the first
                     element of the parenthesized list there is a nested
                     body.  The second element of the parenthesized list
                     is the multipart subtype (mixed, digest, parallel,
                     alternative, etc.).
                     For example, a two part message consisting of a
                     text and a BASE645-encoded text attachment can have
                     a body structure of: (("TEXT" "PLAIN" ("CHARSET"
                     "US-ASCII") NIL NIL "7BIT" 1152 23)("TEXT" "PLAIN"
                     ("CHARSET" "US-ASCII" "NAME" "cc.diff")
                     "<960723163407.20117h@cac.washington.edu>"
                     "Compiler diff" "BASE64" 4554 73) "MIXED"))
                     Extension data follows the multipart subtype.
                     Extension data is never returned with the BODY
                     fetch, but can be returned with a BODYSTRUCTURE
                     fetch.  Extension data, if present, MUST be in the
                     defined order.
                     The extension data of a multipart body part are in
                     the following order:
                     body parameter parenthesized list
                        A parenthesized list of attribute/value pairs
                        [e.g. ("foo" "bar" "baz" "rag") where "bar" is
                        the value of "foo" and "rag" is the value of
                        "baz"] as defined in [MIME-IMB].
                     body disposition
                        A parenthesized list, consisting of a
                        disposition type string followed by a
                        parenthesized list of disposition
                        attribute/value pairs.  The disposition type and
                        attribute names will be defined in a future
                        standards-track revision to [DISPOSITION].
                     body language
                        A string or parenthesized list giving the body
                        language value as defined in [LANGUAGE-TAGS].
                     Any following extension data are not yet defined in
                     this version of the protocol.  Such extension data
                     can consist of zero or more NILs, strings, numbers,
                     or potentially nested parenthesized lists of such
                     data.  Client implementations that do a
                     BODYSTRUCTURE fetch MUST be prepared to accept such
                     extension data.  Server implementations MUST NOT
                     send such extension data until it has been defined
                     by a revision of this protocol.
                     The basic fields of a non-multipart body part are
                     in the following order:
                     body type
                        A string giving the content media type name as
                        defined in [MIME-IMB].
                     body subtype
                        A string giving the content subtype name as
                        defined in [MIME-IMB].
                     body parameter parenthesized list
                        A parenthesized list of attribute/value pairs
                        [e.g. ("foo" "bar" "baz" "rag") where "bar" is
                        the value of "foo" and "rag" is the value of
                        "baz"] as defined in [MIME-IMB].
                     body id
                        A string giving the content id as defined in
                        [MIME-IMB].
                     body description
                        A string giving the content description as
                        defined in [MIME-IMB].
                     body encoding
                        A string giving the content transfer encoding as
                        defined in [MIME-IMB].
                     body size
                        A number giving the size of the body in octets.
                        Note that this size is the size in its transfer
                        encoding and not the resulting size after any
                        decoding.
                     A body type of type MESSAGE and subtype RFC822
                     contains, immediately after the basic fields, the
                     envelope structure, body structure, and size in
                     text lines of the encapsulated message.
                     A body type of type TEXT contains, immediately
                     after the basic fields, the size of the body in
                     text lines.  Note that this size is the size in its
                     content transfer encoding and not the resulting
                     size after any decoding.
                     Extension data follows the basic fields and the
                     type-specific fields listed above.  Extension data
                     is never returned with the BODY fetch, but can be
                     returned with a BODYSTRUCTURE fetch.  Extension
                     data, if present, MUST be in the defined order.
                     The extension data of a non-multipart body part are
                     in the following order:
                     body MD5
                        A string giving the body MD5 value as defined in
                        [MD5].
                     body disposition
                        A parenthesized list with the same content and
                        function as the body disposition for a multipart
                        body part.
                     body language
                        A string or parenthesized list giving the body
                        language value as defined in [LANGUAGE-TAGS].
                     Any following extension data are not yet defined in
                     this version of the protocol, and would be as
                     described above under multipart extension data.
      ENVELOPE       A parenthesized list that describes the envelope
                     structure of a message.  This is computed by the
                     server by parsing the [RFC-822] header into the
                     component parts, defaulting various fields as
                     necessary.
                     The fields of the envelope structure are in the
                     following order: date, subject, from, sender,
                     reply-to, to, cc, bcc, in-reply-to, and message-id.
                     The date, subject, in-reply-to, and message-id
                     fields are strings.  The from, sender, reply-to,
                     to, cc, and bcc fields are parenthesized lists of
                     address structures.
                     An address structure is a parenthesized list that
                     describes an electronic mail address.  The fields
                     of an address structure are in the following order:
                     personal name, [SMTP] at-domain-list (source
                     route), mailbox name, and host name.
                     [RFC-822] group syntax is indicated by a special
                     form of address structure in which the host name
                     field is NIL.  If the mailbox name field is also
                     NIL, this is an end of group marker (semi-colon in
                     RFC 822 syntax).  If the mailbox name field is
                     non-NIL, this is a start of group marker, and the
                     mailbox name field holds the group name phrase.
                     Any field of an envelope or address structure that
                     is not applicable is presented as NIL.  Note that
                     the server MUST default the reply-to and sender
                     fields from the from field; a client is not
                     expected to know to do this.
      RFC822         Equivalent to BODY[].
      RFC822.HEADER  Equivalent to BODY.PEEK[HEADER].
      RFC822.TEXT    Equivalent to BODY[TEXT].
*)

(************************************************************************)

PROCEDURE DoFetch (RC: ReplyCxt;  VAR (*IN*) boxdir: FilenameString;
                                  p: ItemPtr;  Attr: FetchAttr;
                                  LogID: TransactionLogID): BOOLEAN;

    (* Responds to the FETCH instruction for one item, with Attr  *)
    (* specifying which parts we have to fetch.                   *)

    VAR success, ReplyStarted, flagupdate: BOOLEAN;
        date, time: CARDINAL;
        NewAttr: FetchAttr;
        bufptr: POINTER TO ARRAY [0..4095] OF CHAR;
        tempbuf: ARRAY [0..79] OF CHAR;
        filename: FilenameString;

    BEGIN
        success := TRUE;  ReplyStarted := FALSE;  flagupdate := FALSE;
        MakeFullName (boxdir, p^.shortname, filename);
        NEW (bufptr);
        CardinalToStringLJ (p^.seqno, bufptr^);
        Strings.Append (" FETCH (", bufptr^);

        IF Attr.UID THEN
            Strings.Append ("UID ", bufptr^);
            CardinalToStringLJ (p^.UID, tempbuf);
            Strings.Append (tempbuf, bufptr^);
            Strings.Append (" ", bufptr^);
        END (*IF*);

        IF Attr.RFC822_SIZE THEN
            Strings.Append ("RFC822.SIZE ", bufptr^);
            Card64ToStringLJ (SizeOfMessage (filename), tempbuf);
            Strings.Append (tempbuf, bufptr^);
            Strings.Append (" ", bufptr^);
            success := TRUE;
        END (*IF*);

        IF Attr.BODYSTRUCTURE THEN
            Strings.Append ("BODYSTRUCTURE ", bufptr^);
            PartialReplyUntagged (RC, bufptr^);
            bufptr^[0] := Nul;
            ReportBodyStructure (filename, RC, bufptr^, p^.structure);
            Strings.Append (" ", bufptr^);
            ReplyStarted := TRUE;
        END (*IF*);

        IF Attr.ENVELOPE THEN
            Strings.Append ("ENVELOPE ", bufptr^);
            ReturnEnvelope (filename, bufptr^);
            Strings.Append (" ", bufptr^);
        END (*IF*);

        IF Attr.INTERNALDATE THEN
            Strings.Append ('INTERNALDATE "', bufptr^);
            PackedDateTime (filename, date, time);
            PackedDateTimeToString (date, time, tempbuf);
            Strings.Append (tempbuf, bufptr^);
            Strings.Append ('" ', bufptr^);
        END (*IF*);

        IF Attr.FLAGS THEN
            Strings.Append ("FLAGS (", bufptr^);
            EncodeFlags (p^.flags, tempbuf, FALSE);
            Strings.Append (tempbuf, bufptr^);
            Strings.Append (") ", bufptr^);
            EXCL (p^.flags, recent);
        END (*IF*);

        IF Attr.BODY THEN

            (* Send off the partial response so far, so that we *)
            (* can start afresh with an empty reply buffer.     *)

            IF bufptr^[0] <> Nul THEN
                PartialReplyUntagged (RC, bufptr^);
                bufptr^[0] := Nul;
            END (*IF*);

            SendSection (RC, filename, Attr.BodySection, p^.structure);
            flagupdate := NOT (Attr.SuppressSeen OR (seen IN p^.flags));
            IF flagupdate THEN
                INCL (p^.flags, seen);
            END (*IF*);
            ReplyStarted := TRUE;
            success := TRUE;

        END (*IF*);

        DeleteLastSpace (bufptr^);
        Strings.Append (")", bufptr^);
        IF ReplyStarted THEN
            PartialReply (RC, bufptr^);
            CommitReply (RC);
        ELSE
            ReplyUntagged (RC, bufptr^);
        END (*IF*);
        RestoreLogging (RC);

        (* The corresponding "SuppressLogging" was done in the  *)
        (* SendSection operation above.                         *)

        (* A BODY response above might have altered the \Seen attribute *)
        (* meaning that we have to give a new FLAGS response.           *)

        IF flagupdate THEN
            ClearAttr (NewAttr);
            NewAttr.FLAGS := TRUE;
            success := DoFetch (RC, boxdir, p, NewAttr, LogID);
            DiscardFetchAttrs (NewAttr);
        END (*IF*);

        DISPOSE (bufptr);

        (* The MIME structure details in p^.structure might or might    *)
        (* not have been constructed in the above, but to reduce the    *)
        (* risk of memory leaks I'll dispose of them here.              *)

        DiscardStructureData (p^.structure);

        RETURN success;

    END DoFetch;

(************************************************************************)

PROCEDURE FetchItems (RC: ReplyCxt;  MBU: MailboxUser;
                       VAR (*INOUT*) set: MessageSet;
                       VAR (*INOUT*) params: ARRAY OF CHAR;
                       UseUID: BOOLEAN;  LogID: TransactionLogID): BOOLEAN;

    (* Responds to the FETCH instruction, or the UID FETCH instruction  *)
    (* if UseUID is TRUE.  Returns FALSE if we can't fetch it or if     *)
    (* this is something we haven't yet implemented.                    *)
    (* On exit, the set of message numbers has been discarded.          *)

    VAR box: Mailbox;  success: BOOLEAN;
        Attr: FetchAttr;
        p: ItemPtr;

    BEGIN
        success := TRUE;
        ClearAttr (Attr);
        ParseFetchAttrs (params, LogID, Attr);
        IF UseUID THEN
            Attr.UID := TRUE;
        END (*IF*);

        IF MBU = NIL THEN
            box := NIL;
        ELSE
            box := MBU^.box;
        END (*IF*);
        IF box = NIL THEN
            p := NIL;
        ELSE
            Obtain (box^.access);
            p := box^.itemlist;
        END (*IF*);

        WHILE (p <> NIL) AND success DO
            NextInSet (p, set, UseUID);
            IF p <> NIL THEN
                success := DoFetch (RC, box^.fdir, p, Attr, LogID);
                p := p^.next;
            END (*IF*);
        END (*WHILE*);
        DiscardFetchAttrs (Attr);
        DiscardSet (set);
        IF box <> NIL THEN
            Release (box^.access);
        END (*IF*);
        RETURN success;
    END FetchItems;

(************************************************************************)
(*                  STORING NEW DATA IN A MAILBOX                       *)
(************************************************************************)

PROCEDURE DoStore (RC: ReplyCxt;  VAR (*IN*) boxdir: FilenameString;
                       p: ItemPtr;  change: INTEGER;
                          newflags: MessageFlagSet;  silent, UseUID: BOOLEAN;
                                        LogID: TransactionLogID): BOOLEAN;

    (* If change<0, deletes newflags from the item flags.  If change>0, *)
    (* adds those flags.  If change=0, replaces the existing item flags *)
    (* with the new set.   Returns an untagged FETCH response unless    *)
    (* silent=TRUE.                                                     *)
    (* Assumption: we already have exclusive access to the list p^.     *)

    VAR Attr: FetchAttr;  success: BOOLEAN;

    BEGIN
        success := p <> NIL;
        IF success THEN
            IF change < 0 THEN
                p^.flags := p^.flags - newflags;
            ELSIF change = 0 THEN
                p^.flags := newflags;
            ELSE
                p^.flags := p^.flags + newflags;
            END (*IF*);
            IF NOT silent THEN
                ClearAttr (Attr);
                Attr.FLAGS := TRUE;
                Attr.UID := UseUID;
                success := DoFetch (RC, boxdir, p, Attr, LogID);
                DiscardFetchAttrs (Attr);
            END (*IF*);
        END (*IF*);
        RETURN success;
    END DoStore;

(************************************************************************)

PROCEDURE StoreItemData (RC: ReplyCxt;  MBU: MailboxUser;
                       VAR (*INOUT*) set: MessageSet;
                       VAR (*INOUT*) params: ARRAY OF CHAR;
                       UseUID: BOOLEAN;  LogID: TransactionLogID): BOOLEAN;

    (* Responds to the STORE instruction, or the UID STORE instruction  *)
    (* if UseUID is TRUE.  Returns FALSE if we can't do the operation   *)
    (* or if this is something we haven't yet implemented.              *)
    (* On exit, the set of message numbers has been discarded.          *)
    (* The params string is destroyed as a side-effect.                 *)

    VAR M: Mailbox;  success, silent: BOOLEAN;  change: [-1..+1];
        flags: MessageFlagSet;
        p: ItemPtr;

    BEGIN
        IF Match (params, '+') THEN
            change := +1;
        ELSIF Match (params, '-') THEN
            change := -1;
        ELSE
            change := 0;
        END (*IF*);

        (* According to the standard, the only thing that can now       *)
        (* follow is the literal FLAGS.                                 *)

        flags := MessageFlagSet{};  silent := FALSE;
        success := Match (params, "FLAGS");
        IF success THEN
            silent := Match (params, ".SILENT");
            StripLeadingSpaces (params);
            IF Match (params, '(') THEN
                flags := DecodeFlags (params);
                success := Match (params, ')');
            END (*IF*);
        END (*IF*);

        IF MBU = NIL THEN
            M := NIL;
        ELSE
            M := MBU^.box;
        END (*IF*);

        success := success AND (M <> NIL);
        IF success THEN
            Obtain (M^.access);
            p := M^.itemlist;
            WHILE (p <> NIL) AND success DO
                NextInSet (p, set, UseUID);
                IF p <> NIL THEN
                    success := DoStore (RC, M^.fdir, p, change, flags, silent, UseUID, LogID);
                    ReportFlags (M, p, MBU);
                    p := p^.next;
                END (*IF*);
            END (*WHILE*);
            M^.changed := TRUE;
            Release (M^.access);
        END (*IF*);
        DiscardSet (set);
        RETURN success;

    END StoreItemData;

(************************************************************************)
(*                    CREATING A UNIQUE FILENAME                        *)
(************************************************************************)

PROCEDURE MakeUniqueName (VAR (*OUT*) name: EightChar);

    (* Generates a unique 8-character string.  *)

    (********************************************************************)

    PROCEDURE Increment (N: CARDINAL);

        (* Increments NextName[N], with carry as appropriate. *)

        BEGIN
            IF NextName[N] = '9' THEN
                NextName[N] := 'A';
            ELSIF NextName[N] = 'Z' THEN
                NextName[N] := '0';
                IF N > 0 THEN
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

PROCEDURE MakeNewFilename (BaseName, tail: ARRAY OF CHAR;
                           VAR (*OUT*) xxx: EightChar;
                           VAR (*OUT*) NewName: ARRAY OF CHAR);

    (* Creates a file name of the form BaseNamexxxtail, where xxx is    *)
    (* chosen such that a file of that name does not already exist.     *)
    (* Note that BaseName and tail can include punctuation.             *)

    BEGIN
        REPEAT
            MakeUniqueName (xxx);
            Strings.Assign (BaseName, NewName);
            Strings.Append (xxx, NewName);
            Strings.Append (tail, NewName);
        UNTIL NOT FileSys.Exists(NewName);
    END MakeNewFilename;

(************************************************************************)

PROCEDURE OpenNewOutputFile (BaseName, tail: ARRAY OF CHAR;
                             VAR (*OUT*) xxx: EightChar;
                             VAR (*OUT*) NewName: FilenameString;
                                               Visible: BOOLEAN): ChanId;

    (* Creates a file name of the form BaseNamexxxtail, where xxx is    *)
    (* chosen such that a file of that name does not already exist, and *)
    (* opens that file.                                                 *)
    (* Note that BaseName and tail can include punctuation.             *)

    VAR cid: ChanId;  duplication: BOOLEAN;

    BEGIN
        REPEAT
            MakeNewFilename (BaseName, tail, xxx, NewName);
            IF Visible THEN
                cid := OpenNewFile1 (NewName, duplication);
            ELSE
                cid := OpenNewHiddenFile (NewName, duplication);
            END (*IF*);
        UNTIL NOT duplication;
        RETURN cid;
    END OpenNewOutputFile;

(************************************************************************)
(*                        STORING A NEW MESSAGE                         *)
(************************************************************************)

PROCEDURE SetFileDateTime (cid: ChanId;  date, time: CARDINAL);

    (* Updates the date/time of an already opened file.  We update both *)
    (* the "creation" and "last write" date/time.                       *)

    VAR InfoBuffer: OS2.FILESTATUS3;

    BEGIN
        OS2.DosQueryFileInfo (cid, OS2.FIL_STANDARD, ADR(InfoBuffer), SIZE(InfoBuffer));
        WITH InfoBuffer DO
            fdateCreation := date;
            ftimeCreation := time;
            fdateLastWrite := date;
            ftimeLastWrite := time;
        END (*WITH*);
        OS2.DosSetFileInfo (cid, OS2.FIL_STANDARD, ADR(InfoBuffer), SIZE(InfoBuffer));
    END SetFileDateTime;

(************************************************************************)

PROCEDURE DoAddMessage (MBU: MailboxUser;  flags: MessageFlagSet;
                                    date, time, count: CARDINAL): BOOLEAN;

    (* Creates a new message in mailbox M, whose content is the   *)
    (* next 'count' characters to be fetched from the client.     *)
    (* The file date/time is set iff date > 0.                    *)

    VAR M: Mailbox;  success: BOOLEAN;
        length: CARDINAL;
        cid: ChanId;
        R, tail: ItemPtr;
        PartName: EightChar;
        NewName: FilenameString;
        line: ARRAY [0..2047] OF CHAR;

    BEGIN
        IF MBU = NIL THEN
            M := NIL;
        ELSE
            M := MBU^.box;
        END (*IF*);
        success := M <> NIL;
        IF success THEN
            Obtain (M^.access);

            (* Initially make the file invisible, to avoid having it seen   *)
            (* when it is only partially complete.                          *)

            NewName := M^.fdir;
            Strings.Append ("\", NewName);
            cid := OpenNewOutputFile (NewName, ".MSG", PartName, NewName, FALSE);
            success := cid <> NoSuchChannel;

            IF success THEN
                SuppressLogging (MBU^.RC);
                WHILE count > 0 DO
                    Reply (MBU^.RC, "+");
                    IF AcceptLine (MBU^.RC, line) THEN
                        length := Strings.Length (line) + 2;
                        FWriteString (cid, line);
                        FWriteLn (cid);
                        IF count > length THEN
                            DEC (count, length);
                        ELSE
                            count := 0;
                        END (*IF*);
                    ELSE
                        count := 0;
                    END (*IF*);
                END (*WHILE*);
                RestoreLogging (MBU^.RC);

                IF date > 0 THEN
                    SetFileDateTime (cid, date, time);
                END (*IF*);

                CloseFile (cid);

                (* Adding the 'seen' flag here is, I believe, a violation   *)
                (* of the standard, but the technical violation appears to  *)
                (* be required in order to be compatible with Mozilla.  In  *)
                (* any case, the standard is so unclear that it's not       *)
                (* certain what is or is not legal.                         *)

                INCL (flags, seen);

                (* Tack this new message onto the tail of the list of       *)
                (* messages, and make the file visible.                     *)

                NEW (R);
                R^.next := NIL;
                R^.flags := flags;
                R^.shortname := PartName;

                tail := M^.itemlist;
                IF tail <> NIL THEN
                    WHILE tail^.next <> NIL DO
                        tail := tail^.next;
                    END (*WHILE*);
                END (*IF*);

                IF tail = NIL THEN
                    M^.itemlist := R;
                    R^.seqno := 1;
                ELSE
                    tail^.next := R;
                    R^.seqno := tail^.seqno + 1;
                END (*IF*);
                INC (M^.count);
                INC (M^.recent);
                IF NOT (seen IN flags) THEN
                    INC (M^.unseen);
                    IF M^.firstunseen = 0 THEN
                        M^.firstunseen := R^.seqno;
                    END (*IF*);
                END (*IF*);
                Obtain (M^.NextUIDLock);
                R^.UID := M^.NextUID;
                INC (M^.NextUID);
                Release (M^.NextUIDLock);

                HideFile (NewName, FALSE);
                M^.changed := TRUE;
                StatusUpdates (M);

            END (*IF*);
            Release (M^.access);
        END (*IF*);

        RETURN success;
    END DoAddMessage;

(************************************************************************)
(*                          MOVING MESSAGES                             *)
(************************************************************************)

PROCEDURE CopySet (set: MessageSet;  src, dst: MailboxUser;
                                           UseUID: BOOLEAN): BOOLEAN;

    (* Copies a set of messages from the src mailbox to the dst         *)
    (* mailbox.  (We are executing the UID COPY instruction if UseUID   *)
    (* is TRUE.)  If we can't do the operation it is cancelled, and     *)
    (* the copied subset is deleted from the destination, and the       *)
    (* function result is FALSE.                                        *)
    (* On exit, the set of message numbers has been discarded.          *)

    TYPE FileList = POINTER TO
                        RECORD
                            next: FileList;
                            file: FilenameString;
                        END (*RECORD*);

    VAR srcbox: Mailbox;  p, head, prev, this: ItemPtr;
        FL, q: FileList;
        filename: FilenameString;
        seqnum: CARDINAL;
        success: BOOLEAN;

    BEGIN
        success := TRUE;
        head := NIL;  prev := NIL;
        FL := NIL;
        IF src = NIL THEN
            srcbox := NIL;
        ELSE
            srcbox := src^.box;
        END (*IF*);
        IF srcbox = NIL THEN
            p := NIL;
        ELSE
            p := srcbox^.itemlist;
        END (*IF*);
        IF p <> NIL THEN
            Obtain (srcbox^.access);
            WHILE (p <> NIL) AND success DO
                NextInSet (p, set, UseUID);
                IF p <> NIL THEN

                    (* Remember the destination filename, in case we have   *)
                    (* to unwind the operation.                             *)

                    NEW (q);
                    q^.next := FL;  FL := q;
                    MakeFullName (dst^.box^.fdir, p^.shortname, q^.file);

                    (* Copy one file, plus its flags, etc. *)

                    MakeFullName (srcbox^.fdir, p^.shortname, filename);
                    success := CopyFile (filename, dst^.box^.fdir);
                    NEW (this);
                    WITH this^ DO
                        next := NIL;
                        seqno := 0;
                        UID := 0;
                        flags := p^.flags + MessageFlagSet{recent};
                        absent := NOT success;
                        shortname := p^.shortname;
                        structure := NIL;
                    END (*WITH*);
                    IF head = NIL THEN
                        head := this;
                    ELSE
                        prev^.next := this;
                    END (*IF*);
                    prev := this;
                    p := p^.next;

                END (*IF*);
            END (*WHILE*);
            Release (srcbox^.access);
            DiscardSet (set);

            (* The standard seems to say that we should cancel the entire   *)
            (* operation in case of failure, even if some of the files were *)
            (* successfully copied.  In the case of success, we can simply  *)
            (* throw away the FL list.                                      *)

            WHILE FL <> NIL DO
                IF NOT success THEN
                    DeleteFile (FL^.file);
                END (*IF*);
                q := FL^.next;
                DISPOSE (FL);
                FL := q;
            END (*WHILE*);
        END (*IF*);

        (* The standard is unclear on what should be notified here, but *)
        (* we certainly should update dst^.box^.itemlist, and           *)
        (* it looks to me as if we should report the fact that a new    *)
        (* message has appeared in the destination mailbox.             *)

        IF head <> NIL THEN
            IF success THEN

                (* Add the 'head' list to destination itemlist. *)

                head := SortList (head);
                prev := dst^.box^.itemlist;
                IF prev = NIL THEN
                    seqnum := 0;
                    dst^.box^.itemlist := head;
                ELSE
                    WHILE prev^.next <> NIL DO
                        prev := prev^.next;
                    END (*WHILE*);
                    seqnum := prev^.seqno;
                    prev^.next := head;
                END (*IF*);

                (* Update counts, sequence numbers, UIDs. *)

                this := head;
                Obtain (dst^.box^.NextUIDLock);
                WHILE this <> NIL DO
                    INC (dst^.box^.count);
                    INC (seqnum);
                    this^.seqno := seqnum;
                    this^.UID := dst^.box^.NextUID;
                    INC (dst^.box^.NextUID);
                    INC (dst^.box^.recent);
                    this := this^.next;
                END (*WHILE*);
                Release (dst^.box^.NextUIDLock);

                dst^.box^.changed := TRUE;
                StatusUpdates (dst^.box);

            ELSE
                (* Cancel the operation. *)

                WHILE head <> NIL DO
                    this := head;
                    head := head^.next;
                    DISPOSE (this);
                END (*WHILE*);
            END (*IF*);
        END (*IF*);

        RETURN success;

    END CopySet;

(************************************************************************)

PROCEDURE DoExpunge (RC: ReplyCxt;  MBU: MailboxUser): BOOLEAN;

    (* Deletes all messages with the \Deleted flag set, sending back an *)
    (* untagged EXPUNGE response.                                       *)

    VAR M: Mailbox;  success: BOOLEAN;

    BEGIN
        IF MBU = NIL THEN
            M := NIL;
        ELSE
            M := MBU^.box;
        END (*IF*);
        success := (M <> NIL) AND NOT MBU^.readonly;
        IF success THEN
            Obtain (M^.access);
            RemoveDeleted (RC, M, TRUE);
            M^.changed := TRUE;
            Release (M^.access);
        END (*IF*);
        RETURN success;
    END DoExpunge;

(************************************************************************)
(*                              SEARCHING                               *)
(************************************************************************)

TYPE
    (* Linked list of all messages satisfying our search criteria so far. *)

    SearchPtr = POINTER TO
                    RECORD
                        next: SearchPtr;
                        this: ItemPtr;
                    END (*RECORD*);

    (* A SearchList keeps track of the list of messages that we have    *)
    (* so far in our search.  If the IncludeAll field is TRUE, this     *)
    (* means all items in the mailbox (even though head is NIL in this  *)
    (* case); this is the default starting point in a search.  Since    *)
    (* there is an implied AND between search terms, the list is        *)
    (* gradually trimmed down as we proceed.                            *)

    SearchList = RECORD
                     box: Mailbox;
                     IncludeAll: BOOLEAN;
                     head: SearchPtr;
                 END (*RECORD*);

    SearchForType = (S_ALL, S_ANSWERED, S_BCC, S_BEFORE,
                     S_BODY, S_CC, S_DELETED, S_DRAFT, S_FLAGGED,
                     S_FROM, S_HEADER, S_KEYWORD, S_LARGER, S_NEW,
                     S_OLD, S_ON, S_RECENT, S_SEEN, S_SENTBEFORE,
                     S_SENTON, S_SENTSINCE, S_SINCE, S_SMALLER,
                     S_SUBJECT, S_TEXT, S_TO, S_UNANSWERED,
                     S_UNDELETED, S_UNDRAFT, S_UNFLAGGED, S_UNKEYWORD,
                     S_UNSEEN);

    KeywordListType = ARRAY SearchForType OF ARRAY [0..15] OF CHAR;

CONST
    SearchKeywords = KeywordListType{"ALL", "ANSWERED", "BCC", "BEFORE",
                        "BODY", "CC", "DELETED", "DRAFT", "FLAGGED",
                        "FROM", "HEADER", "KEYWORD", "LARGER", "NEW",
                        "OLD", "ON", "RECENT", "SEEN", "SENTBEFORE",
                        "SENTON", "SENTSINCE", "SINCE", "SMALLER",
                        "SUBJECT", "TEXT", "TO", "UNANSWERED",
                        "UNDELETED", "UNDRAFT", "UNFLAGGED", "UNKEYWORD",
                        "UNSEEN"};

(************************************************************************)

PROCEDURE DiscardChain (VAR (*INOUT*) list: SearchPtr);

    (* Discards the entire list. *)

    VAR next: SearchPtr;

    BEGIN
        WHILE list <> NIL DO
            next := list^.next;
            DISPOSE (list);
            list := next;
        END (*WHILE*);
    END DiscardChain;

(************************************************************************)

PROCEDURE Merge (VAR (*INOUT*) list1, list2: SearchList);

    (* Sets list1 to the logical OR of list1 and list2, leaving empty   *)
    (* the list headed by list2.head.                                   *)

    VAR prev1, p1, p2, next2: SearchPtr;

    BEGIN
        IF list1.IncludeAll THEN
            DiscardChain (list2.head);
            list2.IncludeAll := FALSE;
        ELSIF list2.IncludeAll THEN
            DiscardChain (list1.head);
            list1.IncludeAll := TRUE;
            list2.IncludeAll := FALSE;
        ELSE
            prev1 := NIL;  p1 := list1.head;
            p2 := list2.head;  list2.head := NIL;

            WHILE p2 <> NIL DO

                WHILE (p1 <> NIL) AND (p1^.this^.seqno < p2^.this^.seqno) DO
                    prev1 := p1;  p1 := p1^.next;
                END (*WHILE*);

                IF p1 = NIL THEN

                    (* Move the remainder of list2 to the tail of list1. *)

                    IF prev1 = NIL THEN
                        list1.head := p2;
                    ELSE
                        prev1^.next := p2;
                    END (*IF*);
                    p2 := NIL;

                ELSIF p1^.this^.seqno = p2^.this^.seqno THEN

                    (* Duplicate entry, discard the list2 record. *)

                    next2 := p2^.next;
                    DISPOSE (p2);
                    p2 := next2;

                ELSE
                    (* Insert the p2^ record ahead of the p1^ one. *)

                    IF prev1 = NIL THEN
                        list1.head := p2;
                    ELSE
                        prev1^.next := p2;
                    END (*IF*);
                    next2 := p2^.next;
                    p2^.next := p1;
                    p2 := next2;

                END (*IF*);

            END (*WHILE*);

        END (*IF*);

    END Merge;

(************************************************************************)

PROCEDURE Intersect (VAR (*INOUT*) list1, list2: SearchList);

    (* Sets list1 to the logical AND of list1 and list2, leaving empty  *)
    (* the list headed by list2.head.                                   *)

    VAR prev1, p1, p2, next: SearchPtr;
        equal: BOOLEAN;

    BEGIN
        IF list1.IncludeAll THEN
            IF list2.IncludeAll THEN
                list2.IncludeAll := FALSE;
            ELSE
                list1.IncludeAll := FALSE;
                list1.head := list2.head;
                list2.head := NIL;
            END (*IF*);
        ELSIF list2.IncludeAll THEN
            list2.IncludeAll := FALSE;
        ELSE
            prev1 := NIL;  p1 := list1.head;
            p2 := list2.head;  list2.head := NIL;

            WHILE p2 <> NIL DO

                IF p1 = NIL THEN

                    DiscardChain (p2);

                ELSIF p1^.this^.seqno < p2^.this^.seqno THEN

                    (* Discard the list1 record. *)

                    next := p1^.next;
                    DISPOSE (p1);
                    p1 := next;
                    IF prev1 = NIL THEN
                        list1.head := p1;
                    ELSE
                        prev1^.next := p1;
                    END (*IF*);

                ELSE

                    (* Discard the list2 record. *)

                    equal := p1^.this^.seqno = p2^.this^.seqno;
                    next := p2^.next;
                    DISPOSE (p2);
                    p2 := next;
                    IF equal THEN
                        prev1 := p1;  p1 := p1^.next;
                    END (*IF*);

                END (*IF*);

            END (*WHILE*);

            IF p1 <> NIL THEN
                DiscardChain (p1);
                IF prev1 = NIL THEN
                    list1.head := NIL;
                ELSE
                    prev1^.next := NIL;
                END (*IF*);
            END (*IF*);

        END (*IF*);

    END Intersect;

(************************************************************************)

PROCEDURE InvertList (VAR (*INOUT*) list: SearchList);

    (* Replaces list by its complement.  *)

    VAR tail: SearchPtr;

    (********************************************************************)

    PROCEDURE AddItem (item: ItemPtr);

        (* Adds a new record to the result list. *)

        VAR p: SearchPtr;

        BEGIN
            NEW (p);
            p^.next := NIL;
            p^.this := item;
            IF tail = NIL THEN
                list.head := p;
            ELSE
                tail^.next := p;
            END (*IF*);
            tail := p;
        END AddItem;

    (********************************************************************)

    VAR p1, oldhead: SearchPtr;
        allp: ItemPtr;

    BEGIN
        IF list.IncludeAll THEN
            list.IncludeAll := FALSE;
        ELSIF list.head = NIL THEN
            list.IncludeAll := TRUE;
        ELSE
            oldhead := list.head;
            p1 := oldhead;
            list.head := NIL;  tail := NIL;
            allp := list.box^.itemlist;

            WHILE allp <> NIL DO

                WHILE (allp <> NIL) AND
                      ((p1 = NIL) OR (allp^.seqno <> p1^.this^.seqno)) DO

                    AddItem (allp);
                    allp := allp^.next;

                END (*WHILE*);

                IF p1 <> NIL THEN
                    p1 := p1^.next;
                END (*IF*);

            END (*WHILE*);
            DiscardChain (oldhead);

        END (*IF*);

    END InvertList;

(************************************************************************)

PROCEDURE SetToList (set: MessageSet;  fromlist: ItemPtr;
                                             UseUID: BOOLEAN): SearchPtr;

    (* Creates a SearchList from a MessageSet. *)

    VAR result, prev, this: SearchPtr;

    BEGIN
        result := NIL;  prev := NIL;
        WHILE NonEmptySet(set) DO
            NextInSet (fromlist, set, UseUID);
            IF fromlist <> NIL THEN
                NEW (this);
                this^.next := NIL;
                this^.this := fromlist;
                IF prev = NIL THEN
                    result := this;
                ELSE
                    prev^.next := this;
                END (*IF*);
                prev := this;
            END (*IF*);
        END (*WHILE*);
        RETURN result;
    END SetToList;

(************************************************************************)

PROCEDURE SearchMatch (p: ItemPtr;  VAR (*IN*) filename: FilenameString;
                       SearchFor: SearchForType;  N: CARDINAL;
                       textarg1, textarg2: ArgPointer): BOOLEAN;

    (* Returns TRUE iff p^ matches the SearchFor criterion. *)

    VAR result: BOOLEAN;
        val64: CARD64;

    BEGIN
        CASE SearchFor OF
            | S_ANSWERED: result := answered IN p^.flags;
            | S_BEFORE:  result := PackedDate(filename) < PackDate(textarg1);
            | S_BODY:    result := FindInText (filename, textarg1^, FALSE);
            | S_DELETED: result := deleted IN p^.flags;
            | S_DRAFT:   result := draft IN p^.flags;
            | S_FLAGGED: result := flagged IN p^.flags;
            | S_HEADER:  result := FindInHeader (filename, textarg1^, textarg2^);
            | S_KEYWORD: result := FALSE;
            | S_LARGER:  val64 := SizeOfMessage (filename);
                         result := (val64.high > 0) OR (val64.low > N);
            | S_NEW:     result := (recent IN p^.flags) AND NOT (seen IN p^.flags);
            | S_OLD:     result := NOT (recent IN p^.flags);
            | S_ON:      result := PackedDate(filename) = PackDate(textarg1);
            | S_RECENT:  result := recent IN p^.flags;
            | S_SEEN:    result := seen IN p^.flags;
            | S_SENTBEFORE:
                         result := HeaderDate(filename) < PackDate(textarg1);
            | S_SENTON:  result := HeaderDate(filename) = PackDate(textarg1);
            | S_SENTSINCE:
                         result := HeaderDate(filename) >= PackDate(textarg1);
            | S_SINCE:   result := PackedDate(filename) >= PackDate(textarg1);
            | S_SMALLER: val64 := SizeOfMessage (filename);
                         result := (val64.high = 0) AND (val64.low < N);
            | S_TEXT:    result := FindInText (filename, textarg1^, TRUE);
            | S_UNANSWERED:
                         result := NOT(answered IN p^.flags);
            | S_UNDELETED:
                         result := NOT(deleted IN p^.flags);
            | S_UNDRAFT: result := NOT(draft IN p^.flags);
            | S_UNFLAGGED:
                         result := NOT(flagged IN p^.flags);
            | S_UNKEYWORD:
                         result := TRUE;
            | S_UNSEEN:  result := NOT(seen IN p^.flags);
        ELSE
                         result := TRUE;
        END (*IF*);
        RETURN result;
    END SearchMatch;

(************************************************************************)

PROCEDURE ContinueSearch (VAR (*INOUT*) list: SearchList;
                          boxdir: FilenameString;
                          SearchFor: SearchForType;  N: CARDINAL;
                          textarg1, textarg2: ArgPointer);

    (* Uses a new search term SearchFor to trim the list. *)

    VAR previous, current, next: SearchPtr;
        p: ItemPtr;
        filename: FilenameString;

    BEGIN
        previous := NIL;  current := list.head;
        IF list.IncludeAll THEN

            (* Add all matching items to list. *)

            list.IncludeAll := FALSE;
            p := list.box^.itemlist;
            WHILE p <> NIL DO
                MakeFullName (boxdir, p^.shortname, filename);
                IF SearchMatch (p, filename, SearchFor, N, textarg1, textarg2) THEN
                    NEW (current);
                    current^.next := NIL;
                    current^.this := p;
                    IF previous = NIL THEN
                        list.head := current;
                    ELSE
                        previous^.next := current;
                    END (*IF*);
                    previous := current;
                END (*IF*);
                p := p^.next;
            END (*WHILE*);

        ELSE

            (* Remove all non-matching items from list. *)

            WHILE current <> NIL DO
                next := current^.next;
                MakeFullName (boxdir, current^.this^.shortname, filename);
                IF NOT SearchMatch (current^.this, filename, SearchFor, N,
                                                textarg1, textarg2) THEN
                    IF previous = NIL THEN
                        list.head := next;
                    ELSE
                        previous^.next := next;
                    END (*IF*);
                    DISPOSE (current);
                ELSE
                    previous := current;
                END (*IF*);
                current := next;
            END (*WHILE*);

        END (*IF*);

    END ContinueSearch;

(************************************************************************)

PROCEDURE MakeSearchList (M: Mailbox;  keys: ARRAY OF CHAR): SearchList;

    TYPE CharSet = SET OF CHAR;
    CONST ExtendedDigits = CharSet{'*', '0'..'9'};

    VAR result, result1, result2: SearchList;
        textarg1, textarg2: ArgPointer;
        SearchFor: SearchForType;
        N: CARDINAL;

    BEGIN
        (* Start with an 'include everything' initial condition. *)

        WITH result DO
            box := M;
            IncludeAll := TRUE;
            head := NIL;
        END (*WITH*);
        N := 0;
        NEW (textarg1);
        NEW (textarg2);

        (* Now work through the list of search keys. *)

        WHILE keys[0] <> Nul DO

            (* Skip leading spaces. *)

            WHILE SPACE(keys) DO
            END (*WHILE*);

            (* First check for the cases with atypical syntax/semantics. *)

            IF Match (keys, "OR") THEN
                result1 := MakeSearchList (M, keys);
                result2 := MakeSearchList (M, keys);
                Merge (result1, result2);
                Intersect (result, result1);
                SearchFor := S_ALL;

            ELSIF Match (keys, "(") THEN
                result1 := MakeSearchList (M, keys);
                Intersect (result, result1);
                SearchFor := S_ALL;
                EVAL (Match (keys, ")"));

            ELSIF Match (keys, "NOT") THEN
                result1 := MakeSearchList (M, keys);
                InvertList (result1);
                Intersect (result, result1);
                SearchFor := S_ALL;

            ELSIF Match (keys, "UID") THEN
                WHILE SPACE(keys) DO
                END (*WHILE*);
                result1.box := result.box;
                result1.IncludeAll := FALSE;
                result1.head := SetToList (GetSet(keys), result.box^.itemlist, TRUE);
                Intersect (result, result1);
                SearchFor := S_ALL;

            ELSIF keys[0] IN ExtendedDigits THEN
                result1.box := result.box;
                result1.IncludeAll := FALSE;
                result1.head := SetToList (GetSet(keys), result.box^.itemlist, FALSE);
                Intersect (result, result1);
                SearchFor := S_ALL;

            ELSE
                (* End of special cases, look for the "standard" cases. *)

                SearchFor := MIN(SearchForType);
                LOOP
                    IF Match (keys, SearchKeywords[SearchFor]) THEN
                        EXIT (*LOOP*);
                    ELSIF SearchFor = MAX(SearchForType) THEN
                        (* Unrecognised keyword. *)
                        keys[0] := Nul;
                        SearchFor := S_ALL;
                        EXIT (*LOOP*);
                    ELSE
                        INC (SearchFor);
                    END (*IF*);
                END (*LOOP*);

                (* Some of the search keywords have an associated argument. *)

                N := 0;
                textarg1^[0] := Nul;
                textarg2^[0] := Nul;
                CASE SearchFor OF
                    | S_BEFORE, S_ON, S_SINCE, S_SENTBEFORE, S_SENTON, S_SENTSINCE:
                           EVAL(SPACE(keys) AND Date(keys,textarg1^));
                    | S_BCC, S_CC, S_FROM, S_SUBJECT, S_TO:
                           EVAL(SPACE(keys) AND AString(keys,textarg2^));
                           Strings.Assign (SearchKeywords[SearchFor], textarg1^);
                           SearchFor := S_HEADER;
                    | S_BODY, S_TEXT:
                           EVAL(SPACE(keys) AND AString(keys,textarg1^));
                    | S_HEADER:
                           EVAL(SPACE(keys) AND AString(keys,textarg1^)
                                AND SPACE(keys) AND AString(keys,textarg2^));
                    | S_KEYWORD, S_UNKEYWORD:
                           EVAL(SPACE(keys) AND Atom(FALSE,keys,textarg1^));
                    | S_LARGER, S_SMALLER:
                           EVAL(SPACE(keys) AND Number(keys,N));
                ELSE
                         (* nothing to do *)
                END (*CASE*);

            END (*IF*);

            (* Now we know what we want to search for. *)

            IF (SearchFor <> S_ALL) AND (result.IncludeAll OR (result.head <> NIL)) THEN
                ContinueSearch (result, M^.fdir, SearchFor, N, textarg1, textarg2);
            END (*IF*);

        END (*WHILE*);

        DISPOSE (textarg2);
        DISPOSE (textarg1);

        RETURN result;

    END MakeSearchList;

(************************************************************************)

PROCEDURE SearchResult (RC: ReplyCxt;  MBU: MailboxUser;
                        VAR (*INOUT*) keys: ARRAY OF CHAR;  UseUID: BOOLEAN);

    (* Responds to the SEARCH command, or the UID SEARCH command  *)
    (* if UseUID is TRUE.                                         *)
    (* The keys string is destroyed as a side-effect.             *)

    VAR M: Mailbox;  p: ItemPtr;  SP: SearchPtr;
        presult: POINTER TO ARRAY [0..4095] OF CHAR;
        pos, val: CARDINAL;
        SL: SearchList;

    BEGIN
        NEW (presult);
        presult^[0] := Nul;  pos := 0;
        IF MBU = NIL THEN
            M := NIL;
        ELSE
            M := MBU^.box;
        END (*IF*);
        IF M <> NIL THEN
            Obtain (M^.access);
            SL := MakeSearchList (M, keys);
            IF SL.IncludeAll THEN
                p := M^.itemlist;
                WHILE p <> NIL DO
                    presult^[pos] := ' ';  INC(pos);
                    IF UseUID THEN
                        val := p^.UID;
                    ELSE
                        val := p^.seqno;
                    END (*IF*);
                    ConvertCard (val, presult^, pos);
                    p := p^.next;
                END (*WHILE*);
            ELSE
                WHILE SL.head <> NIL DO
                    SP := SL.head;
                    p := SP^.this;
                    SL.head := SP^.next;
                    DISPOSE (SP);
                    presult^[pos] := ' ';  INC(pos);
                    IF UseUID THEN
                        val := p^.UID;
                    ELSE
                        val := p^.seqno;
                    END (*IF*);
                    ConvertCard (val, presult^, pos);
                END (*WHILE*);
            END (*IF*);
            Release (M^.access);
        END (*IF*);
        presult^[pos] := Nul;
        Reply2Untagged (RC, "SEARCH", presult^);
        DISPOSE (presult);
    END SearchResult;

(************************************************************************)
(*                           INITIALISATION                             *)
(************************************************************************)

BEGIN
    CreateLock (NextNameLock);
    NextName := "00000000";
    WITH MasterBoxList DO
        CreateLock (access);
        head := NIL;
    END (*WITH*);
FINALLY
    DestroyLock (MasterBoxList.access);
    DestroyLock (NextNameLock);
END Boxes.

