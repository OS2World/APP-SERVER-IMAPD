/*----------------------------------------------------------
   Appends a build level to IMAPD.EXE.

           Author:       Peter Moylan
           Last revised: 5 January 2013

   Usage:
           bldlvl ver

           where ver is the version string

------------------------------------------------------------*/

parse arg ver
projHost = "PJM2"
timestamp = LEFT(DATE() TIME(),25)LEFT(projHost,10)
signature0 = "@#Peter Moylan:"ver"#@##1## "timestamp"::EN:AU:::@@"
outfile = "level.txt"
"@DEL "outfile" 2> nul"
CALL LINEOUT outfile, signature0||"IMAP daemon for OS/2 and eCS"
CALL STREAM outfile,'C','CLOSE'
"@copy imapd.exe /B + level.txt imapd.exe /B > nul"
"@DEL "outfile

exit

