/* Build script for IMAPD.  Rexx used because some operations need it. */

'del imapd*.zip 2> nul'
cd doc
'ipfc -i imapd.ipf'
cd ..
'xc =p imapd.prj'
'\apps\lxlite\lxlite *.exe'

/* The next three lines can be removed if you don't have Perl. */

'call PerlEnv.cmd'
perl 'D:\Apps\scripts\makexqs.pl' imapd.map
say "imapd.sym and imapd.xqs should now exist"

ver = version()
call bldlvl ver

/* Create the zip file for the binary distribution. */

'copy ..\general\doc\gpl.txt'
mkdir temp
cd temp
mkdir doc
'copy ..\doc\changes.doc doc'
'copy ..\doc\imapd.ipf doc'
'copy ..\README.IMAPD'
'copy ..\file_id.diz'
'copy ..\gpl.txt'
'copy ..\doc\imapd.inf'
'copy ..\imapd.exe'
'copy ..\imapd.map'
'copy ..\imapd.sym'
'copy ..\imapd.xqs'
'copy ..\shutimap.cmd'
'zip -q -r ..\imapd.zip .'
'del doc\* /n'
rmdir doc
'del * /n'
cd ..
'rename imapd.zip imapd'ver'.zip'

/* Now zip up the source files. */

'del src.zip 2> nul'
'Imports IMAPD | zip -q -j -u src.zip -@'

/* Reorganise the source file zip structure.  */

'cd temp'
'mkdir IMAPD'
'move ..\gpl.txt .'
'copy ..\BUILDING'
'copy ..\makezip.cmd'
'copy ..\version.cmd'
'copy ..\bldlvl.cmd'
'copy ..\imapd.prj IMAPD'
'move ..\src.zip IMAPD'
'cd IMAPD'
'unzip -q -o src.zip'
'del src.zip /N'
'cd ..'
'zip -q -r ..\imapdSrc'ver'.zip .'

/* Remove temporary files. */

'del IMAPD\* /N'
'rmdir IMAPD'
'del * /N'
'cd ..'
rmdir temp

