/*----------------------------------------------------------
   Returns the version number of IMapD.

           Author:       Peter Moylan
           Last revised: 5 January 2013

   Usage:
           ver = version()

           (Run this from the IMapD top-level directory)

------------------------------------------------------------*/

DEFFile = "DEF\IMV.def"

DO FOREVER
    IF lines(DEFFile) != 1 THEN LEAVE
    parse value linein(DEFFile) with kwd'='val
    kwd = STRIP(kwd)
    IF kwd = "version" THEN LEAVE
END

/* Extra the part of val inside double quotes. */

PARSE VALUE val WITH v1 '"' version '"' v2
RETURN version

exit

