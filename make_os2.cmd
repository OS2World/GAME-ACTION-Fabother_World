@echo off

rem  Creates a binary file for OS/2 and eComStation (with Open Watcom C)
rem  Cleaning: make_os2.cmd clean
rem
rem  Andrey Vasilkin, 2014
rem  digi@os2.snc.ru

wmake -h -f makefile.os2 %1
