@echo off
del os2sys.dbg >nul

rem You must specify the datafiles directory with --datapath switch

raw.exe --datapath=..\bin --savepath=..\