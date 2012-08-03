@echo off
set ECL=C:\Program Files\ECL

set LIB=%ECL%
call "C:\Program Files\Microsoft Visual Studio 10.0\VC\bin\vcvars32.bat"

"%ECL%\ecl" -norc -shell make-executable.lisp

echo Now copy that to hyde-server.exe
echo Its dependencies are: ecl.dll sockets.fas and must be re-copied
echo from the ECL install dir after each ECL update
pause
