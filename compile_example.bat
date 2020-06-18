@del system.o
@del system.nes
@del system.map.txt
@del system.labels.txt
@del system.nes.ram.nl
@del system.nes.0.nl
@del system.nes.1.nl
@del system.nes.dbg
@echo.
@echo Compiling...
cc65\bin\ca65 system.s -g -o system.o
@IF ERRORLEVEL 1 GOTO failure
@echo.
@echo Linking...
cc65\bin\ld65 -o system.nes -C system.cfg system.o -m system.map.txt -Ln system.labels.txt --dbgfile system.nes.dbg
@IF ERRORLEVEL 1 GOTO failure
@echo.
@echo Generating FCEUX debug symbols...
python example_fceux_symbols.py
@echo.
@echo Success!
@pause
@GOTO endbuild
:failure
@echo.
@echo Build error!
@pause
:endbuild
