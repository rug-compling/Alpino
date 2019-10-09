SET ALPINO_PATH=%~dp0..
SET ALPINO_HOME=%ALPINO_PATH:\=/%
%ALPINO_HOME%/create_bin/Alpino.exe debug=1 tkconsol=on -flag tcl_dir %ALPINO_HOME%/src -flag hdrug_library %ALPINO_HOME%/Hdrug/Tcl -flag tex_library %ALPINO_HOME%/Hdrug/Tex -flag suite %ALPINO_HOME%/Suites/cdb end_hook=triples display_quality=on display_main_parts=on -veryfast debug=undefined demo=on -cmd "asserta(library_directory('%ALPINO_HOME%/hdrug'))" %1 %2 %3 %4 %5 %6 %7 %8 %9
