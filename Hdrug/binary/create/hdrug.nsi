; The name of the installer
Name "hdrug"

; The file to write
OutFile "InstallHdrug.exe"

; The default installation directory
InstallDir $PROGRAMFILES\hdrug
; Registry key to check for directory (so if you install again, it will 
; overwrite the old one automatically)
InstallDirRegKey HKLM SOFTWARE\hdrug "Install_Dir"

; The text to prompt the user to enter a directory
ComponentText "This will install Hdrug on your computer."
; The text to prompt the user to enter a directory
DirText "Choose a directory to install in to:"

LicenseText "Copyright Notice" "ok"
LicenseData ..\..\Copyright


; The stuff to install
Section "Hdrug (required)"
  ; Set output path to the installation directory.
  SetOutPath $INSTDIR
  ; Put file there
  File /r "hdrug"
  File "..\vcredist_x86.exe"
  Exec $INSTDIR\vcredist_x86.exe
  ; Write the installation path into the registry
  WriteRegStr HKLM SOFTWARE\hdrug "Install_Dir" "$INSTDIR"
  ; Write the uninstall keys for Windows
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\hdrug" "DisplayName" "hdrug (remove only)"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\hdrug" "UninstallString" '"$INSTDIR\uninstall.exe"'
  WriteUninstaller "uninstall.exe"
SectionEnd

Section "Hdrug scripts (recommended)"

  FileOpen $1 $INSTDIR\hdrug\hdrug.bat w
  GetFullPathName /SHORT $2 $INSTDIR\hdrug
  FileWrite $1 "set hdrugdir=$2"
  FileWrite $1 "$\r$\n"
  FileWrite $1 "$2\hdrug.exe -flag hdrug_library $$env(hdrugdir)\\Tcl -flag tex_library $$hdrugdir\\Tex -cmd asserta(library_directory('$$hdrugdir\\Tcl')) blt=noblt -flag blt_library blt24 tkconsol=on %1 %2 %3 %4 %5 %6 %7 %8 %9"
  FileClose $1
  GetFullPathName /SHORT $2 $INSTDIR\hdrug\hdrug.bat

  SetOutPath $INSTDIR\hdrug\Applications\Ale
  FileOpen $1 $INSTDIR\hdrug\Applications\Ale\ale.bat w
  FileWrite $1 "$2 -l start %1 %2 %3 %4 %5 %6 %7 %8 %9 $\r$\n"
  FileClose $1
  SetOutPath $INSTDIR

  SetOutPath $INSTDIR\hdrug\Applications\Alvey
  FileOpen $1 $INSTDIR\hdrug\Applications\Alvey\alvey.bat w
  FileWrite $1 "$2 -l start %1 %2 %3 %4 %5 %6 %7 %8 %9 $\r$\n"
  FileClose $1
  SetOutPath $INSTDIR

  SetOutPath $INSTDIR\hdrug\Applications\CBCG
  FileOpen $1 $INSTDIR\hdrug\Applications\CBCG\CBCG.bat w
  FileWrite $1 "$2 -l start %1 %2 %3 %4 %5 %6 %7 %8 %9 $\r$\n"
  FileClose $1
  SetOutPath $INSTDIR

  SetOutPath $INSTDIR\hdrug\Applications\Cfg
  FileOpen $1 $INSTDIR\hdrug\Applications\Alvey\cfg.bat w
  FileWrite $1 "$2 -l start %1 %2 %3 %4 %5 %6 %7 %8 %9 $\r$\n"
  FileClose $1
  SetOutPath $INSTDIR

  SetOutPath $INSTDIR\hdrug\Applications\Chat
  FileOpen $1 $INSTDIR\hdrug\Applications\Chat\chat.bat w
  FileWrite $1 "$2 -l start %1 %2 %3 %4 %5 %6 %7 %8 %9 $\r$\n"
  FileClose $1
  SetOutPath $INSTDIR

  SetOutPath $INSTDIR\hdrug\Applications\Dcg
  FileOpen $1 $INSTDIR\hdrug\Applications\Dcg\dcg.bat w
  FileWrite $1 "$2 -l start %1 %2 %3 %4 %5 %6 %7 %8 %9 $\r$\n"
  FileClose $1
  SetOutPath $INSTDIR

  SetOutPath $INSTDIR\hdrug\Applications\Extraposition
  FileOpen $1 $INSTDIR\hdrug\Applications\Extraposition\extraposition.bat w
  FileWrite $1 "$2 -l start %1 %2 %3 %4 %5 %6 %7 %8 %9 $\r$\n"
  FileClose $1
  SetOutPath $INSTDIR

  SetOutPath $INSTDIR\hdrug\Applications\GenBook
  FileOpen $1 $INSTDIR\hdrug\Applications\GenBook\Genbook.bat w
  FileWrite $1 "$2 -l start %1 %2 %3 %4 %5 %6 %7 %8 %9 $\r$\n"
  FileClose $1
  SetOutPath $INSTDIR

  SetOutPath $INSTDIR\hdrug\Applications\LexRules
  FileOpen $1 $INSTDIR\hdrug\Applications\LexRules\LexRules.bat w
  FileWrite $1 "$2 -l start %1 %2 %3 %4 %5 %6 %7 %8 %9 $\r$\n"
  FileClose $1
  SetOutPath $INSTDIR

  SetOutPath $INSTDIR\hdrug\Applications\Sdcg
  FileOpen $1 $INSTDIR\hdrug\Applications\Scg\Sdcg.bat w
  FileWrite $1 "$2 -l start %1 %2 %3 %4 %5 %6 %7 %8 %9 $\r$\n"
  FileClose $1
  SetOutPath $INSTDIR

  SetOutPath $INSTDIR\hdrug\Applications\Shpsg
  FileOpen $1 $INSTDIR\hdrug\Applications\Shpsg\shpsg.bat w
  FileWrite $1 "$2 -l start %1 %2 %3 %4 %5 %6 %7 %8 %9 $\r$\n"
  FileClose $1
  SetOutPath $INSTDIR

  SetOutPath $INSTDIR\hdrug\Applications\Tag
  FileOpen $1 $INSTDIR\hdrug\Applications\Tag\tag.bat w
  FileWrite $1 "$2 -l start %1 %2 %3 %4 %5 %6 %7 %8 %9 $\r$\n"
  FileClose $1
  SetOutPath $INSTDIR

SectionEnd

; optional section
Section "Start Menu Shortcuts (requires scripts)"
  CreateDirectory "$SMPROGRAMS\hdrug"
; Hdrug uninstall
  CreateShortCut "$SMPROGRAMS\hdrug\Uninstall.lnk" "$INSTDIR\uninstall.exe" "" "$INSTDIR\uninstall.exe" 0
; Hdrug 
  SetOutPath $INSTDIR\hdrug
  CreateShortCut "$SMPROGRAMS\hdrug\manualh.lnk" "$INSTDIR\hdrug\Manual\index.html"
  CreateShortCut "$SMPROGRAMS\hdrug\manualf.lnk" "$INSTDIR\hdrug\Manual\manual.pdf"
  CreateShortCut "$SMPROGRAMS\hdrug\manualg.lnk" "$INSTDIR\hdrug\Manual\manual.ps"
SetOutPath $INSTDIR
; Hdrug with Ale
  SetOutPath $INSTDIR\hdrug\Applications\Ale
  CreateShortCut "$SMPROGRAMS\hdrug\ale.lnk" "$INSTDIR\hdrug\Applications\Ale\ale.bat" "" "$INSTDIR\hdrug\hdrug.ico"
  SetOutPath $INSTDIR
; Hdrug with Alvey
  SetOutPath $INSTDIR\hdrug\Applications\Alvey
  CreateShortCut "$SMPROGRAMS\hdrug\alvey.lnk" "$INSTDIR\hdrug\Applications\Alvey\alvey.bat" "" "$INSTDIR\hdrug\hdrug.ico"
  SetOutPath $INSTDIR
  SetOutPath $INSTDIR\hdrug\Applications\CBCG
  CreateShortCut "$SMPROGRAMS\hdrug\cbcg.lnk" "$INSTDIR\hdrug\Applications\CBCG\cbcg.bat" "" "$INSTDIR\hdrug\hdrug.ico"
  SetOutPath $INSTDIR
  SetOutPath $INSTDIR\hdrug\Applications\Cfg
  CreateShortCut "$SMPROGRAMS\hdrug\cfg.lnk" "$INSTDIR\hdrug\Applications\Cfg\cfg.bat" "" "$INSTDIR\hdrug\hdrug.ico"
  SetOutPath $INSTDIR
  SetOutPath $INSTDIR\hdrug\Applications\Chat
  CreateShortCut "$SMPROGRAMS\hdrug\chat.lnk" "$INSTDIR\hdrug\Applications\Chat\chat.bat" "" "$INSTDIR\hdrug\hdrug.ico"
  SetOutPath $INSTDIR
  SetOutPath $INSTDIR\hdrug\Applications\Dcg
  CreateShortCut "$SMPROGRAMS\hdrug\dcg.lnk" "$INSTDIR\hdrug\Applications\Dcg\dcg.bat" "" "$INSTDIR\hdrug\hdrug.ico"
  SetOutPath $INSTDIR
  SetOutPath $INSTDIR\hdrug\Applications\Extraposition
  CreateShortCut "$SMPROGRAMS\hdrug\extraposition.lnk" "$INSTDIR\hdrug\Applications\Extraposition\extraposition.bat" "" "$INSTDIR\hdrug\hdrug.ico"
  SetOutPath $INSTDIR
  SetOutPath $INSTDIR\hdrug\Applications\GenBook
  CreateShortCut "$SMPROGRAMS\hdrug\genbook.lnk" "$INSTDIR\hdrug\Applications\GenBook\genbook.bat" "" "$INSTDIR\hdrug\hdrug.ico"
  SetOutPath $INSTDIR
  SetOutPath $INSTDIR\hdrug\Applications\LexRules
  CreateShortCut "$SMPROGRAMS\hdrug\lexrules.lnk" "$INSTDIR\hdrug\Applications\LexRules\lexrules.bat" "" "$INSTDIR\hdrug\hdrug.ico"
  SetOutPath $INSTDIR
  SetOutPath $INSTDIR\hdrug\Applications\Sdcg
  CreateShortCut "$SMPROGRAMS\hdrug\sdcg.lnk" "$INSTDIR\hdrug\Applications\Sdcg\sdcg.bat" "" "$INSTDIR\hdrug\hdrug.ico"
  SetOutPath $INSTDIR
  SetOutPath $INSTDIR\hdrug\Applications\Shpsg
  CreateShortCut "$SMPROGRAMS\hdrug\shpsg.lnk" "$INSTDIR\hdrug\Applications\Shpsg\shpsg.bat" "" "$INSTDIR\hdrug\hdrug.ico"
  SetOutPath $INSTDIR
  SetOutPath $INSTDIR\hdrug\Applications\Tag
  CreateShortCut "$SMPROGRAMS\hdrug\tag.lnk" "$INSTDIR\hdrug\Applications\Tag\tag.bat" "" "$INSTDIR\hdrug\hdrug.ico"
  SetOutPath $INSTDIR

SectionEnd

; uninstall stuff

UninstallText "This will uninstall hdrug. Hit uninstall to continue."

; special uninstall section.
Section "Uninstall"
  ; remove registry keys
  DeleteRegKey HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\hdrug"
  DeleteRegKey HKLM SOFTWARE\hdrug
  ; remove files
  Delete $INSTDIR\hdrug
  ; MUST REMOVE UNINSTALLER, too
  Delete $INSTDIR\uninstall.exe
  ; remove shortcuts, if any.
  Delete "$SMPROGRAMS\hdrug\*.*"
  ; remove directories used.
  RMDir /r "$SMPROGRAMS\hdrug"
  RMDir /r "$INSTDIR"
SectionEnd

; eof
