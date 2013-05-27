; NSIS Modern User Interface
; Ejabberd installation script

;--------------------------------
;Include Modern UI

    !include "MUI.nsh"
    !include "ejabberd.nsh" ; All release specific parameters come from this

;--------------------------------
;General

    ;Name and file
    !define PRODUCT "Ejabberd"
    Name ${PRODUCT}
    OutFile "${OUTFILEDIR}\${PRODUCT}-${VERSION}.exe"
    ShowInstDetails show
    ShowUninstDetails show

    !define MUI_ICON "ejabberd.ico"
    !define MUI_UNICON "ejabberd.ico"
    !define MUI_HEADERIMAGE                                  
    !define MUI_HEADERIMAGE_BITMAP "ejabberd_header.bmp"     
    !define MUI_WELCOMEFINISHPAGE_BITMAP "ejabberd_intro.bmp"


;--------------------------------
;Configuration

    SetCompressor lzma

;--------------------------------
;Reserve Files
  
    ReserveFile "ejabberd.ico"
    ReserveFile "ejabberd.ico"
    ReserveFile "ejabberd_header.bmp"     
    ReserveFile "ejabberd_intro.bmp"
    !ifdef HACKED_INSTALLOPTIONS
	ReserveFile "CheckUserH.ini"
	ReserveFile "CheckReqs1H.ini"
    !else
	ReserveFile "CheckUser.ini"
	ReserveFile "CheckReqs1.ini"
    !endif
    ReserveFile "CheckReqs.ini"
    ReserveFile "CheckService.ini"
    !insertmacro MUI_RESERVEFILE_INSTALLOPTIONS

;--------------------------------
;Variables

    Var MUI_TEMP
    Var STARTMENU_FOLDER
    Var ADMIN
    Var ENABLE_SERVICE
    Var ERLANG_PATH
    Var ERLANG_VERSION
    Var REQUIRED_ERLANG_VERSION
    Var OPENSSL_PATH
    Var OPENSSL_VERSION
    Var REQUIRED_OPENSSL_VERSION
    Var ERLSRV

;----------------------------------------------------------
;.onInit uses UserInfo plugin, so it's as high as possible

Function .onInit

    StrCpy $REQUIRED_ERLANG_VERSION "5.4.9"
    StrCpy $REQUIRED_OPENSSL_VERSION "0.9.7c"

    ;Default installation folder
    StrCpy $INSTDIR "$PROGRAMFILES\${PRODUCT}"

    ;Get installation folder from registry if available
    ClearErrors
    ReadRegStr $0 HKLM "SOFTWARE\${PRODUCT}" ""
    IfErrors 0 copydir
    ReadRegStr $0 HKCU "SOFTWARE\${PRODUCT}" ""
    IfErrors skipdir
    copydir:
	StrCpy $INSTDIR "$0"

    skipdir:
    ;Extract InstallOptions INI files
    !ifdef HACKED_INSTALLOPTIONS
	!insertmacro MUI_INSTALLOPTIONS_EXTRACT "CheckUserH.ini"
	!insertmacro MUI_INSTALLOPTIONS_EXTRACT "CheckReqs1H.ini"
    !else
	!insertmacro MUI_INSTALLOPTIONS_EXTRACT "CheckUser.ini"
	!insertmacro MUI_INSTALLOPTIONS_EXTRACT "CheckReqs1.ini"
    !endif
    !insertmacro MUI_INSTALLOPTIONS_EXTRACT "CheckReqs.ini"
    !insertmacro MUI_INSTALLOPTIONS_EXTRACT "CheckService.ini"
  
    ClearErrors
    UserInfo::GetName
    IfErrors admin
    Pop $0
    UserInfo::GetAccountType
    Pop $1
    StrCmp $1 "Admin" admin user

    admin:
	StrCpy $ADMIN 1
	Goto skip

    user:
	StrCpy $ADMIN 0

    skip:

FunctionEnd

;--------------------------------
;Interface Settings

    !define MUI_ABORTWARNING

;--------------------------------
;Installer/Uninstaller pages

    !insertmacro MUI_PAGE_WELCOME
    !insertmacro MUI_PAGE_LICENSE "..\..\COPYING"
    Page custom CheckReqs LeaveCheckReqs
    Page custom CheckReqs1 LeaveCheckReqs1
    Page custom CheckUser LeaveCheckUser
    Page custom CheckService LeaveCheckService
    ;!insertmacro MUI_PAGE_COMPONENTS
    !insertmacro MUI_PAGE_DIRECTORY

    !insertmacro MUI_PAGE_STARTMENU ${PRODUCT} $STARTMENU_FOLDER

    !insertmacro MUI_PAGE_INSTFILES
  
    !insertmacro MUI_UNPAGE_WELCOME
    !insertmacro MUI_UNPAGE_CONFIRM
    !insertmacro MUI_UNPAGE_INSTFILES

;--------------------------------
;Languages
 
    !insertmacro MUI_LANGUAGE "English"
  
;--------------------------------
;Language Strings

;Description

    LangString DESC_SecEjabberd ${LANG_ENGLISH} "Erlang jabber server."

;--------------------------------
;Installer Sections

Section "Ejabberd" SecEjabberd
SectionIn 1 RO

    SetOutPath "$INSTDIR"
    File /r "${TESTDIR}\doc"
    File /r "${TESTDIR}\ebin"
    File /r "${TESTDIR}\msgs"
    File /r "${TESTDIR}\win32"
    File "${TESTDIR}\*.dll"
    File "${TESTDIR}\inetrc"
    File /oname=ejabberd.cfg.example "${TESTDIR}\ejabberd.cfg"
    SetOverwrite off
    File "${TESTDIR}\ejabberd.cfg"
    SetOverwrite on
    ;File /r "${TESTDIR}\src"
    CreateDirectory "$INSTDIR\log"
  
;The startmenu stuff
    !insertmacro MUI_STARTMENU_WRITE_BEGIN ${PRODUCT}

    ;Create shortcuts
    StrCpy $0 "$SMPROGRAMS\$STARTMENU_FOLDER"
    CreateDirectory "$0"
    CreateShortCut "$0\Start Ejabberd.lnk" "$ERLANG_PATH\bin\werl.exe" \
	'-sname ejabberd -pa ebin \
	-env EJABBERD_LOG_PATH log/ejabberd.log \
	-s ejabberd -kernel inetrc \"./inetrc\" -mnesia dir \"spool\" \
	-sasl sasl_error_logger {file,\"log/erlang.log\"}' \
	$INSTDIR\win32\ejabberd.ico
    CreateShortCut "$0\Edit Config.lnk" "%SystemRoot%\system32\notepad.exe" \
	"$INSTDIR\ejabberd.cfg"
    CreateShortCut "$0\Read Docs.lnk" "$INSTDIR\doc\guide.html"
    CreateShortCut "$0\Uninstall.lnk" "$INSTDIR\Uninstall.exe"
  
    !insertmacro MUI_STARTMENU_WRITE_END

;Create Windows service
    StrCmp $ADMIN 1 0 skipservice

    StrCpy $ERLSRV ""
    Push $ERLANG_PATH
    Push erlsrv.exe
    GetFunctionAddress $0 FFCallback
    Push $0
    Call FindFiles

    StrCmp $ERLSRV "" skipservice

    nsExec::Exec '"$ERLSRV" list ejabberd'
    Pop $0
    StrCmp $0 "error" skipservice
    StrCmp $0 "0" 0 installsrv

    nsExec::ExecToLog '"$ERLSRV" remove ejabberd'
    Pop $0

    installsrv:
    nsExec::ExecToLog '"$ERLSRV" add ejabberd -stopaction "init:stop()." \
	-onfail restart -workdir "$INSTDIR" \
	-args "-s ejabberd -pa ebin \
	-kernel inetrc \\\"./inetrc\\\" \
	-env EJABBERD_LOG_PATH log/ejabberd.log \
	-sasl sasl_error_logger {file,\\\"log/erlang.log\\\"} \
	-mnesia dir \\\"spool\\\"" -d'
    Pop $0

    StrCmp $ENABLE_SERVICE 0 0 skipservice
    nsExec::ExecToLog '"$ERLSRV" disable ejabberd'
    Pop $0

    skipservice:

    ;Create uninstaller
    WriteUninstaller "$INSTDIR\Uninstall.exe"

    StrCpy $1 "Software\Microsoft\Windows\CurrentVersion\Uninstall\${PRODUCT}"
    StrCmp $ADMIN 1 admin2

    WriteRegStr HKCU "Software\${PRODUCT}" "" "$INSTDIR"
    WriteRegStr HKCU "$1" "DisplayName" "${PRODUCT} ${VERSION}"
    WriteRegStr HKCU "$1" "UninstallString" "$INSTDIR\Uninstall.exe"
    WriteRegDWORD HKCU "$1" "NoModify" 1
    WriteRegDWORD HKCU "$1" "NoRepair" 1
    Goto done2

    admin2:
    WriteRegStr HKLM "Software\${PRODUCT}" "" "$INSTDIR"
    WriteRegStr HKLM "Software\${PRODUCT}" "Erlsrv" "$ERLSRV"
    WriteRegStr HKLM "$1" "DisplayName" "${PRODUCT} ${VERSION}"
    WriteRegStr HKLM "$1" "UninstallString" "$INSTDIR\Uninstall.exe"
    WriteRegDWORD HKLM "$1" "NoModify" 1
    WriteRegDWORD HKLM "$1" "NoRepair" 1

    done2:

SectionEnd ; SecEjabberd

Function FFCallback

    Exch $0
    StrCpy $ERLSRV $0
    Pop $0
    Push "stop"

FunctionEnd

;--------------------------------
;Descriptions

    !insertmacro MUI_FUNCTION_DESCRIPTION_BEGIN
    !insertmacro MUI_DESCRIPTION_TEXT ${SecEjabberd} $(DESC_SecEjabberd)
    !insertmacro MUI_FUNCTION_DESCRIPTION_END
 
;--------------------------------
;Uninstaller Section

Section "Uninstall"

    ClearErrors
    UserInfo::GetName
    IfErrors admin
    Pop $0
    UserInfo::GetAccountType
    Pop $1
    StrCmp $1 "Admin" admin

    StrCpy $ADMIN 0
    Goto skipservice
    
    admin:
	StrCpy $ADMIN 1
	ReadRegStr $ERLSRV HKLM "Software\${PRODUCT}" "Erlsrv"

	nsExec::Exec '"$ERLSRV" list ejabberd'
	Pop $0
	StrCmp $0 "error" skipservice
	StrCmp $0 "0" 0 skipservice
	
	nsExec::ExecToLog '"$ERLSRV" remove ejabberd'
	Pop $0

    skipservice:
    RMDir /r "$INSTDIR\doc"
    RMDir /r "$INSTDIR\ebin"
    RMDir /r "$INSTDIR\msgs"
    RMDir /r "$INSTDIR\win32"
    ;RMDir /r "$INSTDIR\src"
    RMDir /r "$INSTDIR\log"
    Delete "$INSTDIR\*.dll"
    Delete "$INSTDIR\inetrc"
    Delete "$INSTDIR\ejabberd.cfg.example"
    Delete "$INSTDIR\Uninstall.exe"
    RMDir "$INSTDIR"  

    !insertmacro MUI_STARTMENU_GETFOLDER ${PRODUCT} $MUI_TEMP
    
    Delete "$SMPROGRAMS\$MUI_TEMP\Start Ejabberd.lnk"
    Delete "$SMPROGRAMS\$MUI_TEMP\Edit Config.lnk"
    Delete "$SMPROGRAMS\$MUI_TEMP\Read Docs.lnk"
    Delete "$SMPROGRAMS\$MUI_TEMP\Uninstall.lnk"
  
    ;Delete empty start menu parent diretories
    StrCpy $MUI_TEMP "$SMPROGRAMS\$MUI_TEMP"
 
    startMenuDeleteLoop:
	RMDir $MUI_TEMP
	GetFullPathName $MUI_TEMP "$MUI_TEMP\.."
    
	IfErrors startMenuDeleteLoopDone
  
	StrCmp $MUI_TEMP $SMPROGRAMS startMenuDeleteLoopDone startMenuDeleteLoop
    startMenuDeleteLoopDone:

    StrCpy $1 "Software\Microsoft\Windows\CurrentVersion\Uninstall\${PRODUCT}"
    StrCmp $ADMIN 1 admin1
    DeleteRegKey HKCU "Software\${PRODUCT}"
    DeleteRegKey HKCU $1
    Goto done
    admin1:
    DeleteRegKey HKLM "Software\${PRODUCT}"
    DeleteRegKey HKLM $1

    done:

SectionEnd

LangString TEXT_CU_TITLE ${LANG_ENGLISH} "Checking User Privileges"
LangString TEXT_CU_SUBTITLE ${LANG_ENGLISH} "Checking user privileged required to install Ejabberd."

Function CheckUser

    StrCmp $ADMIN 1 0 showpage
    Abort

    showpage:
	!insertmacro MUI_HEADER_TEXT $(TEXT_CU_TITLE) $(TEXT_CU_SUBTITLE)

	!ifdef HACKED_INSTALLOPTIONS
	    !insertmacro MUI_INSTALLOPTIONS_INITDIALOG "CheckUserH.ini"
	    !insertmacro MUI_INSTALLOPTIONS_READ $0 "CheckUserH.ini" "Field 2" "State"
	    GetDlgItem $1 $HWNDPARENT 1
	    EnableWindow $1 $0
	!else
	    !insertmacro MUI_INSTALLOPTIONS_INITDIALOG "CheckUser.ini"
	!endif
	
	!insertmacro MUI_INSTALLOPTIONS_SHOW

FunctionEnd

Function LeaveCheckUser

    !ifdef HACKED_INSTALLOPTIONS
	!insertmacro MUI_INSTALLOPTIONS_READ $0 "CheckUserH.ini" "Settings" "State"
	StrCmp $0 0 validate  ;Next button?
	StrCmp $0 2 checkbox  ;checkbox?
	Abort                 ;Return to the page

	checkbox:
	    !insertmacro MUI_INSTALLOPTIONS_READ $0 "CheckUserH.ini" "Field 2" "State"
	    GetDlgItem $1 $HWNDPARENT 1
	    EnableWindow $1 $0
	    Abort

	validate:
    !endif

FunctionEnd

LangString TEXT_CU_TITLE ${LANG_ENGLISH} "Configuring Ejabberd Service"
LangString TEXT_CU_SUBTITLE ${LANG_ENGLISH} "Configuring Ejabberd Service."

Function CheckService

    StrCmp $ADMIN 0 0 showpage
    Abort

    showpage:
	!insertmacro MUI_HEADER_TEXT $(TEXT_CU_TITLE) $(TEXT_CU_SUBTITLE)

	!insertmacro MUI_INSTALLOPTIONS_INITDIALOG "CheckService.ini"
	
	!insertmacro MUI_INSTALLOPTIONS_SHOW

FunctionEnd

Function LeaveCheckService

    !insertmacro MUI_INSTALLOPTIONS_READ $0 "CheckService.ini" "Field 2" "State"
    StrCmp $0 0 0 autostart
    StrCpy $ENABLE_SERVICE 0
    Goto endfun

    autostart:
    StrCpy $ENABLE_SERVICE 1

    endfun:

FunctionEnd

LangString TEXT_CR_TITLE ${LANG_ENGLISH} "Unsatisfied Requirements"
LangString TEXT_CR_SUBTITLE ${LANG_ENGLISH} "Unsatisfied Ejabberd requirements found."

Function CheckReqs

    Push "HKLM"
    Call FindErlang
    Pop $ERLANG_PATH
    Pop $ERLANG_VERSION
    StrCmp $ERLANG_PATH "" 0 abort
    Push "HKCU"
    Call FindErlang
    Pop $ERLANG_PATH
    Pop $ERLANG_VERSION
    StrCmp $ERLANG_PATH "" 0 abort

    !insertmacro MUI_HEADER_TEXT $(TEXT_CR_TITLE) $(TEXT_CR_SUBTITLE)
    
    !insertmacro MUI_INSTALLOPTIONS_INITDIALOG "CheckReqs.ini"
    GetDlgItem $R0 $HWNDPARENT 1
    EnableWindow $R0 0


    !insertmacro MUI_INSTALLOPTIONS_SHOW

    abort:
	Abort

FunctionEnd

Function LeaveCheckReqs

    Abort

FunctionEnd

Function CheckReqs1

    Push "HKLM"
    Call FindOpenSSL
    Pop $OPENSSL_PATH
    Pop $OPENSSL_VERSION
    StrCmp $OPENSSL_PATH "" 0 abort
    Push "HKCU"
    Call FindOpenSSL
    Pop $OPENSSL_PATH
    Pop $OPENSSL_VERSION
    StrCmp $OPENSSL_PATH "" 0 abort

    !insertmacro MUI_HEADER_TEXT $(TEXT_CR_TITLE) $(TEXT_CR_SUBTITLE)
    
    !ifdef HACKED_INSTALLOPTIONS
	!insertmacro MUI_INSTALLOPTIONS_INITDIALOG "CheckReqs1H.ini"
	!insertmacro MUI_INSTALLOPTIONS_READ $0 "CheckReqs1H.ini" "Field 3" "State"
	GetDlgItem $1 $HWNDPARENT 1
	EnableWindow $1 $0
    !else
	!insertmacro MUI_INSTALLOPTIONS_INITDIALOG "CheckReqs1.ini"
    !endif

    !insertmacro MUI_INSTALLOPTIONS_SHOW

    abort:
	Abort

FunctionEnd

Function LeaveCheckReqs1

    !ifdef HACKED_INSTALLOPTIONS
	!insertmacro MUI_INSTALLOPTIONS_READ $0 "CheckReqs1H.ini" "Settings" "State"
	StrCmp $0 0 validate  ;Next button?
	StrCmp $0 3 checkbox  ;checkbox?
	Abort                 ;Return to the page

	checkbox:
	    !insertmacro MUI_INSTALLOPTIONS_READ $0 "CheckReqs1H.ini" "Field 3" "State"
	    GetDlgItem $1 $HWNDPARENT 1
	    EnableWindow $1 $0
	    Abort

	validate:
    !endif

FunctionEnd

Function FindErlang

    Exch $R0
    Push $R1
    Push $R2
    Push $R3
    Push $R4
    Push $R5
    
    StrCpy $R1 0
    StrCpy $R2 "SOFTWARE\Ericsson\Erlang"

    loop:
	StrCmp $R0 HKLM h1
	    EnumRegKey $R3 HKCU $R2 $R1
	    Goto l1
	    h1:
	    EnumRegKey $R3 HKLM $R2 $R1
	    l1:
	IntOp $R1 $R1 + 1
	StrCmp $R3 "" endloop
	ClearErrors
	StrCmp $R0 HKLM h2
	    ReadRegStr $R4 HKCU "$R2\$R3" ""
	    Goto l2
	    h2:
	    ReadRegStr $R4 HKLM "$R2\$R3" ""
	    l2:
	IfFileExists "$R4\bin\erl.exe" 0 loop
	Push $REQUIRED_ERLANG_VERSION
	Push $R3
	Call CompareVersions
	Pop $R5
	StrCmp $R5 1 get
	Goto loop

    endloop:
	StrCpy $R4 ""

    get:
	StrCpy $R0 $R4
	StrCpy $R1 $R3

	Pop $R5
	Pop $R4
	Pop $R3
	Pop $R2
	Exch $R1
	Exch
	Exch $R0

FunctionEnd

Function FindOpenSSL

    Exch $R0
    Push $R1
    Push $R2
    Push $R3
    Push $R4
    Push $R5
    
    StrCpy $R1 0
    StrCpy $R2 "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\OpenSSL_is1"

    StrCmp $R0 HKLM h1
	ReadRegStr $R3 HKCU "$R2" "DisplayName"
	ReadRegStr $R4 HKCU "$R2" "Inno Setup: App Path"
	Goto l1
    h1:
	ReadRegStr $R3 HKLM "$R2" "DisplayName"
	ReadRegStr $R4 HKLM "$R2" "Inno Setup: App Path"
    l1:

    IfFileExists "$R4\bin\openssl.exe" 0 notfound
	Goto get
	; TODO check version
	;Push $REQUIRED_OPENSSL_VERSION
	;Push $R3
	;Call CompareVersions
	;Pop $R5
	;StrCmp $R5 1 get

    notfound:
	StrCpy $R4 ""

    get:
	StrCpy $R0 $R4
	StrCpy $R1 $R3

	Pop $R5
	Pop $R4
	Pop $R3
	Pop $R2
	Exch $R1
	Exch
	Exch $R0

FunctionEnd

;----------------------------------------------------------------------
; CompareVersions
; input:
;    top of stack = existing version
;    top of stack-1 = needed version
; output:
;    top of stack = 1 if current version => neded version, else 0
; version is a string in format "xx.xx.xx.xx" (number of interger sections 
; can be different in needed and existing versions)

Function CompareVersions
    ; stack: existing ver | needed ver
    Exch $R0 
    Exch
    Exch $R1 
    ; stack: $R1|$R0

    Push $R1
    Push $R0
    ; stack: e|n|$R1|$R0

    ClearErrors
    loop:
	IfErrors VersionNotFound
	Strcmp $R0 "" VersionTestEnd

	Call ParseVersion
	Pop $R0
	Exch

	Call ParseVersion
	Pop $R1 
	Exch

	IntCmp $R1 $R0 +1 VersionOk VersionNotFound
	Pop $R0
	Push $R0

    goto loop
   
    VersionTestEnd:
	Pop $R0
	Pop $R1
	Push $R1
	Push $R0
	StrCmp $R0 $R1 VersionOk VersionNotFound

    VersionNotFound:
	StrCpy $R0 "0"
	Goto end
      
    VersionOk:
	StrCpy $R0 "1"
end:
    ; stack: e|n|$R1|$R0
    Exch $R0
    Pop $R0
    Exch $R0
    ; stack: res|$R1|$R0
    Exch
    ; stack: $R1|res|$R0
    Pop $R1
    ; stack: res|$R0
    Exch
    Pop $R0
    ; stack: res
FunctionEnd

;-----------------------------------------------------------------------
; ParseVersion
; input:
;      top of stack = version string ("xx.xx.xx.xx")
; output: 
;      top of stack   = first number in version ("xx")
;      top of stack-1 = rest of the version string ("xx.xx.xx")
Function ParseVersion
    Exch $R1 ; version
    Push $R2
    Push $R3

    StrCpy $R2 1
    loop:
	StrCpy $R3 $R1 1 $R2
	StrCmp $R3 "." loopend
	StrLen $R3 $R1
	IntCmp $R3 $R2 loopend loopend
	IntOp $R2 $R2 + 1
	Goto loop
    loopend:
    Push $R1
    StrCpy $R1 $R1 $R2
    Exch $R1

    StrLen $R3 $R1
    IntOp $R3 $R3 - $R2
    IntOp $R2 $R2 + 1
    StrCpy $R1 $R1 $R3 $R2

    Push $R1

    Exch 2
    Pop $R3

    Exch 2
    Pop $R2

    Exch 2
    Pop $R1
FunctionEnd

Function FindFiles

    Exch $R5 # callback function
    Exch 
    Exch $R4 # file name
    Exch 2
    Exch $R0 # directory
    Push $R1
    Push $R2
    Push $R3
    Push $R6

    Push $R0 # first dir to search

    StrCpy $R3 1

    nextDir:
	Pop $R0
	IntOp $R3 $R3 - 1
	ClearErrors
	FindFirst $R1 $R2 "$R0\*.*"
	nextFile:
	    StrCmp $R2 "." gotoNextFile
	    StrCmp $R2 ".." gotoNextFile

	    StrCmp $R2 $R4 0 isDir
		Push "$R0\$R2"
		Call $R5
		Pop $R6
		StrCmp $R6 "stop" 0 isDir
		loop:
		    StrCmp $R3 0 done
		    Pop $R0
		    IntOp $R3 $R3 - 1
		    Goto loop

	    isDir:
		IfFileExists "$R0\$R2\*.*" 0 gotoNextFile
		    IntOp $R3 $R3 + 1
		    Push "$R0\$R2"

    gotoNextFile:
	FindNext $R1 $R2
	IfErrors 0 nextFile

    done:
	FindClose $R1
	StrCmp $R3 0 0 nextDir

    Pop $R6
    Pop $R3
    Pop $R2
    Pop $R1
    Pop $R0
    Pop $R5
    Pop $R4

FunctionEnd

