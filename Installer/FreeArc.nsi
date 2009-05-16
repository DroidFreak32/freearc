; Script generated by the HM NIS Edit Script Wizard.

; HM NIS Edit Wizard helper defines
!define PRODUCT_NAME "FreeArc"
!define PRODUCT_VERSION "0.51"
!define PRODUCT_PUBLISHER "Bulat Ziganshin"
!define PRODUCT_WEB_SITE "http://freearc.org"
!define PRODUCT_DIR_REGKEY "Software\Microsoft\Windows\CurrentVersion\App Paths\FreeArc.exe"
!define PRODUCT_UNINST_KEY "Software\Microsoft\Windows\CurrentVersion\Uninstall\${PRODUCT_NAME}"
!define PRODUCT_UNINST_ROOT_KEY "HKLM"

SetCompressor /SOLID lzma

!include "setenv.nsh"

; MUI 1.67 compatible ------
!include "MUI.nsh"

; MUI Settings
!define MUI_ABORTWARNING
!define MUI_COMPONENTSPAGE_NODESC
!define MUI_ICON "${NSISDIR}\Contrib\Graphics\Icons\modern-install.ico"
!define MUI_UNICON "${NSISDIR}\Contrib\Graphics\Icons\modern-uninstall.ico"
ReserveFile "Documentation\readme.txt"

; Welcome page
; !insertmacro MUI_PAGE_WELCOME
; License page
!define MUI_LICENSEPAGE_TEXT_TOP " "
!define MUI_LICENSEPAGE_TEXT_BOTTOM " "
!define MUI_LICENSEPAGE_BUTTON "I like it!"
!insertmacro MUI_PAGE_LICENSE "Documentation\readme.txt"
; Directory page
!insertmacro MUI_PAGE_DIRECTORY
; Components selection page
!insertmacro MUI_PAGE_COMPONENTS
; Instfiles page
!insertmacro MUI_PAGE_INSTFILES
; Finish page
!define MUI_FINISHPAGE_RUN "$INSTDIR\bin\FreeArc.exe"
!define MUI_FINISHPAGE_SHOWREADME "$INSTDIR\Documentation\whatsnew.txt"
!define MUI_FINISHPAGE_SHOWREADME_TEXT "Show what's new in the latest versions"
;!define MUI_FINISHPAGE_SHOWREADME_NOTCHECKED
!define MUI_FINISHPAGE_LINK "Visit http://freearc.org"
!define MUI_FINISHPAGE_LINK_LOCATION "http://freearc.org"
!insertmacro MUI_PAGE_FINISH

; Uninstaller pages
!insertmacro MUI_UNPAGE_INSTFILES

; Language files
!insertmacro MUI_LANGUAGE "English"

; Reserve files
!insertmacro MUI_RESERVEFILE_INSTALLOPTIONS

; MUI end ------

Name "${PRODUCT_NAME} ${PRODUCT_VERSION}"
!ifdef GTK
OutFile "FreeArc-install.exe"
!else
OutFile "FreeArc-update.exe"
!endif

InstallDir "$PROGRAMFILES\FreeArc"
InstallDirRegKey HKLM "${PRODUCT_DIR_REGKEY}" ""
ShowInstDetails show
ShowUnInstDetails show

Section "Install FreeArc" SEC01
  !include "FreeArc-delete-old.nsh"   ; Delete old-style winarc*.* files
  SetOverwrite try
  SetOutPath "$INSTDIR"
  File    "FreeArc.url"
  SetOutPath "$INSTDIR\Addons"
  File /r "Addons\*.*"
  SetOutPath "$INSTDIR\bin"
  File /r "bin\*.*"
  SetOutPath "$INSTDIR\Documentation"
  File /r "Documentation\*.*"
!ifdef GTK
  SetOutPath "$INSTDIR"
  File /r "GTK\*.*"
  SetOutPath "$INSTDIR"
  File /r "gtk2-themes\*.*"
!else
;Temporary - exclude from Update install in next version
  SetOutPath "$INSTDIR\bin"
  File    "GTK\bin\zlib1.dll"
!endif
SectionEnd

SubSection /e "Options" pOptions
  Section "Add to Start Menu"
    SetDetailsPrint textonly
    DetailPrint "Installing Start Menu shortcuts..."
    SetDetailsPrint listonly
    RMDir /r "$SMPROGRAMS\FreeArc"
    CreateDirectory "$SMPROGRAMS\FreeArc"
    CreateDirectory "$SMPROGRAMS\FreeArc\Documentation"
    CreateDirectory "$SMPROGRAMS\FreeArc\Documentation (rus)"
    CreateShortCut  "$SMPROGRAMS\FreeArc\Documentation\What's new.lnk" "$INSTDIR\Documentation\whatsnew.txt"
    CreateShortCut  "$SMPROGRAMS\FreeArc\Documentation\FreeArc GUI.lnk" "$INSTDIR\Documentation\FreeArc-GUI-Eng.htm"
    CreateShortCut  "$SMPROGRAMS\FreeArc\Documentation\FreeArc command line.lnk" "$INSTDIR\Documentation\FreeArc036-eng.htm"
    CreateShortCut  "$SMPROGRAMS\FreeArc\Documentation (rus)\What's new.lnk" "$INSTDIR\Documentation\whatsnew-rus.txt"
    CreateShortCut  "$SMPROGRAMS\FreeArc\Documentation (rus)\FreeArc GUI.lnk" "$INSTDIR\Documentation\FreeArc-GUI-Rus.htm"
    CreateShortCut  "$SMPROGRAMS\FreeArc\Documentation (rus)\FreeArc command line.lnk" "$INSTDIR\Documentation\FreeArc040-rus.htm"
    CreateShortCut  "$SMPROGRAMS\FreeArc\FreeArc.lnk"     "$INSTDIR\bin\FreeArc.exe"
    CreateShortCut  "$SMPROGRAMS\FreeArc\Change skin.lnk" "$INSTDIR\bin\gtk2_prefs.exe"
    CreateShortCut  "$SMPROGRAMS\FreeArc\Website.lnk"     "$INSTDIR\${PRODUCT_NAME}.url"
    CreateShortCut  "$SMPROGRAMS\FreeArc\Uninstall.lnk"   "$INSTDIR\uninst.exe"
  SectionEnd

  Section "Add shortcut to desktop"
    CreateShortCut  "$DESKTOP\FreeArc.lnk" "$INSTDIR\bin\FreeArc.exe"
  SectionEnd

  Section "Add shortcut to Quick Launch menu"
    CreateShortCut  "$QUICKLAUNCH\FreeArc.lnk" "$INSTDIR\bin\FreeArc.exe"
  SectionEnd

  Section "Add FreeArc to PATH"
    SetDetailsPrint textonly
    DetailPrint "Setting PATH variable..."
    SetDetailsPrint listonly
    Push "$INSTDIR\bin"
    Call AddToPath
  SectionEnd
SubSectionEnd

Section -AdditionalIcons
SectionEnd

Section -Post
  WriteUninstaller "$INSTDIR\uninst.exe"
  WriteRegStr HKLM "${PRODUCT_DIR_REGKEY}" "" "$INSTDIR\FreeArc.exe"
  WriteRegStr ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}" "DisplayName" "$(^Name)"
  WriteRegStr ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}" "UninstallString" "$INSTDIR\uninst.exe"
  WriteRegStr ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}" "DisplayIcon" "$INSTDIR\bin\FreeArc.exe"
  WriteRegStr ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}" "DisplayVersion" "${PRODUCT_VERSION}"
  WriteRegStr ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}" "URLInfoAbout" "${PRODUCT_WEB_SITE}"
  WriteRegStr ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}" "Publisher" "${PRODUCT_PUBLISHER}"
SectionEnd


Function un.onInit
  MessageBox MB_ICONQUESTION|MB_YESNO|MB_DEFBUTTON2 "Are you sure you want to completely remove $(^Name) and all of its components?" IDYES +2
  Abort
FunctionEnd

Section Uninstall
  SetShellVarContext "all"
    RMDir /r "$SMPROGRAMS\FreeArc"
    Delete   "$DESKTOP\FreeArc.lnk"
    Delete   "$QUICKLAUNCH\FreeArc.lnk"
    Push "$INSTDIR\bin"
    Call un.RemoveFromPath

  SetShellVarContext "current"
    RMDir /r "$SMPROGRAMS\FreeArc"
    Delete   "$DESKTOP\FreeArc.lnk"
    Delete   "$QUICKLAUNCH\FreeArc.lnk"
    Push "$INSTDIR\bin"
    Call un.RemoveFromPath

  !include "FreeArc-delete-old.nsh"   ; Delete winarc*.*
  !include "FreeArc-delete-all.nsh"
  RMDir    "$INSTDIR"

  DeleteRegKey ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}"
  DeleteRegKey HKLM "${PRODUCT_DIR_REGKEY}"

  SetAutoClose true
SectionEnd

Function un.onUninstSuccess
  HideWindow
  MessageBox MB_ICONINFORMATION|MB_OK "$(^Name) was successfully removed from your computer."
FunctionEnd
