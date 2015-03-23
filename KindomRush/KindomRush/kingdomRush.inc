IFNDEF KINGDOM_RUSH

MAX_MAP_SIZE = 20
MAP_SIZE = MAX_MAP_SIZE + 1

;===================Protos====================
WinMain PROTO :DWORD,:DWORD,:DWORD,:DWORD      
WndProc PROTO :DWORD,:DWORD,:DWORD,:DWORD
PaintProc PROTO :DWORD 

;====================Data=====================
.data
	hInstance dd 0					; Windows handler
	hWnd  dd 0
	hMenu dd 0
	memDC dd 0
	imgDC dd 0
	hDC dd 0

	textBgBrush HBRUSH ?
	hBitmap   HBITMAP ?
	bgBrush   HBRUSH ?
	textFont  HFONT ?
	titleFont HFONT ?

	CommandLine dd 0
	CurrentBmp dd 0
	rect RECT <>

	
	WndWidth dd 720					;Window Parameter
	WndHeight dd 660
	ClientWidth dd 700
	ClientHeight dd 600
	WndOffX dd 850
	WndOffY dd 150
	BgColor dd 00EFF8FAh
	TextBgColor dd 00A0ADBBh
	BgBmpWidth dd 700
	BgBmpHeight dd 600
	ClientOffX dd 0
	ClientOffY dd 0
	Padding dd 12

	BmpBackground dd ?					; bmp handler
	GdiPlusStartupToken	dd ?			; GDI+ start up token
	GdiInput dd ?						; Gdiplus::GdiplusStartupInput
			
	GameTitle db "Kingdom Rush!",0		; String Definition
	FontName db "Arial", 0
	WinMsg db "YOU WIN!!!", 0
	FailMsg db "YOU LOSE!", 0
	LoadMsg db "Do you want to read the game progress?", 0
	QuitMsg db "Do you want to save the game progress?", 0
	MsgTitle db 0
	MoveDir dd 0
	ErrorTitle db "Error",0
    ClassName db "KingdomRush",0      
    WindowName db "Kingdom Rush",0      
    MenuAbout db "Help", 0
	MenuAboutHelpInfo db "Help(H)", 0      
    MenuAboutAuthor db "About Us(A)", 0   
    Author db "Author: lyz",0dh,"Date: 23/03/2015",0
	Help db "Keyboard Shortcuts",0dh,"'N' for creating a new game",0dh,"'S' for saving the game progress",0dh,"'P' for playing the background music",0dh,"'P' again for stopping the background music",0
	MenuFile db "File",0 
	MenuFileNewM db "New(medium)",0
	MenuFileNewH db "New(hard)",0 
	MenuFileSave db "Save(S)",0
	MenuFilePlay db "Play Music(P)",0
	MenuFileStop db "Stop Music(P)",0

	MENU_NEWGAMEM equ 1000					; Menu Parameters
	MENU_NEWGAMEH equ 1001
	MENU_SAVEGAME equ 1002
	MENU_PLAYMUSIC equ 1003
	MENU_STOPMUSIC equ 1004
	MENU_HELPINFO equ 1005
	MENU_ABOUTAUTHOR equ 1006

;===================Macros====================


ENDIF