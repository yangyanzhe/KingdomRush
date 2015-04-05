TITLE Windows Application                   (WinApp.asm)

.386      
.model flat,stdcall      
option casemap:none

INCLUDE     windows.inc
INCLUDE     gdi32.inc
INCLUDE     user32.inc
INCLUDE		msimg32.inc
INCLUDE     kernel32.inc

INCLUDELIB  gdi32.lib
INCLUDELIB  kernel32.lib
INCLUDELIB  user32.lib
INCLUDELIB  msimg32.lib

INCLUDE     gui.inc

white  = 1111b

;==================== DATA =======================
.data

hInstance       dd ?
hMainWnd        dd ?
hIcon           dd ?
classname       db "Game Application", 0
windowname      db "Kingdom Rush", 0

IDI_ICON        EQU 101
IDB_ARROW       EQU 103
IDB_ARROW_SIGN	EQU 104
IDB_CIRCLE		EQU 105
IDB_MAGIC		EQU 106
IDB_MAGIC_SIGN	EQU 107
IDB_SODIER		EQU 109
IDB_TURRET		EQU 111
IDB_TURRET_SIGN	EQU 112
IDB_SODIER_SIGN EQU 113
IDB_MAPONE		EQU 115
IDB_BLANK		EQU 116

;=================== CODE =========================
TimerProc PROTO,
    hWnd: DWORD

LMouseProc PROTO,
	hWnd: DWORD,
	cursorPosition: POINTS
	
PaintProc PROTO,
    hWnd: DWORD

.code

;-----------------------------------------------------
WinMain PROC
;-----------------------------------------------------
    LOCAL   wndClass: WNDCLASSEX
    LOCAL   msg: MSG
    LOCAL   scrWidth: DWORD
    LOCAL   scrHeight: DWORD

    ; 获取句柄
    INVOKE  GetModuleHandle, NULL
    mov     hInstance, eax
    mov     wndClass.hInstance, eax
    INVOKE  RtlZeroMemory, ADDR wndClass, SIZEOF wndClass

    ; 加载程序的光标和图标
    INVOKE  LoadIcon, hInstance, IDI_ICON
    mov     hIcon, eax
    mov     wndClass.hIcon, eax
    mov     wndClass.hIconSm, eax
    INVOKE  LoadCursor, hInstance, IDC_ARROW
    mov     wndClass.hCursor, eax

	; 初始化窗口
    mov     wndClass.cbSize, SIZEOF WNDCLASSEX
    mov     wndClass.style, CS_HREDRAW or CS_VREDRAW or CS_BYTEALIGNWINDOW
	mov     wndClass.lpfnWndProc, OFFSET WinProc
	mov		wndClass.cbClsExtra,0      
    mov		wndClass.cbWndExtra,0  

	; 创建画刷
	INVOKE	CreateSolidBrush, white
	mov		bgBrush, eax
    mov		wndClass.hbrBackground, eax 
	mov     wndClass.lpszClassName, OFFSET classname

    ; 创建窗口（移至屏幕中央）
    INVOKE  GetSystemMetrics, SM_CXSCREEN
	mov     scrWidth, eax
	INVOKE  GetSystemMetrics, SM_CYSCREEN
	mov     scrHeight, eax
    mov     ebx, 2
	mov     edx, 0
	mov     eax, scrWidth
	sub     eax, window.w
	div     ebx
	mov     window.x, eax
	mov     eax, scrHeight
	sub     eax, window.h
	div     ebx
	mov     window.y, eax

	; 注册窗口类
	INVOKE  RegisterClassEx, ADDR wndClass
    .IF eax == 0
      call  ErrorHandler
      jmp   Exit_Program
    .ENDIF

    INVOKE  CreateWindowEx, 
            0, 
            OFFSET classname,
            OFFSET windowname, 
            WS_OVERLAPPED or WS_SYSMENU or WS_MINIMIZEBOX ,
            window.x, 
            window.y, 
            window.w,
            window.h, 
            NULL, 
            NULL, 
            hInstance, 
            NULL
	
	; 保存窗口句柄
    mov     hMainWnd,eax

	; 加载图片
	INVOKE  InitiateImages, hInstance 

    ; 绘制窗口
    INVOKE  ShowWindow, hMainWnd, SW_SHOW
    INVOKE  UpdateWindow, hMainWnd

    ; 消息循环
Message_Loop:
    INVOKE  GetMessage, ADDR msg, NULL, NULL, NULL

    ; 退出WM_QUIT
    .IF eax == 0
      jmp   Exit_Program
    .ENDIF

    INVOKE  TranslateMessage, ADDR msg
    INVOKE  DispatchMessage, ADDR msg
    jmp     Message_Loop

Exit_Program:
    ret
WinMain ENDP

;-----------------------------------------------------
WinProc PROC,
    hWnd: DWORD, 
    localMsg: DWORD, 
    wParam: DWORD, 
    lParam: DWORD
;-----------------------------------------------------
    LOCAL   srcPosition: POINTS
    LOCAL   destPosition: POINTS
    LOCAL   cursorPosition: POINTS
	LOCAL	ps:PAINTSTRUCT
   
    mov      eax, localMsg

    .IF eax == WM_TIMER
      INVOKE    TimerProc, hWnd
      jmp    	WinProcExit
    .ELSEIF eax == WM_LBUTTONDOWN       ; 鼠标事件
      mov  	    ebx, lParam
      mov  	    cursorPosition.x, bx
	  shr  	    ebx, 16
	  mov  	    cursorPosition.y, bx
	  .IF wParam == MK_LBUTTON
		INVOKE 	LMouseProc, hWnd, cursorPosition
	  .ENDIF
      jmp    	WinProcExit
    .ELSEIF eax == WM_CLOSE         ; 关闭窗口事件
      INVOKE 	PostQuitMessage, 0
      jmp    	WinProcExit
    .ELSEIF eax == WM_CREATE        ; 创建窗口事件
      INVOKE 	SendMessage, hWnd, WM_SETICON, ICON_SMALL, hIcon
      INVOKE    SetTimer, hWnd, 1, 1000, NULL
      jmp    	WinProcExit
    .ELSEIF eax == WM_PAINT         ; 绘图
	  INVOKE BeginPaint, hWnd, ADDR ps
	  mov hDC, eax
	  INVOKE PaintProc, hWnd
	  INVOKE EndPaint, hWnd, ADDR ps
	  jmp WinProcExit
    .ELSE                           ; 其他事件
      INVOKE 	DefWindowProc, hWnd, localMsg, wParam, lParam
      jmp    	WinProcExit
    .ENDIF

WinProcExit:
    ret
WinProc ENDP

TimerProc PROC,
    hWnd: DWORD
    ; INVOKE MessageBox, hWnd, NULL, NULL, MB_OK
    ret
TimerProc ENDP

;-----------------------------------------------------------------------
LMouseProc PROC,
	hWnd: DWORD,
	cursorPosition: POINTS
;-----------------------------------------------------------------------
    ; INVOKE MessageBox, hWnd, NULL, NULL, MB_OK
    ret
LMouseProc ENDP


;---------------------------------------------------------
InitiateImages PROC hInst:DWORD
;
; LoadImage of game. If more levels are designed, considering
; input the level number.
; Receives: handler
; Returns:  nothing
;---------------------------------------------------------
	push eax
	push esi

	INVOKE LoadBitmap, hInst, IDB_MAPONE
	mov map1, eax

	mov esi, OFFSET towerHandler
	INVOKE LoadBitmap, hInst, IDB_BLANK
	mov [esi], eax
	INVOKE LoadBitmap, hInst, IDB_ARROW
	mov [esi+4], eax
	INVOKE LoadBitmap, hInst, IDB_MAGIC
	mov [esi+8], eax
	INVOKE LoadBitmap, hInst, IDB_SODIER
	mov [esi+12], eax
	INVOKE LoadBitmap, hInst, IDB_TURRET
	mov [esi+16], eax

	mov esi, OFFSET towerSignHandler
	INVOKE LoadBitmap, hInst, IDB_CIRCLE
	mov [esi], eax
	INVOKE LoadBitmap, hInst, IDB_ARROW_SIGN
	mov [esi+4], eax
	INVOKE LoadBitmap, hInst, IDB_MAGIC_SIGN
	mov [esi+8], eax
	INVOKE LoadBitmap, hInst, IDB_SODIER_SIGN
	mov [esi+12], eax
	INVOKE LoadBitmap, hInst, IDB_TURRET_SIGN
	mov [esi+16], eax

	pop esi
	pop eax
	ret
InitiateImages ENDP

;-----------------------------------------------------------------------
PaintProc PROC hWin:DWORD
;
; Painting  Function
; Receives: Windows handler
; Returns:  nothing
;-----------------------------------------------------------------------
	LOCAL hOld: DWORD
	push eax
	push ebx
	push esi

	INVOKE CreateCompatibleBitmap, hDC, client.w, client.h
	mov hBitmap, eax
    INVOKE CreateCompatibleDC, hDC
    mov memDC, eax
	INVOKE CreateCompatibleDC, hDC
    mov imgDC, eax

    INVOKE SelectObject, memDC, hBitmap
    mov hOld, eax
	
	INVOKE FillRect, memDC, ADDR rect, bgBrush
	INVOKE SelectObject,hDC,hOld

	; 画地图
	INVOKE SelectObject, imgDC, map1
	INVOKE StretchBlt, memDC, client.x, client.y, client.w, client.h, imgDC, bgstart.x, bgstart.y, bgstart.w, bgstart.h, SRCCOPY

	; 画空地
	mov	   esi, OFFSET towerHandler
	
	INVOKE SelectObject, imgDC, [esi]
	mov    esi, OFFSET towerLocation
	mov    ecx, locationNum
DrawBlank:
	push   ecx
	mov    eax, (Coord PTR [esi]).x
	mov	   ebx, (Coord PTR [esi]).y
	sub	   ebx, blankSize.y
	INVOKE TransparentBlt, memDC, eax, ebx, blankSize.x, blankSize.y, imgDC, 0 , 0, blankSize.x, blankSize.y, tcolor
	add    esi, sizeof Coord
	pop    ecx
	loop   DrawBlank
	
	; 画塔

	; 画兵

	; 画小怪

	; 画子弹

	INVOKE BitBlt, hDC, 0, 0, window.w, window.h, memDC, 0, 0, SRCCOPY 
    INVOKE DeleteDC,memDC
	INVOKE DeleteDC,imgDC
	INVOKE DeleteObject, hBitmap

	pop esi
	pop ebx
	pop eax

	ret
PaintProc ENDP

;---------------------------------------------------
ErrorHandler PROC
; Display the appropriate system error message.
;---------------------------------------------------
.data

pErrorMsg   dd ?      ; ptr to error message
messageID   dd ?
ErrorTitle  db "Error", 0

.code

    INVOKE  GetLastError ; Returns message ID in EAX
    mov     messageID, eax

    ; Get the corresponding message string.
    INVOKE  FormatMessage, FORMAT_MESSAGE_ALLOCATE_BUFFER + \
            FORMAT_MESSAGE_FROM_SYSTEM,NULL,messageID,NULL,
            ADDR pErrorMsg,NULL,NULL

    ; Display the error message.
    INVOKE  MessageBox,NULL, pErrorMsg, ADDR ErrorTitle,
            MB_ICONERROR+MB_OK

    ; Free the error message string.
    INVOKE  LocalFree, pErrorMsg
    ret
ErrorHandler ENDP

start:
    INVOKE  WinMain
    INVOKE  ExitProcess, NULL

END start
