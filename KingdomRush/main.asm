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

INCLUDE     struct.inc
INCLUDE     data.inc
INCLUDE     core.inc
INCLUDE     main.inc

;==================== DATA =======================
.data

;=================== CODE =========================
InitImages PROTO,
    hInst:DWORD

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
    mov     wndClass.hbrBackground, COLOR_WINDOW + 1
    mov     wndClass.lpfnWndProc, OFFSET WinProc
    mov     wndClass.lpszClassName, OFFSET classname
    mov     wndClass.style, CS_HREDRAW or CS_VREDRAW

    ; 注册窗口类
	INVOKE  RegisterClassEx, ADDR wndClass
    .IF eax == 0
      call  ErrorHandler
      jmp   Exit_Program
    .ENDIF

    ; 创建窗口（移至屏幕中央）
    INVOKE  GetSystemMetrics, SM_CXSCREEN
	mov     scrWidth, eax
	INVOKE  GetSystemMetrics, SM_CYSCREEN
	mov     scrHeight, eax
    mov     ebx, 2
	mov     edx, 0
	mov     eax, scrWidth
	sub     eax, window_w
	div     ebx
	mov     window_x, eax
	mov     eax, scrHeight
	sub     eax, window_h
	div     ebx
	mov     window_y, eax

    INVOKE  CreateWindowEx, 
            0, 
            OFFSET classname,
            OFFSET windowname, 
            WS_OVERLAPPED or WS_SYSMENU or WS_MINIMIZEBOX ,
            window_x, 
            window_y, 
            window_w,
            window_h, 
            NULL, 
            NULL, 
            hInstance, 
            NULL
    mov     hMainWnd,eax

    .IF eax == 0
      call  ErrorHandler
      jmp   Exit_Program
    .ENDIF

	; 加载图片
	INVOKE  InitImages, hInstance 

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
    LOCAL   cursorPosition: POINTS
	LOCAL	ps: PAINTSTRUCT

    mov      eax, localMsg

    .IF eax == WM_TIMER
      INVOKE    TimerProc, hWnd
      jmp    	WinProcExit
    .ELSEIF eax == WM_PAINT         ; 绘图
	  INVOKE    BeginPaint, hWnd, ADDR ps
	  mov       hDC, eax
	  INVOKE    PaintProc, hWnd
	  INVOKE    EndPaint, hWnd, ADDR ps
	  jmp       WinProcExit
    .ELSEIF eax == WM_LBUTTONDOWN   ; 鼠标事件
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
      INVOKE    LoadGameInfo
      INVOKE    SetTimer, hWnd, TIMER_ID, TIMER_INTERVAL, NULL
      jmp    	WinProcExit
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
    INVOKE UpdateTimer
    INVOKE UpdateEnemies
    INVOKE InvalidateRect, hWnd, NULL, FALSE
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
InitImages PROC,
    hInst:DWORD
;
; LoadImage of game. If more levels are designed, considering
; input the level number.
; Receives: handler
; Returns:  nothing
;---------------------------------------------------------
    LOCAL   bm: BITMAP

    mov     ecx, mapNum
    mov     ebx, OFFSET mapHandler
    mov     edx, IDB_MAP
LoadMap:
    push    ecx
    push    edx
	INVOKE  LoadBitmap, hInst, edx
    mov     (BitmapInfo PTR [ebx]).bHandler, eax
    INVOKE  GetObject, (BitmapInfo PTR [ebx]).bHandler, SIZEOF BITMAP, ADDR bm
    mov     eax, bm.bmWidth
    mov     (BitmapInfo PTR [ebx]).bWidth, eax
    mov     eax, bm.bmHeight
    mov     (BitmapInfo PTR [ebx]).bHeight, eax

    ; Continue
    ;mov     eax, edx
    ;sub     eax, IDB_MAP
    ;inc     eax
    ;mov     ecx, BlankIndex[eax * TYPE DWORD]
    ;dec     eax
    ;sub     ecx, BlankIndex[eax * TYPE DWORD]
    ;mov     (BitmapInfo PTR [ebx]).BlankNum, ecx
    ;mov     edi, OFFSET BlankPosition
    ; Continue

    pop     edx
    pop     ecx
    add     ebx, TYPE BitmapInfo
    add     edx, 1
    loop    LoadMap

    mov     ecx, towerNum
    mov     ebx, OFFSET towerHandler
    mov     edx, IDB_TOWER
LoadTower:
    push    ecx
    push    edx
    INVOKE  LoadBitmap, hInst, edx
    mov     (BitmapInfo PTR [ebx]).bHandler, eax
    INVOKE  GetObject, (BitmapInfo PTR [ebx]).bHandler, SIZEOF BITMAP, ADDR bm
    mov     eax, bm.bmWidth
    mov     (BitmapInfo PTR [ebx]).bWidth, eax
    mov     eax, bm.bmHeight
    mov     (BitmapInfo PTR [ebx]).bHeight, eax
    pop     edx
    pop     ecx
    add     ebx, TYPE BitmapInfo
    add     edx, 1
    loop    LoadTower

    mov     ecx, signNum
    mov     ebx, OFFSET signHandler
    mov     edx, IDB_SIGN
LoadSign:
    push    ecx
    push    edx
    INVOKE  LoadBitmap, hInst, edx
    mov     (BitmapInfo PTR [ebx]).bHandler, eax
    INVOKE  GetObject, (BitmapInfo PTR [ebx]).bHandler, SIZEOF BITMAP, ADDR bm
    mov     eax, bm.bmWidth
    mov     (BitmapInfo PTR [ebx]).bWidth, eax
    mov     eax, bm.bmHeight
    mov     (BitmapInfo PTR [ebx]).bHeight, eax
    pop     edx
    pop     ecx
    add     ebx, TYPE BitmapInfo
    add     edx, 1
    loop    LoadSign
	
    mov     ecx, monsterNum
    mov     ebx, OFFSET monsterHandler
    mov     edx, IDB_MONSTER1
LoadMonster:
    push    ecx
	
	mov 	ecx, 5
LoadMonster0:
	push 	ecx
    push    edx
    INVOKE  LoadBitmap, hInst, edx
    mov     (BitmapInfo PTR [ebx]).bHandler, eax
    INVOKE  GetObject, (BitmapInfo PTR [ebx]).bHandler, SIZEOF BITMAP, ADDR bm
    mov     eax, bm.bmWidth
    mov     (BitmapInfo PTR [ebx]).bWidth, eax
    mov     eax, bm.bmHeight
    mov     (BitmapInfo PTR [ebx]).bHeight, eax
    pop     edx
	add 	ebx, TYPE BitmapInfo
	add 	edx, 1

    push    edx
    INVOKE  LoadBitmap, hInst, edx
    mov     (BitmapInfo PTR [ebx]).bHandler, eax
    INVOKE  GetObject, (BitmapInfo PTR [ebx]).bHandler, SIZEOF BITMAP, ADDR bm
    mov     eax, bm.bmWidth
    mov     (BitmapInfo PTR [ebx]).bWidth, eax
    mov     eax, bm.bmHeight
    mov     (BitmapInfo PTR [ebx]).bHeight, eax
    pop     edx
    pop     ecx
	add 	ebx, TYPE BitmapInfo
	add 	edx, 1
	loop 	LoadMonster0

    pop     ecx
    loop    LoadMonster

	ret
InitImages ENDP

;-----------------------------------------------------------------------
PaintTowers PROC
;-----------------------------------------------------------------------

    ; 画出所有空地
	INVOKE  SelectObject, imgDC, towerHandler[0].bHandler
    
    mov     ecx, Game.Tower_Num
    mov     ebx, OFFSET Game.TowerArray
DrawBlank:
	push    ecx
	INVOKE  TransparentBlt, 
            memDC, (Tower PTR [ebx]).Pos.x, (Tower PTR [ebx]).Pos.y,
            towerHandler[0].bWidth, towerHandler[0].bHeight,
            imgDC, 0, 0, 
            towerHandler[0].bWidth, towerHandler[0].bHeight,
            tcolor
	add     ebx, TYPE Tower
	pop     ecx
	loop    DrawBlank
	
	; 画出存在的塔

    ret
PaintTowers ENDP

;---------------------------------------------------------
PaintMonsters PROC
;
; LoadImage of game. If more levels are designed, considering
; input the level number.
; Receives: handler
; Returns:  nothing
;---------------------------------------------------------
	mov     eax, OFFSET Game.pEnemyArray
    mov     ebx, [eax]
    mov     ecx, Game.Enemy_Num
    cmp     ecx, 0
    je      PaintMonstersExit

DrawMonsters:
    push    ecx
    mov     edx, OFFSET monsterHandler
    mov     eax, (Enemy PTR [ebx]).Enemy_Type
    .WHILE  eax > 0
      add   edx, TYPE MonsterBitmapInfo
      dec   eax
    .ENDW
    mov     eax, (Enemy PTR [ebx]).Current_Dir
    .WHILE  eax > 0
      add   edx, TYPE BitmapInfo
      add   edx, TYPE BitmapInfo
      dec   eax
    .ENDW
    mov     eax, (Enemy PTR [ebx]).Gesture
    .IF     eax > 0
      add   edx, TYPE BitmapInfo
    .ENDIF
    push    edx
    INVOKE  SelectObject, imgDC, (BitmapInfo PTR [edx]).bHandler
    pop     edx
	INVOKE	TransparentBlt, 
			memDC, (Enemy PTR [ebx]).Current_Pos.x, (Enemy PTR [ebx]).Current_Pos.y,
            (BitmapInfo PTR [edx]).bWidth, (BitmapInfo PTR [edx]).bHeight, 
			imgDC, 0, 0,
            (BitmapInfo PTR [edx]).bWidth, (BitmapInfo PTR [edx]).bHeight, 
			tcolor
    add     ebx, TYPE Enemy
    pop     ecx
    loop    DrawMonsters
	
PaintMonstersExit:
    ret
PaintMonsters ENDP

;-----------------------------------------------------------------------
PaintProc PROC,
	hWnd:DWORD
;
; Painting  Function
; Receives: Windows handler
; Returns:  nothing
;-----------------------------------------------------------------------
	LOCAL 	hBitmap: DWORD
	LOCAL 	hOld: DWORD

    INVOKE 	CreateCompatibleDC, hDC
    mov 	memDC, eax
	INVOKE 	CreateCompatibleDC, hDC
    mov 	imgDC, eax

	INVOKE 	CreateCompatibleBitmap, hDC, window_w, window_h
	mov 	hBitmap, eax
    INVOKE 	SelectObject, memDC, hBitmap
    mov 	hOld, eax

	; 画地图
	INVOKE 	SelectObject, imgDC, mapHandler[0].bHandler
	INVOKE 	StretchBlt, 
			memDC, 0, 0, window_w, window_h, 
			imgDC, 0, 0, mapHandler[0].bWidth, mapHandler[0].bHeight, 
			SRCCOPY

	; 画空地，塔
    INVOKE  PaintTowers

	; 画兵

	; 画小怪
	INVOKE 	PaintMonsters

	; 画子弹

	
	INVOKE 	BitBlt, hDC, 0, 0, window_w, window_h, memDC, 0, 0, SRCCOPY 
    INVOKE 	DeleteDC, memDC
	INVOKE 	DeleteDC, imgDC
	INVOKE 	DeleteObject, hBitmap

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
