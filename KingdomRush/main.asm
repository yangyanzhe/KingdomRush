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

INCLUDE     core.inc
INCLUDE     main.inc

;==================== DATA =======================
.data
Level       dd 0

;=================== CODE =========================
InitImages PROTO,
    hInst:DWORD

InitMapInfo PROTO

TimerProc PROTO,
    hWnd: DWORD

LMouseProc PROTO,
	hWnd: DWORD,
	cursorPosition: Coord
	
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

    ; 加载图片
	INVOKE  InitImages, hInstance 

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
    LOCAL   cursorPosition: Coord
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
      movzx     edx, bx
      mov     	cursorPosition.x, edx
	  shr  	    ebx, 16
      movzx     edx, bx
	  mov     	cursorPosition.y, edx
	  .IF wParam == MK_LBUTTON
		INVOKE 	LMouseProc, hWnd, cursorPosition
	  .ENDIF
      jmp    	WinProcExit
    .ELSEIF eax == WM_CLOSE         ; 关闭窗口事件
      INVOKE 	PostQuitMessage, 0
      jmp    	WinProcExit
    .ELSEIF eax == WM_CREATE        ; 创建窗口事件
      INVOKE 	SendMessage, hWnd, WM_SETICON, ICON_SMALL, hIcon
      INVOKE    InitMapInfo
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

    ; 载入地图图片
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
    pop     edx
    pop     ecx
    add     ebx, TYPE BitmapInfo
    add     edx, 1
    loop    LoadMap

    ; 载入空地位置
    mov     ecx, mapNum
    mov     ebx, OFFSET blankSet
    mov     edx, OFFSET blankIndex
    mov     esi, OFFSET blankPosition
LoadBlankPosition:
    push    ecx
    mov     eax, [edx + TYPE DWORD]
    sub     eax, [edx]
    mov     (PositionSet PTR [ebx]).number, eax

    mov     ecx, eax
    mov     edi, ebx
    add     edi, TYPE DWORD
LoadBlankPosition0:
    mov     eax, (Coord PTR [esi]).x
    mov     (Coord PTR [edi]).x, eax
    mov     eax, (Coord PTR [esi]).y
    mov     (Coord PTR [edi]).y, eax
    add     esi, TYPE Coord
    add     edi, TYPE Coord
    loop    LoadBlankPosition0

    add     ebx, TYPE PositionSet
    add     edx, TYPE DWORD
    pop     ecx
    loop    LoadBlankPosition

    ; 载入塔的图片
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

    ; 载入塔的标志的图片
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
	
    ; 载入怪物图片
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

    ; 载入子弹图片
    mov     ecx, attackNum
    mov     ebx, OFFSET attackHandler
    mov     edx, IDB_ATTACK
LoadAttack:
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
    loop    LoadAttack

	ret
InitImages ENDP

;-----------------------------------------------------------------------
InitMapInfo PROC
;-----------------------------------------------------------------------
    ; 初始化所有塔
    mov     ecx, blankSet[0].number
    mov     Game.Tower_Num, ecx
    mov     ebx, OFFSET blankSet[0].position
    mov     edx, OFFSET Game.TowerArray
InitTower:  
    mov     (Tower PTR [edx]).Tower_Type, 0     ;塔的初始类型为0（空地）
    mov     (Tower PTR [edx]).Range, 100        ;塔的攻击范围
    mov     eax, (Coord PTR [ebx]).x
    mov     (Tower PTR [edx]).Pos.x, eax
    mov     eax, (Coord PTR [ebx]).y
    mov     (Tower PTR [edx]).Pos.y, eax
    add     ebx, TYPE Coord
    add     edx, TYPE Tower
    loop    InitTower

    ret
InitMapInfo ENDP

;-----------------------------------------------------------------------
TimerProc PROC,
    hWnd: DWORD
;-----------------------------------------------------------------------
    ; INVOKE MessageBox, hWnd, NULL, NULL, MB_OK
    INVOKE UpdateTimer
    INVOKE UpdateEnemies
    INVOKE InvalidateRect, hWnd, NULL, FALSE
    ret
TimerProc ENDP

;-----------------------------------------------------------------------
LMouseProc PROC,
	hWnd: DWORD,
	cursorPosition: Coord
;-----------------------------------------------------------------------
    ; INVOKE MessageBox, hWnd, NULL, NULL, MB_OK

    mov     eax, Game.IsClicked
    .IF eax == 1
      mov   Game.IsClicked, 0
    .ELSE
      mov   ecx, Game.Tower_Num
      mov   ebx, OFFSET Game.TowerArray
      mov   esi, 0
CheckClicked:
      mov   edx, OFFSET towerHandler
      mov   eax, (Tower PTR [ebx]).Tower_Type
      .WHILE eax > 0
        add edx, TYPE BitmapInfo
        dec eax
      .ENDW
      ; 判断是否点击在空地/塔的范围内
      mov   eax, (Tower PTR [ebx]).Pos.x
      cmp   cursorPosition.x, eax
      jb    CheckClicked0
      add   eax, (BitmapInfo PTR [edx]).bWidth
      cmp   cursorPosition.x, eax
      ja    CheckClicked0
      mov   eax, (Tower PTR [ebx]).Pos.y
      cmp   cursorPosition.y, eax
      ja    CheckClicked0
      sub   eax, (BitmapInfo PTR [edx]).bHeight
      cmp   cursorPosition.y, eax
      jb    CheckClicked0

      mov   Game.IsClicked, 1
      mov   Game.ClickedIndex, esi
      jmp   LMouseProcExit
CheckClicked0:
      add   ebx, TYPE Tower
      add   esi, 1
      loop  CheckClicked
    .ENDIF

LMouseProcExit:
    ret
LMouseProc ENDP

;-----------------------------------------------------------------------
PaintTowers PROC
;-----------------------------------------------------------------------
    ; 画出所有空地
	INVOKE  SelectObject, imgDC, towerHandler[0].bHandler
    mov     ecx, blankSet[0].number
    mov     ebx, OFFSET blankSet[0].position
DrawBlank:
	push    ecx
    mov     eax, (Coord PTR [ebx]).y
    sub     eax, towerHandler[0].bHeight
	INVOKE  TransparentBlt, 
            memDC, (Coord PTR [ebx]).x, eax,
            towerHandler[0].bWidth, towerHandler[0].bHeight,
            imgDC, 0, 0, 
            towerHandler[0].bWidth, towerHandler[0].bHeight,
            tcolor
	add     ebx, TYPE Coord
	pop     ecx
	loop    DrawBlank
	
	; 画出存在的塔
    mov     ecx, Game.Tower_Num
    mov     ebx, OFFSET Game.TowerArray
    cmp     ecx, 0
    je      PaintTowersExit
DrawTowers:
    push    ecx
    mov     eax, (Tower PTR [ebx]).Tower_Type
    cmp     eax, 0
    je      DrawTowers0
    mov     edx, OFFSET towerHandler
    .WHILE  eax > 0
      add   edx, TYPE BitmapInfo
      dec   eax
    .ENDW
    push    edx
    INVOKE  SelectObject, imgDC, (BitmapInfo PTR [edx]).bHandler
    pop     edx
    mov     eax, (Tower PTR [ebx]).Pos.y
    sub     eax, (BitmapInfo PTR [edx]).bHeight
	INVOKE  TransparentBlt, 
            memDC, (Tower PTR [ebx]).Pos.x, eax,
            (BitmapInfo PTR [edx]).bWidth, (BitmapInfo PTR [edx]).bHeight,
            imgDC, 0, 0, 
            (BitmapInfo PTR [edx]).bWidth, (BitmapInfo PTR [edx]).bHeight,
            tcolor
DrawTowers0:
	add     ebx, TYPE Tower        
    pop     ecx
    loop    DrawTowers

PaintTowersExit:
    ret
PaintTowers ENDP

;---------------------------------------------------------
PaintMonsters PROC
;
;---------------------------------------------------------
	mov     eax, OFFSET Game.pEnemyArray
    mov     esi, eax
    mov     ecx, Game.Enemy_Num
    cmp     ecx, 0
    je      PaintMonstersExit

DrawMonsters:
    push    ecx
    mov     edx, OFFSET monsterHandler
    mov     ebx, [esi]
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
    add     esi, TYPE DWORD
    pop     ecx
    loop    DrawMonsters
	
PaintMonstersExit:
    ret
PaintMonsters ENDP

;---------------------------------------------------------
PaintSigns PROC uses eax esi ebx ecx
;	
;	Functions: 画出选择塔的标识
;	Receives:  
;---------------------------------------------------------
	LOCAL   oriX: DWORD         ; 画标志的原点
    LOCAL   oriY: DWORD         ; 画标志的原点
    LOCAL   x: DWORD
    LOCAL   y: DWORD

    cmp     Game.IsClicked, 0
    je      PaintSignsExit

    mov     ebx, OFFSET Game.TowerArray
    mov     eax, Game.ClickedIndex
    .WHILE  eax > 0
      add   ebx, TYPE Tower
      dec   eax
    .ENDW

    ; 调整原点位置
    mov     eax, (Tower PTR [ebx]).Pos.x
    mov     oriX, eax
    mov     eax, (Tower PTR [ebx]).Pos.y
    mov     oriY, eax
    mov     eax, towerHandler[0].bWidth
    shr     eax, 1
    add     oriX, eax
    mov     eax, towerHandler[0].bHeight
    shr     eax, 1
    sub     oriY, eax
    mov     eax, signHandler[0].bWidth
    shr     eax, 1
    sub     oriX, eax
	mov     eax, signHandler[0].bHeight
    shr     eax, 1
    sub     oriY, eax

	mov		eax, (Tower PTR [ebx]).Tower_Type
	.IF		eax == 0
	    mov		ecx, signNum
	.ELSE
		mov		ecx, 1
	.ENDIF
	
	mov     ebx, OFFSET signHandler
	mov     edx, OFFSET signPosition
DrawSigns:
    push    ecx
    push    edx
    mov     eax, oriX
    add     eax, (Coord PTR [edx]).x
    mov     x, eax
    mov     eax, oriY
    add     eax, (Coord PTR [edx]).y
    mov     y, eax
    INVOKE  SelectObject, imgDC, (BitmapInfo PTR [ebx]).bHandler
    INVOKE	TransparentBlt, 
			memDC, x, y,
            (BitmapInfo PTR [ebx]).bWidth, (BitmapInfo PTR [ebx]).bHeight, 
			imgDC, 0, 0,
            (BitmapInfo PTR [ebx]).bWidth, (BitmapInfo PTR [ebx]).bHeight, 
			tcolor
    pop     edx
    add     ebx, TYPE BitmapInfo
    add     edx, TYPE Coord
    pop     ecx
    loop    DrawSigns

PaintSignsExit:
    ret
PaintSigns ENDP

;--------------------------------------------------
RotateDC Proc,
	hDC:DWORD,		; HDC
	angle:WORD,		; rotate angle
	centerPt:Coord  ; center point
;
;	return eax = nGraphicsMode
;--------------------------------------------------
	Local nGraphicsMode:WORD
	Local fangle:DWORD
	Local xform:XFORM

    INVOKE SetGraphicsMode, hDc, GM_ADVANCED;  
	cmp angle, 0
	je	RotateExit

	mov edx, 0
	mov eax, angle
	mov ecx, 180
	div ecx
	mov ecx, 3.1415926
	mul ecx
	mov fangle, eax

    ;double fangle = (double)iAngle / 180. * 3.1415926;  
    ;xform.eM11 = (float)cos(fangle);  
    ;xform.eM12 = (float)sin(fangle);  
    ;xform.eM21 = (float)-sin(fangle);  
    ;xform.eM22 = (float)cos(fangle);  
    ;xform.eDx = (float)(centerPt.x - cos(fangle)*centerPt.x + sin(fangle)*centerPt.y);  
    ;xform.eDy = (float)(centerPt.y - cos(fangle)*centerPt.y - sin(fangle)*centerPt.x);  
    ;SetWorldTransform(hDc, &xform);  

	movzx eax, nGraphicsMode
RotateExit:
	ret

RotateDC ENDP


;---------------------------------------------------
PaintBombAttack PROC
;
;---------------------------------------------------
	LOCAL   oriX: DWORD         ; 画标志的原点
    LOCAL   oriY: DWORD         ; 画标志的原点
    LOCAL   desX: DWORD
    LOCAL   desY: DWORD

	INVOKE  SelectObject, imgDC, (BitmapInfo PTR [ebx]).bHandler
	
    ;INVOKE  SelectObject, imgDC, (BitmapInfo PTR [ebx]).bHandler
    ;INVOKE	TransparentBlt, 
	;		memDC, x, y,
    ;        (BitmapInfo PTR [ebx]).bWidth, (BitmapInfo PTR [ebx]).bHeight, 
	;		imgDC, 0, 0,
    ;        (BitmapInfo PTR [ebx]).bWidth, (BitmapInfo PTR [ebx]).bHeight, 
	;		tcolor
    ;pop     edx
    ;add     ebx, TYPE BitmapInfo
    ;add     edx, TYPE Coord
    ;pop     ecx
    ;loop    DrawSigns

	ret
PaintBombAttack ENDP

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
	;INVOKE  PaintBombAttack

	; 画建塔提示圆圈
	INVOKE	PaintSigns
	
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
