TITLE Windows Application

.386      
.model flat,stdcall      
option casemap:none

INCLUDE     windows.inc
INCLUDE     gdi32.inc
INCLUDE     user32.inc
INCLUDE		msimg32.inc
INCLUDE     kernel32.inc
INCLUDE		winmm.inc

INCLUDELIB  gdi32.lib
INCLUDELIB  kernel32.lib
INCLUDELIB  user32.lib
INCLUDELIB  msimg32.lib
INCLUDELIB  winmm.lib

INCLUDE     core.inc
INCLUDE     main.inc

;==================== DATA =======================
.data
Level       dd 0

;=================== CODE =========================
InitImages PROTO,
    hInst:DWORD

InitMapInfo PROTO

TimerProc_Prepared PROTO,
    hWnd: DWORD

TimerProc_Started PROTO,
    hWnd: DWORD

TimerProc_Ended PROTO,
    hWnd: DWORD

MouseMoveProc_Prepared PROTO,
	hWnd: DWORD,
    cursorPosition: Coord

MouseMoveProc_Started PROTO,
	hWnd: DWORD,
	cursorPosition: Coord

LMouseProc_Prepared PROTO,
	hWnd: DWORD,
	cursorPosition: Coord

LMouseProc_Started PROTO,
	hWnd: DWORD,
	cursorPosition: Coord
	
LMouseProc_Ended PROTO,
	hWnd: DWORD,
	cursorPosition: Coord

PaintProc PROTO,
	hWnd: DWORD

PlayMp3File PROTO,
	hWnd:DWORD,
	NameOfFile:DWORD

ContinuePlayMp3File PROTO,
	hWnd:DWORD

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

	; 创建文字
	INVOKE CreateFont, 16,
                    0,
                    0,
                    0,
                    FW_EXTRABOLD,
                    FALSE,
                    FALSE,
                    FALSE,
                    DEFAULT_CHARSET,
                    OUT_TT_PRECIS,
                    CLIP_DEFAULT_PRECIS,
                    CLEARTYPE_QUALITY,
                    DEFAULT_PITCH or FF_DONTCARE,
                    OFFSET FontName
    mov textFont, eax

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
      .IF Game.State == 0
        INVOKE 	TimerProc_Prepared, hWnd
      .ELSEIF Game.State == 1
		INVOKE 	TimerProc_Started, hWnd
      .ELSE
        INVOKE  TimerProc_Ended, hWnd
      .ENDIF
      INVOKE    InvalidateRect, hWnd, NULL, FALSE
      jmp    	WinProcExit
    .ELSEIF eax == WM_PAINT         ; 绘图
	  INVOKE    BeginPaint, hWnd, ADDR ps
	  mov       hDC, eax
	  INVOKE    PaintProc, hWnd
	  INVOKE    EndPaint, hWnd, ADDR ps

	  ;播放音乐
	  .IF PlayStart == 0 && Game.State == 0
        mov     PlayStart, 1 
		INVOKE  PlayMp3File, hWnd, ADDR StartFileName 
	  .ENDIF

	  .IF PlayFlag == 0 && Game.State > 0
		invoke mciSendCommand,Mp3DeviceID,MCI_CLOSE,0,0
		mov		PlayStart,0
        mov     PlayFlag, 1 
		INVOKE  PlayMp3File, hWnd, ADDR MusicFileName 
	  .ENDIF

	  .IF PlayFlag == 1
		mov		eax, countTime
		add		eax, 1
		mov		countTime, eax
		.IF		countTime == 600
			INVOKE mciSendCommand,Mp3DeviceID,MCI_CLOSE,0,0
			INVOKE  PlayMp3File, hWnd, ADDR MusicFileName 
			;INVOKE ContinuePlayMp3File, hWnd
			mov	eax, 0
			mov countTime, eax
		.ENDIF
	  .ENDIF

	  jmp       WinProcExit
    .ELSEIF eax == WM_LBUTTONDOWN   ; 鼠标点击事件
	  INVOKE	PlaySound, OFFSET ClickFileName, 0, SND_ASYNC
      mov  	    ebx, lParam
      movzx     edx, bx
      mov     	cursorPosition.x, edx
	  shr  	    ebx, 16
      movzx     edx, bx
	  mov     	cursorPosition.y, edx
	  .IF wParam == MK_LBUTTON
        .IF Game.State == 0
          INVOKE 	LMouseProc_Prepared, hWnd, cursorPosition
        .ELSEIF Game.State == 1
		  INVOKE 	LMouseProc_Started, hWnd, cursorPosition
        .ELSE
          INVOKE    LMouseProc_Ended, hWnd, cursorPosition
        .ENDIF
	  .ENDIF
      jmp    	WinProcExit
    .ELSEIF eax == MM_MCINOTIFY     ; 音乐循环播放
      ;INVOKE    ContinuePlayMp3File, hWnd
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
LoadTypeImages PROC,
    hInst: DWORD,
    typeNum: DWORD,
    typeHandler: DWORD,
    typeID: DWORD
;
; As the load procedure are repeatable, remove the repeatable code here
;---------------------------------------------------------
    LOCAL   bm: BITMAP

    mov     ecx, typeNum
    mov     ebx, typeHandler
    mov     edx, typeID
 LoadImages:
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
    loop    LoadImages

    ret
LoadTypeImages ENDP

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

    ; 载入开始图片和按钮
    INVOKE  LoadTypeImages, hInst, instructionNum, OFFSET instructionHandler, IDB_INSTRUCTION
    INVOKE  LoadTypeImages, hInst, buttonNum, OFFSET buttonHandler, IDB_BUTTON

    ; 载入结束图片
    INVOKE  LoadTypeImages, hInst, endNum, OFFSET endHandler, IDB_END

    ; 载入地图图片
    INVOKE  LoadTypeImages, hInst, mapNum, OFFSET mapHandler, IDB_MAP

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
    INVOKE  LoadTypeImages, hInst, towerNum, OFFSET towerHandler, IDB_TOWER

    ; 载入塔的标志的图片
    INVOKE  LoadTypeImages, hInst, signNum, OFFSET signHandler, IDB_SIGN
	
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

	; 载入子弹
    INVOKE  LoadTypeImages, hInst, bulletNum, OFFSET bulletHandler, IDB_BULLET

	; 载入动画的图片
    INVOKE  LoadTypeImages, hInst, animateNum, OFFSET animateHandler, IDB_ANIMATE

	ret
InitImages ENDP

;-----------------------------------------------------------------------
InitMapInfo PROC
;-----------------------------------------------------------------------
    ; 初始化所有塔
    mov     edx, OFFSET blankSet
    mov     eax, Level
    .WHILE eax > 0
      add   edx, TYPE PositionSet
      dec   eax
    .ENDW
    mov     ecx, (PositionSet PTR [edx]).number
    mov     Game.Tower_Num, ecx
    mov     ebx, edx
    add     ebx, TYPE DWORD
    mov     edx, OFFSET Game.TowerArray
InitTower:  
    mov     (Tower PTR [edx]).Tower_Type, 0     ;塔的初始类型为0（空地）
    mov     (Tower PTR [edx]).Range, 80         ;塔的攻击范围
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
TimerProc_Prepared PROC,
    hWnd: DWORD
;-----------------------------------------------------------------------
    LOCAL   p: POINT

    INVOKE  GetCursorPos, ADDR p
    INVOKE  ScreenToClient, hWnd, ADDR p
    mov     eax, Game.InstructionIndex
    .IF eax == 0
      mov   eax, START_BUTTON_POS.x
      cmp   eax, p.x
      jg    TimerStartExit
      add   eax, buttonHandler[0].bWidth
      cmp   eax, p.x
      jl    TimerStartExit
      mov   eax, START_BUTTON_POS.y
      cmp   eax, p.y
      jg    TimerStartExit
      add   eax, buttonHandler[0].bHeight
      cmp   eax, p.y
      jl    TimerStartExit

      mov   Game.ButtonIndex, 1
      ret

TimerStartExit:
      mov   Game.ButtonIndex, 0
      ret
    .ELSEIF eax == 1
      mov   eax, SKIP_BUTTON_POS1.x
      cmp   eax, p.x
      jg    TimerInstruction1Next
      add   eax, buttonHandler[TYPE BitmapInfo].bWidth
      cmp   eax, p.x
      jl    TimerInstruction1Next
      mov   eax, SKIP_BUTTON_POS1.y
      cmp   eax, p.y
      jg    TimerInstruction1Next
      add   eax, buttonHandler[TYPE BitmapInfo].bHeight
      cmp   eax, p.y
      jl    TimerInstruction1Next

      mov   Game.ButtonIndex, 1
      ret

TimerInstruction1Next:
      mov   eax, CONTINUE_BUTTON_POS1.x
      cmp   eax, p.x
      jg    TimerInstruction1Exit
      add   eax, buttonHandler[TYPE BitmapInfo * 2].bWidth
      cmp   eax, p.x
      jl    TimerInstruction1Exit
      mov   eax, CONTINUE_BUTTON_POS1.y
      cmp   eax, p.y
      jg    TimerInstruction1Exit
      add   eax, buttonHandler[TYPE BitmapInfo * 2].bHeight
      cmp   eax, p.y
      jl    TimerInstruction1Exit

      mov   Game.ButtonIndex, 2
      ret

TimerInstruction1Exit:
      mov   Game.ButtonIndex, 0
      ret
    .ELSEIF eax == 2
      mov   eax, SKIP_BUTTON_POS2.x
      cmp   eax, p.x
      jg    TimerInstruction2Next
      add   eax, buttonHandler[TYPE BitmapInfo].bWidth
      cmp   eax, p.x
      jl    TimerInstruction2Next
      mov   eax, SKIP_BUTTON_POS2.y
      cmp   eax, p.y
      jg    TimerInstruction2Next
      add   eax, buttonHandler[TYPE BitmapInfo].bHeight
      cmp   eax, p.y
      jl    TimerInstruction2Next

      mov   Game.ButtonIndex, 1
      ret

TimerInstruction2Next:
      mov   eax, CONTINUE_BUTTON_POS2.x
      cmp   eax, p.x
      jg    TimerInstruction2Exit
      add   eax, buttonHandler[TYPE BitmapInfo * 2].bWidth
      cmp   eax, p.x
      jl    TimerInstruction2Exit
      mov   eax, CONTINUE_BUTTON_POS2.y
      cmp   eax, p.y
      jg    TimerInstruction2Exit
      add   eax, buttonHandler[TYPE BitmapInfo * 2].bHeight
      cmp   eax, p.y
      jl    TimerInstruction2Exit

      mov   Game.ButtonIndex, 2
      ret

TimerInstruction2Exit:
      mov   Game.ButtonIndex, 0
      ret
    .ELSEIF eax == 3
      mov   eax, READY_BUTTON_POS.x
      cmp   eax, p.x
      jg    TimerInstruction3Exit
      add   eax, buttonHandler[TYPE BitmapInfo * 3].bWidth
      cmp   eax, p.x
      jl    TimerInstruction3Exit
      mov   eax, READY_BUTTON_POS.y
      cmp   eax, p.y
      jg    TimerInstruction3Exit
      add   eax, buttonHandler[TYPE BitmapInfo * 3].bHeight
      cmp   eax, p.y
      jl    TimerInstruction3Exit

      mov   Game.ButtonIndex, 1
      ret

TimerInstruction3Exit:
      mov   Game.ButtonIndex, 0
      ret
    .ENDIF
TimerProc_Prepared ENDP

;-----------------------------------------------------------------------
TimerProc_Started PROC,
    hWnd: DWORD
;-----------------------------------------------------------------------
    ; INVOKE MessageBox, hWnd, NULL, NULL, MB_OK
    INVOKE UpdateTimer
    INVOKE UpdateEnemies
    INVOKE UpdateTowers
    INVOKE UpdateBullets
    INVOKE UpdateAnimates
    INVOKE CheckWinGame
    INVOKE CheckLoseGame
    INVOKE InvalidateRect, hWnd, NULL, FALSE
    ret
TimerProc_Started ENDP

;-----------------------------------------------------------------------
TimerProc_Ended PROC,
    hWnd: DWORD
;-----------------------------------------------------------------------
    LOCAL   p: POINT

    INVOKE  GetCursorPos, ADDR p
    INVOKE  ScreenToClient, hWnd, ADDR p
    mov     eax, END_BUTTON_POS.x
    cmp     eax, p.x
    jg      TimerProc_EndedExit
    add     eax, buttonHandler[0].bWidth
    cmp     eax, p.x
    jl      TimerProc_EndedExit
    mov     eax, END_BUTTON_POS.y
    cmp     eax, p.y
    jg      TimerProc_EndedExit
    add     eax, buttonHandler[0].bHeight
    cmp     eax, p.y
    jl      TimerProc_EndedExit

    mov     Game.ButtonIndex, 1
    ret

TimerProc_EndedExit:
    mov     Game.ButtonIndex, 0
    ret
TimerProc_Ended ENDP

;-----------------------------------------------------------------------
LMouseProc_Prepared PROC,
	hWnd: DWORD,
	cursorPosition: Coord
;-----------------------------------------------------------------------
    mov     eax, Game.InstructionIndex
    .IF eax == 0
      mov   eax, START_BUTTON_POS.x
      cmp   eax, cursorPosition.x
      jg    LMouseProc_PreparedExit
      add   eax, buttonHandler[0].bWidth
      cmp   eax, cursorPosition.x
      jl    LMouseProc_PreparedExit
      mov   eax, START_BUTTON_POS.y
      cmp   eax, cursorPosition.y
      jg    LMouseProc_PreparedExit
      add   eax, buttonHandler[0].bHeight
      cmp   eax, cursorPosition.y
      jl    LMouseProc_PreparedExit

      inc   Game.InstructionIndex

    .ELSEIF eax == 1
      mov   eax, SKIP_BUTTON_POS1.x
      cmp   eax, cursorPosition.x
      jg    LMouseInstruction1Next
      add   eax, buttonHandler[TYPE BitmapInfo].bWidth
      cmp   eax, cursorPosition.x
      jl    LMouseInstruction1Next
      mov   eax, SKIP_BUTTON_POS1.y
      cmp   eax, cursorPosition.y
      jg    LMouseInstruction1Next
      add   eax, buttonHandler[TYPE BitmapInfo].bHeight
      cmp   eax, cursorPosition.y
      jl    LMouseInstruction1Next

      mov   Game.InstructionIndex, 0
      mov   Game.ButtonIndex, 0
      inc   Game.State
      jmp   LMouseProc_PreparedExit

LMouseInstruction1Next:
      mov   eax, CONTINUE_BUTTON_POS1.x
      cmp   eax, cursorPosition.x
      jg    LMouseProc_PreparedExit
      add   eax, buttonHandler[TYPE BitmapInfo * 2].bWidth
      cmp   eax, cursorPosition.x
      jl    LMouseProc_PreparedExit
      mov   eax, CONTINUE_BUTTON_POS1.y
      cmp   eax, cursorPosition.y
      jg    LMouseProc_PreparedExit
      add   eax, buttonHandler[TYPE BitmapInfo * 2].bHeight
      cmp   eax, cursorPosition.y
      jl    LMouseProc_PreparedExit

      inc   Game.InstructionIndex

    .ELSEIF eax == 2
      mov   eax, SKIP_BUTTON_POS2.x
      cmp   eax, cursorPosition.x
      jg    LMouseInstruction2Next
      add   eax, buttonHandler[TYPE BitmapInfo].bWidth
      cmp   eax, cursorPosition.x
      jl    LMouseInstruction2Next
      mov   eax, SKIP_BUTTON_POS2.y
      cmp   eax, cursorPosition.y
      jg    LMouseInstruction2Next
      add   eax, buttonHandler[TYPE BitmapInfo].bHeight
      cmp   eax, cursorPosition.y
      jl    LMouseInstruction2Next

      mov   Game.InstructionIndex, 0
      mov   Game.ButtonIndex, 0
      inc   Game.State
      jmp   LMouseProc_PreparedExit

LMouseInstruction2Next:
      mov   eax, CONTINUE_BUTTON_POS2.x
      cmp   eax, cursorPosition.x
      jg    LMouseProc_PreparedExit
      add   eax, buttonHandler[TYPE BitmapInfo * 2].bWidth
      cmp   eax, cursorPosition.x
      jl    LMouseProc_PreparedExit
      mov   eax, CONTINUE_BUTTON_POS2.y
      cmp   eax, cursorPosition.y
      jg    LMouseProc_PreparedExit
      add   eax, buttonHandler[TYPE BitmapInfo * 2].bHeight
      cmp   eax, cursorPosition.y
      jl    LMouseProc_PreparedExit

      inc   Game.InstructionIndex

    .ELSEIF eax == 3
      mov   eax, READY_BUTTON_POS.x
      cmp   eax, cursorPosition.x
      jg    LMouseProc_PreparedExit
      add   eax, buttonHandler[TYPE BitmapInfo * 3].bWidth
      cmp   eax, cursorPosition.x
      jl    LMouseProc_PreparedExit
      mov   eax, READY_BUTTON_POS.y
      cmp   eax, cursorPosition.y
      jg    LMouseProc_PreparedExit
      add   eax, buttonHandler[TYPE BitmapInfo * 3].bHeight
      cmp   eax, cursorPosition.y
      jl    LMouseProc_PreparedExit

      mov   Game.InstructionIndex, 0
      mov   Game.ButtonIndex, 0
      inc   Game.State
    .ENDIF

LMouseProc_PreparedExit:
    ret
LMouseProc_Prepared ENDP

;-----------------------------------------------------------------------
LMouseProc_Started PROC,
	hWnd: DWORD,
	cursorPosition: Coord
;-----------------------------------------------------------------------
    LOCAL   oriX: DWORD         ; 画标志的原点
    LOCAL   oriY: DWORD         ; 画标志的原点
    ; INVOKE MessageBox, hWnd, NULL, NULL, MB_OK

    mov     eax, Game.IsClicked
    .IF eax == 1                ; Circle已有
      mov   ebx, OFFSET Game.TowerArray
      mov   eax, Game.ClickedIndex
      .WHILE eax > 0
        add ebx, TYPE Tower
        dec eax
      .ENDW

      ; 调整原点位置
      mov   eax, (Tower PTR [ebx]).Pos.x
      mov   oriX, eax
      mov   eax, (Tower PTR [ebx]).Pos.y
      mov   oriY, eax
      mov   edx, OFFSET towerHandler
      mov   eax, (Tower PTR [ebx]).Tower_Type
      .WHILE eax > 0
        add edx, TYPE BitmapInfo
        dec eax
      .ENDW
      mov   eax, (BitmapInfo PTR [edx]).bWidth
      shr   eax, 1
      add   oriX, eax
      mov   eax, (BitmapInfo PTR [edx]).bHeight
      shr   eax, 1
      sub   oriY, eax
      mov   eax, signHandler[0].bWidth
      shr   eax, 1
      sub   oriX, eax
	  mov   eax, signHandler[0].bHeight
      shr   eax, 1
      sub   oriY, eax

      mov   eax, (Tower PTR [ebx]).Tower_Type
      .IF eax == 0                  ; Blank Circle已有
        mov ecx, blankSignNum
        dec ecx
        mov edx, OFFSET signHandler
        add edx, TYPE BitmapInfo
        mov esi, OFFSET signPosition
        add esi, TYPE Coord
        mov edi, 1

CheckSignClicked0:
        mov  eax, (Coord PTR [esi]).x
        add  eax, oriX
        cmp  cursorPosition.x, eax
        jb   CheckSignClicked1
        add  eax, (BitmapInfo PTR [edx]).bWidth
        cmp  cursorPosition.x, eax
        ja   CheckSignClicked1
        mov  eax, (Coord PTR [esi]).y
        add  eax, oriY
        cmp  cursorPosition.y, eax
        jb   CheckSignClicked1
        add  eax, (BitmapInfo PTR [edx]).bHeight
        cmp  cursorPosition.y, eax
        ja   CheckSignClicked1

        mov  (Tower PTR [ebx]).Tower_Type, edi
        .IF edi == 1
            mov  (Tower PTR [ebx]).Attack, 10
        .ELSEIF edi == 2
            mov  (Tower PTR [ebx]).Attack, 10
        .ELSEIF edi == 3
            mov  (Tower PTR [ebx]).Attack, 20
        .ELSE
            mov  (Tower PTR [ebx]).Attack, 15
        .ENDIF
        jmp  CheckSignClicked2
CheckSignClicked1:
        add  edx, TYPE BitmapInfo
        add  esi, TYPE Coord
        inc  edi
        loop CheckSignClicked0

CheckSignClicked2:
        mov  Game.IsClicked, 0
        jmp  LMouseProcExit

      .ELSE                         ; Tower Circle已有
        mov  ecx, towerSignNum
        dec  ecx
        mov  edx, OFFSET signHandler
        mov  eax, blankSignNum
        .WHILE eax > 0
          add edx, TYPE BitmapInfo
          dec eax
        .ENDW
        add  edx, TYPE BitmapInfo
        mov  esi, OFFSET signPosition
        add  esi, TYPE Coord

        mov  eax, (Coord PTR [esi]).x
        add  eax, oriX
        cmp  cursorPosition.x, eax
        jb   CheckSignClicked3
        add  eax, (BitmapInfo PTR [edx]).bWidth
        cmp  cursorPosition.x, eax
        ja   CheckSignClicked3
        mov  eax, (Coord PTR [esi]).y
        add  eax, oriY
        cmp  cursorPosition.y, eax
        jb   CheckSignClicked3
        add  eax, (BitmapInfo PTR [edx]).bHeight
        cmp  cursorPosition.y, eax
        ja   CheckSignClicked3

        add  (Tower PTR [ebx]).Tower_Type, 4
        jmp  CheckSignClicked4

CheckSignClicked3:
        add  edx, TYPE BitmapInfo
        add  esi, TYPE Coord

        mov  eax, (Coord PTR [esi]).x
        add  eax, oriX
        cmp  cursorPosition.x, eax
        jb   CheckSignClicked4
        add  eax, (BitmapInfo PTR [edx]).bWidth
        cmp  cursorPosition.x, eax
        ja   CheckSignClicked4
        mov  eax, (Coord PTR [esi]).y
        add  eax, oriY
        cmp  cursorPosition.y, eax
        jb   CheckSignClicked4
        add  eax, (BitmapInfo PTR [edx]).bHeight
        cmp  cursorPosition.y, eax
        ja   CheckSignClicked4

        mov  (Tower PTR [ebx]).Tower_Type, 0
CheckSignClicked4:
        mov  Game.IsClicked, 0
        jmp  LMouseProcExit
      .ENDIF
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
    INVOKE InvalidateRect, hWnd, NULL, FALSE
    ret
LMouseProc_Started ENDP

;-----------------------------------------------------------------------
LMouseProc_Ended PROC,
	hWnd: DWORD,
	cursorPosition: Coord
;-----------------------------------------------------------------------
    mov     eax, END_BUTTON_POS.x
    cmp     eax, cursorPosition.x
    jg      LMouseProc_EndedExit
    add     eax, buttonHandler[0].bWidth
    cmp     eax, cursorPosition.x
    jl      LMouseProc_EndedExit
    mov     eax, END_BUTTON_POS.y
    cmp     eax, cursorPosition.y
    jg      LMouseProc_EndedExit
    add     eax, buttonHandler[0].bHeight
    cmp     eax, cursorPosition.y
    jl      LMouseProc_EndedExit
    INVOKE  InitMapInfo
    INVOKE  LoadGameInfo
    INVOKE  mciSendCommand,Mp3DeviceID,MCI_CLOSE,0,0
	INVOKE  PlayMp3File, hWnd, ADDR MusicFileName 
	mov     countTime, 0
    mov     Game.State, 1

LMouseProc_EndedExit:
    ret
LMouseProc_Ended ENDP

;-----------------------------------------------------------------------
PaintTowers PROC
;-----------------------------------------------------------------------
    ; 画出所有空地
	INVOKE  SelectObject, imgDC, towerHandler[0].bHandler
    mov     ecx, Game.Tower_Num
    mov     ebx, OFFSET Game.TowerArray
DrawBlank:
	push    ecx
    mov     eax, (Tower PTR [ebx]).Pos.y
    sub     eax, towerHandler[0].bHeight
	INVOKE  TransparentBlt, 
            memDC, (Tower PTR [ebx]).Pos.x, eax,
            towerHandler[0].bWidth, towerHandler[0].bHeight,
            imgDC, 0, 0, 
            towerHandler[0].bWidth, towerHandler[0].bHeight,
            tcolor
	add     ebx, TYPE Tower
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

	mov		edx, (Tower PTR [ebx]).AnimatePlaying
	.IF		edx == 1
		jmp DrawTowers0
	.ENDIF

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
    LOCAL   hMonster: DWORD
    LOCAL   x: DWORD
    LOCAL   y: DWORD

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
    mov     hMonster, edx
    INVOKE  SelectObject, imgDC, (BitmapInfo PTR [edx]).bHandler
    mov     edx, hMonster
	INVOKE	TransparentBlt, 
			memDC, (Enemy PTR [ebx]).Current_Pos.x, (Enemy PTR [ebx]).Current_Pos.y,
            (BitmapInfo PTR [edx]).bWidth, (BitmapInfo PTR [edx]).bHeight, 
			imgDC, 0, 0,
            (BitmapInfo PTR [edx]).bWidth, (BitmapInfo PTR [edx]).bHeight, 
			tcolor
    ; 画血条
    INVOKE  SelectObject, memDC, hTotalLifePen
    mov     hOldPen, eax
    mov     edx, hMonster
    mov     eax, (Enemy PTR [ebx]).Current_Pos.y
    sub     eax, lifeHeight
    mov     y, eax
    mov     eax, (Enemy PTR [ebx]).Current_Pos.x
    mov     x, eax
    INVOKE  MoveToEx, memDC, x, y, NULL
    mov     edx, hMonster
    mov     eax, (BitmapInfo PTR [edx]).bWidth
    add     x, eax
    INVOKE  LineTo, memDC, x, y
    INVOKE  SelectObject, memDC, hCurrentLifePen
    mov     edx, hMonster
    mov     eax, (Enemy PTR [ebx]).Current_Pos.x
    mov     x, eax
    INVOKE  MoveToEx, memDC, x, y, NULL
    mov     edx, hMonster
    mov     eax, (BitmapInfo PTR [edx]).bWidth
    mul     WORD PTR (Enemy PTR [ebx]).Current_Life
    div     WORD PTR (Enemy PTR [ebx]).Total_Life
    add     x, eax
    mov     edx, hMonster
    INVOKE  LineTo, memDC, x, y
    INVOKE  SelectObject, memDC, hOldPen
    add     esi, TYPE DWORD
    pop     ecx
    dec     ecx
    cmp     ecx, 0
    je      PaintMonstersExit
    jmp     DrawMonsters
	
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
    mov     edx, OFFSET towerHandler
    mov     eax, (Tower PTR [ebx]).Tower_Type
    .WHILE  eax > 0
      add   edx, TYPE BitmapInfo
      dec   eax
    .ENDW
    mov     eax, (BitmapInfo PTR [edx]).bWidth
    shr     eax, 1
    add     oriX, eax
    mov     eax, (BitmapInfo PTR [edx]).bHeight
    shr     eax, 1
    sub     oriY, eax
    mov     eax, signHandler[0].bWidth
    shr     eax, 1
    sub     oriX, eax
	mov     eax, signHandler[0].bHeight
    shr     eax, 1
    sub     oriY, eax

	mov		eax, (Tower PTR [ebx]).Tower_Type
	.IF eax == 0
	  mov	ecx, blankSignNum
      mov   ebx, OFFSET signHandler
	  mov   edx, OFFSET signPosition
DrawBlankSigns:
      push  ecx
      push  edx
      mov   eax, oriX
      add   eax, (Coord PTR [edx]).x
      mov   x, eax
      mov   eax, oriY
      add   eax, (Coord PTR [edx]).y
      mov   y, eax
    INVOKE  SelectObject, imgDC, (BitmapInfo PTR [ebx]).bHandler
    INVOKE	TransparentBlt, 
			memDC, x, y,
            (BitmapInfo PTR [ebx]).bWidth, (BitmapInfo PTR [ebx]).bHeight, 
			imgDC, 0, 0,
            (BitmapInfo PTR [ebx]).bWidth, (BitmapInfo PTR [ebx]).bHeight, 
			tcolor
      pop   edx
      add   ebx, TYPE BitmapInfo
      add   edx, TYPE Coord
      pop   ecx
      loop  DrawBlankSigns
	.ELSE
	  mov	ecx, towerSignNum
      mov   ebx, OFFSET signHandler
      mov   eax, blankSignNum
      .WHILE eax > 0
        add ebx, TYPE signHandler
        dec eax
      .ENDW
	  mov   edx, OFFSET signPosition
DrawTowerSigns:
      push  ecx
      push  edx
      mov   eax, oriX
      add   eax, (Coord PTR [edx]).x
      mov   x, eax
      mov   eax, oriY
      add   eax, (Coord PTR [edx]).y
      mov   y, eax
    INVOKE  SelectObject, imgDC, (BitmapInfo PTR [ebx]).bHandler
    INVOKE	TransparentBlt, 
			memDC, x, y,
            (BitmapInfo PTR [ebx]).bWidth, (BitmapInfo PTR [ebx]).bHeight, 
			imgDC, 0, 0,
            (BitmapInfo PTR [ebx]).bWidth, (BitmapInfo PTR [ebx]).bHeight, 
			tcolor
      pop   edx
      add   ebx, TYPE BitmapInfo
      add   edx, TYPE Coord
      pop   ecx
      loop  DrawTowerSigns
	.ENDIF

PaintSignsExit:
    ret
PaintSigns ENDP

;---------------------------------------------------
PaintBullets PROC uses eax ebx ecx edx
;
;---------------------------------------------------
	LOCAL   count:DWORD

	mov		ecx, Game.Bullet_Num
	cmp		ecx, 0
	je		DrawBulletExit

	mov     edx, OFFSET Game.BulletArray
	
DrawBullet:
	; get the image
	push	edx
    mov		count, ecx
    mov     ebx, OFFSET bulletHandler
	mov		eax, (Bullet PTR [edx]).Bullet_Type
	cmp		eax, 0			; 空地
	je		L2

	sub   eax, 1
	mov		ecx, type BitmapInfo
	mul		ecx
	add		ebx, eax

L1:
	INVOKE  SelectObject, imgDC, (BitmapInfo PTR [ebx]).bHandler
	pop		edx
    push    edx
    INVOKE	TransparentBlt, 
			memDC, (Bullet PTR [edx]).Pos.x, (Bullet PTR [edx]).Pos.y,
            (BitmapInfo PTR [ebx]).bWidth, (BitmapInfo PTR [ebx]).bHeight, 
			imgDC, 0, 0,
            (BitmapInfo PTR [ebx]).bWidth, (BitmapInfo PTR [ebx]).bHeight, 
			tcolor
    pop     edx
	add		edx, TYPE Bullet
L2:
	mov		ecx, count
	loop	DrawBullet

DrawBulletExit:
	ret
PaintBullets ENDP

;--------------------------------------------------
UpdateWindmill PROC uses eax ebx
;
;--------------------------------------------------
	mov		ebx, OFFSET windmill

	mov		eax, (Animate PTR [ebx]).Gesture
	inc		eax
	.IF		eax > 7
		mov	 eax, 0
	.ENDIF
	mov		(Animate PTR [ebx]).Gesture, eax

	ret
UpdateWindmill ENDP

;---------------------------------------------------
DrawSingleAnimate PROC uses eax ecx edx 
; 
;	ebx-offset animate
;---------------------------------------------------
	Local	animateT:DWORD
	Local	gesture:DWORD

	mov		eax, 0

	; get the image
    mov     edx, OFFSET animateHandler
	push	edx
	
	mov		eax, (Animate PTR [ebx]).Animate_Type
	mov		animateT, eax
	mov		ecx, 8
	mul		ecx
	add		eax, (Animate PTR [ebx]).Gesture
	mov		ecx, (Animate PTR [ebx]).Gesture
	mov		gesture, ecx

	pushad
	.IF	gesture == 1
		.IF		animateT == 1			; 加上炸弹声音
			INVOKE	PlaySound, OFFSET BombFileName, 0, SND_ASYNC
		.ELSEIF	animateT == 2
			INVOKE  PlaySound, OFFSET AhFileName, 0, SND_ASYNC
		.ENDIF
	.ENDIF
	popad

	mov		ecx, type BitmapInfo
	mul		ecx

	pop		edx
	add		edx, eax

	push edx
	INVOKE  SelectObject, imgDC, (BitmapInfo PTR [edx]).bHandler
	pop edx
	INVOKE	TransparentBlt, 
			memDC, (Animate PTR [ebx]).Pos.x, (Animate PTR [ebx]).Pos.y,
			(BitmapInfo PTR [edx]).bWidth, (BitmapInfo PTR [edx]).bHeight, 
			imgDC, 0, 0,
            (BitmapInfo PTR [edx]).bWidth, (BitmapInfo PTR [edx]).bHeight, 
			tcolor

	ret
DrawSingleAnimate ENDP

;---------------------------------------------------
PaintAnimates PROC uses eax ebx ecx edx 
;
;---------------------------------------------------
	mov		eax, 0
	mov		ebx, 0

	mov		ecx, Game.Animate_Num
	cmp		ecx, 0
	je		PaintAnimatesExit

	; get the struct-Animate
	mov     ebx, OFFSET Game.AnimateArray
L1:
	push	ecx
	INVOKE	DrawSingleAnimate
	add		ebx, TYPE Animate
	pop		ecx
	loop	L1

PaintAnimatesExit:
	ret
PaintAnimates ENDP

;--------------------------------------------------------------
TransferNumToString PROC,
	arrayOffset: DWORD
;	
;	Receives: eax num to transfer
;	Returns: none but change the array which pointed by arrayOffset
;--------------------------------------------------------------
	Local num:DWORD
	Local arraySize:BYTE
	
	push ebx
	push ecx
	push esi
	push edi

	mov	ecx, 8
clear:
	mov esi, arrayOffset
	mov ebx, 0
	mov [esi], ebx
	loop clear

	mov	num, eax
	mov ebx, 10		; divider
	mov ch, 0		; array size
	mov esi, OFFSET textArrayA		; temp array
L1:
	mov edx, 0
	div ebx
	
	.IF eax == 0
		.IF edx == 0
			mov arraySize, ch
			mov ch, 0
			jmp L2
		.ENDIF
	.ENDIF

	add dl, '0'
	mov [esi], dl
	add	esi, type byte

	inc ch
	jmp L1
L2:
	mov edi, arrayOffset
	mov al, 48
	.IF arraySize == 0
		mov [edi], al
		jmp L4
	.ENDIF

L3:
	cmp ch, arraySize
	jae  L4

	sub	esi, type byte
	mov al, [esi]
	mov [edi], al

	add	edi, type byte
	inc ch
	cmp ch, arraySize
	jb	L3

L4:
	pop edi
	pop esi
	pop ecx
	pop ebx
	ret
TransferNumToString ENDP

;---------------------------------------------------------------
PaintText PROC 
;
;	draw blood, count and wave
;---------------------------------------------------------------
	LOCAL textRect: RECT
	pushad

	INVOKE SetBkMode, memDC, TRANSPARENT
	INVOKE SetTextColor, memDC, 0ffffffh
	INVOKE SelectObject, memDC, textFont

	; life
	mov eax, Game.Player_Life
	INVOKE TransferNumToString, ADDR textLife
	mov textRect.top, 17
	mov textRect.left, 60
	mov textRect.right, 300
	mov textRect.bottom, 200
	INVOKE DrawText, memDC, ADDR textLife, -1, ADDR textRect, DT_VCENTER

	; money
	mov eax, Game.Player_Money
	INVOKE TransferNumToString, ADDR textMoney
	mov textRect.left, 110
	INVOKE DrawText, memDC, ADDR textMoney, -1, ADDR textRect, DT_VCENTER

	; wave
	mov eax, Game.Next_Round
	INVOKE TransferNumToString, ADDR textWave
	mov textRect.top, 42
	mov textRect.left, 104
	INVOKE DrawText, memDC, ADDR textWave, -1, ADDR textRect, DT_VCENTER

	mov textRect.left, 115
	INVOKE DrawText, memDC, ADDR textWave2, -1, ADDR textRect, DT_VCENTER

	mov eax, Game.Round_Num
	INVOKE TransferNumToString, ADDR textWave3
	mov textRect.left, 125
	INVOKE DrawText, memDC, ADDR textWave3, -1, ADDR textRect, DT_VCENTER

	popad
	ret
PaintText ENDP

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

    INVOKE  CreatePen, PS_SOLID, 0, currentLifeColor
    mov     hCurrentLifePen, eax
    INVOKE  CreatePen, PS_SOLID, 0, totalLifeColor
    mov     hTotalLifePen, eax

    ; 判断是否处于等待页面
    cmp     Game.State, 1
    je      AlreadyStarted
    cmp     Game.State, 0
    jg      AlreadyEnded

    ; 游戏尚未开始
    ; Start页面
    mov     eax, Game.InstructionIndex
    mov     ebx, OFFSET instructionHandler
    .WHILE eax > 0
      add   ebx, TYPE BitmapInfo
      dec   eax
    .ENDW
    INVOKE 	SelectObject, imgDC, (BitmapInfo PTR [ebx]).bHandler
	INVOKE 	StretchBlt, 
			memDC, 0, 0, window_w, window_h, 
			imgDC, 0, 0, (BitmapInfo PTR [ebx]).bWidth, (BitmapInfo PTR [ebx]).bHeight, 
			SRCCOPY

    ; 悬停的button绘制
    cmp     Game.ButtonIndex, 0
    je      PaintProcExit

    mov     eax, Game.InstructionIndex
    .IF eax == 0
        INVOKE  SelectObject, imgDC, buttonHandler[0].bHandler
        INVOKE  TransparentBlt, 
                memDC, START_BUTTON_POS.x, START_BUTTON_POS.y,
                buttonHandler[0].bWidth, buttonHandler[0].bHeight,
                imgDC, 0, 0, 
                buttonHandler[0].bWidth, buttonHandler[0].bHeight, 
                tcolor
    .ELSEIF eax == 1
      mov   eax, Game.ButtonIndex
      .IF eax == 1
        INVOKE  SelectObject, imgDC, buttonHandler[TYPE BitmapInfo].bHandler
        INVOKE  TransparentBlt, 
                memDC, SKIP_BUTTON_POS1.x, SKIP_BUTTON_POS1.y,
                buttonHandler[TYPE BitmapInfo].bWidth, 
                buttonHandler[TYPE BitmapInfo].bHeight,
                imgDC, 0, 0, 
                buttonHandler[TYPE BitmapInfo].bWidth, 
                buttonHandler[TYPE BitmapInfo].bHeight,
                tcolor 
      .ELSE
        INVOKE  SelectObject, imgDC, buttonHandler[TYPE BitmapInfo * 2].bHandler
        INVOKE  TransparentBlt, 
                memDC, CONTINUE_BUTTON_POS1.x, CONTINUE_BUTTON_POS1.y,
                buttonHandler[TYPE BitmapInfo * 2].bWidth, 
                buttonHandler[TYPE BitmapInfo * 2].bHeight,
                imgDC, 0, 0, 
                buttonHandler[TYPE BitmapInfo * 2].bWidth, 
                buttonHandler[TYPE BitmapInfo * 2].bHeight,
                tcolor 
      .ENDIF
    .ELSEIF eax == 2
      mov   eax, Game.ButtonIndex
      .IF eax == 1
        INVOKE  SelectObject, imgDC, buttonHandler[TYPE BitmapInfo].bHandler
        INVOKE  TransparentBlt, 
                memDC, SKIP_BUTTON_POS2.x, SKIP_BUTTON_POS2.y,
                buttonHandler[TYPE BitmapInfo].bWidth, 
                buttonHandler[TYPE BitmapInfo].bHeight,
                imgDC, 0, 0, 
                buttonHandler[TYPE BitmapInfo].bWidth, 
                buttonHandler[TYPE BitmapInfo].bHeight,
                tcolor
      .ELSE
        INVOKE  SelectObject, imgDC, buttonHandler[TYPE BitmapInfo * 2].bHandler
        INVOKE  TransparentBlt, 
                memDC, CONTINUE_BUTTON_POS2.x, CONTINUE_BUTTON_POS2.y,
                buttonHandler[TYPE BitmapInfo * 2].bWidth, 
                buttonHandler[TYPE BitmapInfo * 2].bHeight,
                imgDC, 0, 0, 
                buttonHandler[TYPE BitmapInfo * 2].bWidth, 
                buttonHandler[TYPE BitmapInfo * 2].bHeight,
                tcolor
      .ENDIF
    .ELSE
        INVOKE  SelectObject, imgDC, buttonHandler[TYPE BitmapInfo * 3].bHandler
        INVOKE  TransparentBlt, 
                memDC, READY_BUTTON_POS.x, READY_BUTTON_POS.y,
                buttonHandler[TYPE BitmapInfo * 3].bWidth, 
                buttonHandler[TYPE BitmapInfo * 3].bHeight,
                imgDC, 0, 0, 
                buttonHandler[TYPE BitmapInfo * 3].bWidth, 
                buttonHandler[TYPE BitmapInfo * 3].bHeight,
                tcolor
    .ENDIF
    jmp     PaintProcExit

AlreadyEnded:
    mov     ebx, OFFSET endHandler
    .IF Game.State == 3
      add   ebx, TYPE BitmapInfo
    .ENDIF
    INVOKE 	SelectObject, imgDC, (BitmapInfo PTR [ebx]).bHandler
	INVOKE 	StretchBlt, 
			memDC, 0, 0, window_w, window_h, 
			imgDC, 0, 0, (BitmapInfo PTR [ebx]).bWidth, (BitmapInfo PTR [ebx]).bHeight, 
			SRCCOPY

    ; 悬停的button绘制
    cmp     Game.ButtonIndex, 0
    je      PaintProcExit

    INVOKE  SelectObject, imgDC, buttonHandler[0].bHandler
    INVOKE  TransparentBlt, 
            memDC, END_BUTTON_POS.x, END_BUTTON_POS.y,
            buttonHandler[0].bWidth, buttonHandler[0].bHeight,
            imgDC, 0, 0, 
            buttonHandler[0].bWidth, buttonHandler[0].bHeight, 
            tcolor
    jmp     PaintProcExit

    ; 游戏已经开始
AlreadyStarted:
	; 画地图
	INVOKE 	SelectObject, imgDC, mapHandler[0].bHandler
	INVOKE 	StretchBlt, 
			memDC, 0, 0, window_w, window_h, 
			imgDC, 0, 0, mapHandler[0].bWidth, mapHandler[0].bHeight, 
			SRCCOPY

	; 画空地，塔
    INVOKE  PaintTowers

	; 画文字
	INVOKE	PaintText

	; 画兵

	; 画小怪
	INVOKE 	PaintMonsters

	; 画子弹
	INVOKE  PaintBullets

	; 画动画
	mov     ebx, OFFSET windmill
	INVOKE	DrawSingleAnimate
	INVOKE  UpdateWindmill
	INVOKE	PaintAnimates

	; 画建塔提示圆圈
	INVOKE	PaintSigns

PaintProcExit:
	INVOKE 	BitBlt, hDC, 0, 0, window_w, window_h, memDC, 0, 0, SRCCOPY
    INVOKE 	DeleteObject, hBitmap
    INVOKE 	DeleteDC, memDC
	INVOKE 	DeleteDC, imgDC
    INVOKE 	ReleaseDC, hWnd, hDC

	ret
PaintProc ENDP

;----------------------------------------------------------------------
PlayMp3File PROC hWnd:DWORD, NameOfFile:DWORD
;
; 播放音乐函数
;----------------------------------------------------------------------
	LOCAL   mciOpenParms: MCI_OPEN_PARMS
    LOCAL   mciPlayParms: MCI_PLAY_PARMS

    INVOKE  RtlZeroMemory, ADDR mciOpenParms, SIZEOF mciOpenParms
    INVOKE  RtlZeroMemory, ADDR mciPlayParms, SIZEOF mciPlayParms
	mov     eax, OFFSET Mp3Device
	mov     mciOpenParms.lpstrDeviceType, eax
	mov     eax, NameOfFile
	mov     mciOpenParms.lpstrElementName, eax
	INVOKE  mciSendCommand, 0, MCI_OPEN,MCI_OPEN_TYPE or MCI_OPEN_ELEMENT, ADDR mciOpenParms
	mov     eax, mciOpenParms.wDeviceID
	mov     Mp3DeviceID, eax
	
    mov     eax, hWnd        
	mov     mciPlayParms.dwCallback, eax
    INVOKE  mciSendCommand, Mp3DeviceID, MCI_PLAY, MCI_NOTIFY, ADDR mciPlayParms
	
	ret
PlayMp3File ENDP

;----------------------------------------------------------------------
ContinuePlayMp3File PROC hWnd:DWORD
;
; 播放音乐函数
;----------------------------------------------------------------------
	LOCAL   mciPlayParms: MCI_PLAY_PARMS
    LOCAL   seekParam: MCI_SEEK_PARMS
 
    INVOKE  RtlZeroMemory, ADDR seekParam, SIZEOF seekParam
    INVOKE  mciSendCommand, Mp3DeviceID, MCI_SEEK, MCI_SEEK_TO_START, ADDR seekParam

	mov     eax, hWnd        
	mov     mciPlayParms.dwCallback, eax
    INVOKE  mciSendCommand, Mp3DeviceID, MCI_PLAY, MCI_NOTIFY, ADDR mciPlayParms
	
	ret
ContinuePlayMp3File ENDP

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
