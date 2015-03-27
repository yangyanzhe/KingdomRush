TITLE Windows Application                   (WinApp.asm)

    .386      
    .model flat,stdcall      
    option casemap:none

INCLUDE     windows.inc
INCLUDE     gdi32.inc
INCLUDELIB  gdi32.lib
INCLUDE     kernel32.inc
INCLUDELIB  kernel32.lib
INCLUDE     user32.inc
INCLUDELIB  user32.lib

;==================== DATA =======================
.data

hInstance       dd ?
hMainWnd        dd ?
classname       db "Game Application", 0
windowname      db "Kingdom Rush", 0
wndWidth        dd 568
wndHeight       dd 691
wndX            dd ?
wndY            dd ?

;=================== CODE =========================
.code

WinMain PROC
    LOCAL   lWndClass: WNDCLASSEX
    LOCAL   lMsg: MSG
    LOCAL   lScreenWidth: DWORD
    LOCAL   lScreenHeight: DWORD

    ; 获取句柄
    INVOKE  GetModuleHandle, NULL
    mov     hInstance, eax
    mov     lWndClass.hInstance, eax
    INVOKE  RtlZeroMemory, ADDR lWndClass, SIZEOF lWndClass

    ; 注册窗口
    INVOKE  LoadIcon, NULL, IDI_APPLICATION
    mov     lWndClass.hIcon, eax
    mov     lWndClass.hIconSm, eax
    INVOKE  LoadCursor, NULL, IDC_ARROW
    mov     lWndClass.hCursor, eax

    mov     lWndClass.cbSize, SIZEOF WNDCLASSEX
    mov     lWndClass.hbrBackground, COLOR_WINDOW + 1
    mov     lWndClass.lpfnWndProc, OFFSET WinProc
    mov     lWndClass.lpszClassName, OFFSET classname
    mov     lWndClass.style, CS_HREDRAW or CS_VREDRAW

    INVOKE  RegisterClassEx, ADDR lWndClass
    .IF eax == 0
      call  ErrorHandler
      jmp   Exit_Program
    .ENDIF

    ; 创建窗口（移至屏幕中央）
    INVOKE  GetSystemMetrics, SM_CXSCREEN
	mov     lScreenWidth, eax
	INVOKE  GetSystemMetrics, SM_CYSCREEN
	mov     lScreenHeight, eax
    mov     ebx, 2
	mov     edx, 0
	mov     eax, lScreenWidth
	sub     eax, wndWidth
	div     ebx
	mov     wndX, eax
	mov     eax, lScreenHeight
	sub     eax, wndHeight
	div     ebx
	mov     wndY, eax

    INVOKE  CreateWindowEx, 
            WS_EX_OVERLAPPEDWINDOW, 
            OFFSET classname,
            OFFSET windowname, 
            WS_OVERLAPPEDWINDOW,
            wndX, 
            wndY, 
            wndWidth,
            wndHeight, 
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
    INVOKE  GetMessage, ADDR lMsg, NULL, NULL, NULL

    ; 退出WM_QUIT
    .IF eax == 0
      jmp   Exit_Program
    .ENDIF

    INVOKE  TranslateMessage, ADDR lMsg
    INVOKE  DispatchMessage, ADDR lMsg
    jmp     Message_Loop

Exit_Program:
    ret
WinMain ENDP

;-----------------------------------------------------
WinProc PROC,
    hWnd:DWORD, localMsg:DWORD, wParam:DWORD, lParam:DWORD
; The application's message handler, which handles
; application-specific messages. All other messages
; are forwarded to the default Windows message
; handler.
;-----------------------------------------------------
    mov      eax, localMsg

    .IF eax == WM_LBUTTONDOWN       ; 鼠标事件
      jmp    WinProcExit
    .ELSEIF eax == WM_CREATE        ; 创建窗口事件
      jmp    WinProcExit
    .ELSEIF eax == WM_CLOSE         ; 关闭窗口事件
      INVOKE PostQuitMessage, 0
      jmp    WinProcExit
    .ELSE                           ; 其他事件
      INVOKE DefWindowProc, hWnd, localMsg, wParam, lParam
      jmp    WinProcExit
    .ENDIF

WinProcExit:
    ret
WinProc ENDP

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
    mov     messageID,eax

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
