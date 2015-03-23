TITLE main.asm      
.386      
.model flat,stdcall      
option casemap:none    

INCLUDELIB kernel32.lib
INCLUDELIB user32.lib
INCLUDELIB gdi32.lib
INCLUDELIB masm32.lib
INCLUDELIB comctl32.lib
INCLUDELIB winmm.lib
INCLUDELIB gdiplus.lib

INCLUDE masm32.inc
INCLUDE windows.inc
INCLUDE user32.inc
INCLUDE kernel32.inc
INCLUDE gdi32.inc
INCLUDE comctl32.inc
INCLUDE winmm.inc 
INCLUDE gdiplus.inc
        
INCLUDE kingdomRush.inc 
     
.code      
start:      
	INVOKE GetTickCount
	INVOKE nseed, eax

    INVOKE GetModuleHandle,0    ;获取应用程序模块句柄   
    mov hInstance,eax           ;保存应用程序句柄 

	INVOKE GetCommandLine
	mov CommandLine, eax
    
	INVOKE WinMain,hInstance,0,CommandLine,SW_SHOWDEFAULT      
    INVOKE ExitProcess,eax      ;退出程序,并返回eax的值   

;-------------------------------------------------------
WinMain PROC hInst:DWORD, 
			 hPrevInst:DWORD,
			 CmdLine:DWORD,
			 CmdShow:DWORD      
;
; Create the window and menu
; Receives: hInst(handler for current App), hPrevInst etc.
; Returns:  nothing
;--------------------------------------------------------
    LOCAL wndclass:WNDCLASSEX      
    LOCAL msg:MSG      
	LOCAL dwStyle:DWORD
	LOCAL scrWidth:DWORD
	LOCAL scrHeight:DWORD

	INVOKE GdiplusStartup, ADDR GdiPlusStartupToken, ADDR GdiInput, 0

	;初始化窗口
    mov wndclass.cbSize,sizeof WNDCLASSEX      
    mov wndclass.style,CS_HREDRAW or CS_VREDRAW or CS_BYTEALIGNWINDOW      
    mov wndclass.lpfnWndProc,OFFSET WndProc      
    mov wndclass.cbClsExtra,0      
    mov wndclass.cbWndExtra,0      
    mov eax,hInst      
    mov wndclass.hInstance,eax     

	INVOKE CreateSolidBrush,BgColor
	mov bgBrush, eax
    mov wndclass.hbrBackground, eax      
    mov wndclass.lpszMenuName,0      
    mov wndclass.lpszClassName,OFFSET ClassName      
    
	INVOKE LoadIcon, 0, IDI_APPLICATION
	mov wndclass.hIcon, eax
	mov wndclass.hIconSm, eax

    INVOKE LoadCursor,0,IDC_ARROW      
	mov wndclass.hCursor,eax            

	;创建画刷、字体
	INVOKE CreateSolidBrush,TextBgColor
	mov textBgBrush, eax
	
	INVOKE CreateFont, 80,
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
    mov titleFont, eax
	INVOKE CreateFont, 22,
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

	;计算窗口位置，使窗口位于屏幕中央
	mov dwStyle, WS_OVERLAPPEDWINDOW
	mov eax, WS_SIZEBOX
	not eax
	and dwStyle, eax
	INVOKE GetSystemMetrics,SM_CXSCREEN
	mov scrWidth, eax
	INVOKE GetSystemMetrics,SM_CYSCREEN
	mov scrHeight, eax
	mov edx, 0
	mov ebx, 2
	mov eax, scrWidth
	sub eax, WndWidth
	div ebx
	mov WndOffX, eax
	mov eax, scrHeight
	sub eax, WndHeight
	div ebx
	mov WndOffY, eax

	;注册用户定义的窗口类
    INVOKE RegisterClassEx,ADDR wndclass        
	
	;创建窗口
	INVOKE CreateWindowEx,WS_EX_OVERLAPPEDWINDOW, ADDR ClassName,      
                            ADDR WindowName,      
                            dwStyle,      
                            WndOffX,WndOffY,WndWidth,WndHeight,      
                            0,0,      
                            hInst,0           
	.IF eax == 0		
		call ErrorHandler
		jmp Exit_Program
	.ENDIF		  

	;保存窗口句柄
    mov   hWnd,eax                          

	;载入图片
	INVOKE LoadBitmap, hInstance, 101
	mov BmpBackground, eax

	;显示绘制窗口
	INVOKE ShowWindow,hWnd,SW_SHOWNORMAL   
    INVOKE UpdateWindow,hWnd

	;开始程序的持续消息处理循环     
MessageLoop:      
    INVOKE GetMessage,ADDR msg,0,0,0        ;获取消息      
    cmp eax,0      
    je Exit_Program      
    INVOKE TranslateMessage,ADDR msg        ;转换键盘消息   
    INVOKE DispatchMessage,ADDR msg         ;分发消息   
    jmp MessageLoop  
    
	;关闭时钟
	INVOKE KillTimer, hWnd, 1
	INVOKE GdiplusShutdown, GdiPlusStartupToken

Exit_Program:
	INVOKE ExitProcess, 0  
	ret
WinMain ENDP

;----------------------------------------------------------------------   
WndProc PROC hWin:DWORD, uMsg:DWORD, wParam:DWORD, lParam:DWORD      
;
;消息处理函数
;----------------------------------------------------------------------
    LOCAL hPopMenu      ;一级菜单句柄
	LOCAL ps  :PAINTSTRUCT
	LOCAL pt  :POINT

    .IF uMsg == WM_CREATE      
		INVOKE CreateMenu   
		mov hMenu, eax   
		.IF eax
			INVOKE CreatePopupMenu      ;创建一级菜单   
			mov hPopMenu, eax           ;保存一级菜单句柄   
			INVOKE AppendMenu, hPopMenu, NULL, MENU_NEWGAMEM, addr MenuFileNewM   ;添加二级菜单
			INVOKE AppendMenu, hPopMenu, NULL, MENU_NEWGAMEH, addr MenuFileNewH   ;添加二级菜单
			INVOKE AppendMenu, hPopMenu, NULL, MENU_SAVEGAME, addr MenuFileSave   ;添加二级菜单
			INVOKE AppendMenu, hPopMenu, NULL, MENU_PLAYMUSIC, addr MenuFilePlay   ;添加二级菜单
			INVOKE AppendMenu, hPopMenu, NULL, MENU_STOPMUSIC, addr MenuFileStop   ;添加二级菜单
			INVOKE AppendMenu, hMenu, MF_POPUP, hPopMenu, addr MenuFile                ;添加一级菜单   
			INVOKE CreatePopupMenu      ;创建一级菜单   
			mov hPopMenu, eax           ;保存一级菜单句柄   
			INVOKE AppendMenu, hPopMenu, NULL, MENU_ABOUTAUTHOR, addr MenuAboutAuthor   ;添加二级菜单
			INVOKE AppendMenu, hPopMenu, NULL, MENU_HELPINFO, addr MenuAboutHelpInfo   ;添加二级菜单
			INVOKE AppendMenu, hMenu, MF_POPUP, hPopMenu, addr MenuAbout                ;添加一级菜单   
		.ENDIF   
		INVOKE SetMenu, hWin, hMenu     ;设置菜单
		jmp WndProcExit
	.ELSEIF uMsg == WM_CLOSE
		;保存游戏进度对话框
		INVOKE PostQuitMessage,0
		jmp WndProcExit
	.ELSEIF uMsg == WM_PAINT
		INVOKE BeginPaint, hWin, ADDR ps
		mov hDC, eax
		INVOKE PaintProc, hWin
		INVOKE EndPaint, hWin, ADDR ps
		jmp WndProcExit
    .ELSE
        INVOKE DefWindowProc,hWin,uMsg,wParam,lParam    ;调用默认消息处理函数   
        jmp WndProcExit      
    .ENDIF      
    ;xor eax,eax

WndProcExit:      
    ret      
WndProc endp      

;------------------------------------------------------------------
ErrorHandler PROC
;
;错误处理，打印出错误信息
;------------------------------------------------------------------
.data
pErrorMsg  DWORD ?		; ptr to error message
messageID  DWORD ?
.code
	INVOKE GetLastError	; Returns message ID in EAX
	mov messageID,eax

	; Get the corresponding message string.
	INVOKE FormatMessage, FORMAT_MESSAGE_ALLOCATE_BUFFER + \
	  FORMAT_MESSAGE_FROM_SYSTEM,NULL,messageID,NULL,
	  ADDR pErrorMsg,NULL,NULL

	; Display the error message.
	INVOKE MessageBox, NULL, pErrorMsg, ADDR ErrorTitle, MB_ICONERROR+MB_OK

	; Free the error message string.
	INVOKE LocalFree, pErrorMsg
	ret
ErrorHandler ENDP

;-----------------------------------------------------------------------
PaintProc PROC hWin:DWORD
;
; Painting Function
;-----------------------------------------------------------------------

	LOCAL hOld: DWORD
	LOCAL xIndex: DWORD
	LOCAL yIndex: DWORD
	LOCAL textRect: RECT
	LOCAL movedis: DWORD
	LOCAL scale: DWORD  ;1~100

	mov movedis, 0

    INVOKE CreateCompatibleDC, hDC
    mov memDC, eax
	INVOKE CreateCompatibleDC, hDC
    mov imgDC, eax
	INVOKE CreateCompatibleBitmap, hDC, WndWidth, WndHeight
	mov hBitmap, eax
    INVOKE SelectObject, memDC, hBitmap
    mov hOld, eax
	INVOKE FillRect, memDC, ADDR rect, bgBrush
	
	;画背景
	INVOKE SelectObject, imgDC, BmpBackground
	INVOKE StretchBlt, memDC, ClientOffX, ClientOffY, ClientWidth, ClientHeight, imgDC,0, 0, BgBmpWidth, BgBmpHeight, SRCCOPY
	
	INVOKE BitBlt, hDC, 0, 0, WndWidth, WndHeight, memDC, 0, 0, SRCCOPY 
    INVOKE SelectObject,hDC,hOld
    INVOKE DeleteDC,memDC
	INVOKE DeleteDC,imgDC
	INVOKE DeleteObject, hBitmap
    ret
PaintProc ENDP

end start