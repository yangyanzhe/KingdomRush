IFNDEF MAIN_INC
MAIN_INC     equ <1>

INCLUDE     struct.inc

.data
; 窗口信息
window_x        dd 0
window_y        dd 0
window_w        dd 700
window_h        dd 600

hInstance       dd ?
hMainWnd        dd ?
hIcon           dd ?
classname       db "Game Application", 0
windowname      db "Kingdom Rush", 0

; 设备上下文
hDC		        dd ?
memDC	        dd ?
imgDC	        dd ?

; 字体
textFont		HFONT ?
textArrayA		db 8 DUP (0)
textLife		db 8 DUP (0)
textMoney		db 8 DUP (0)
textWave		db 8 DUP (0)
textWave2		db "/",0
textWave3		db 8 DUP (0)
FontName		db "Arial", 0

; 图片资源
tcolor	        dd 0FFFFFFh				; transparent color

; 音乐
PlayFlag        dd 0
Mp3Files        db "*.mp3", 125 dup (0)
Mp3Device       db "MPEGVideo", 0
Mp3DeviceID     dd 0
Mp3BombDeviceID dd 0

MusicFileName db 'resource/music/background01.mp3', 0
StartFileName db 'resource/music/start.mp3', 0
ClickFileName db 'resource/music/click.wav', 0
BombFileName  db 'resource/music/bomb.wav', 0
AhFileName	  db 'resource/music/ah.wav', 0

instructionNum  = 4
buttonNum       = 4
mapNum      	= 1
towerNum    	= 5
signNum         = 8
blankSignNum    = 5
towerSignNum    = 3
monsterNum	 	= 1
bulletNum		= 4
animateNum    	= 32

instructionHandler  BitmapInfo instructionNum DUP(<>)
buttonHandler       BitmapInfo buttonNum DUP(<>)
mapHandler          BitmapInfo mapNum DUP(<>)
blankSet            PositionSet mapNum DUP(<>)
signHandler         BitmapInfo signNum DUP(<>)
towerHandler        BitmapInfo towerNum DUP(<>)
monsterHandler      MonsterBitmapInfo monsterNum DUP(<>)
bulletHandler       BitmapInfo bulletNum DUP(<>)
animateHandler      BitmapInfo animateNum DUP(<>)
windmill		    Animate <0, <500, 250>, 0>

IDI_ICON                EQU 101
IDB_INSTRUCTION         EQU 102
IDB_START               EQU 102
START_BUTTON_POS        Coord <247, 400>
IDB_INSTRUCTION1        EQU 103
SKIP_BUTTON_POS1        Coord <171, 463>
CONTINUE_BUTTON_POS1    Coord <362, 463>
IDB_INSTRUCTION2        EQU 104
SKIP_BUTTON_POS2        Coord <171, 417>
CONTINUE_BUTTON_POS2    Coord <362, 417>
IDB_INSTRUCTION3        EQU 105
READY_BUTTON_POS        Coord <264, 405>
IDB_BUTTON              EQU 106
IDB_START_BUTTON        EQU 106
IDB_INSTRUCTION_BUTTON1 EQU 107
IDB_INSTRUCTION_BUTTON2 EQU 108
IDB_INSTRUCTION_BUTTON3 EQU 109

IDB_SIGN        EQU 110
IDB_BLANKSIGN   EQU 110
IDB_BLANKCIRCLE	EQU 110
IDB_ARROW_SIGN	EQU 111
IDB_SODIER_SIGN EQU 112
IDB_MAGIC_SIGN	EQU 113
IDB_TURRET_SIGN	EQU 114

IDB_TOWERSIGN   EQU 115
IDB_TOWERCIRCLE EQU 115
IDB_UPDATE      EQU 116
IDB_SELL        EQU 117
IDB_PRICE       EQU 118

IDB_TOWER       EQU 119
IDB_BLANK		EQU 119
IDB_ARROW       EQU 120
IDB_SODIER		EQU 121
IDB_MAGIC		EQU 122
IDB_TURRET		EQU 123
IDB_BLANKCLICK  EQU 124

IDB_BULLET		 EQU 127

IDB_ANIMATE 		EQU	140

; 地图
IDB_MAP         EQU 190
IDB_MAPONE      EQU 191

blankIndex      dd 0, 8
blankPosition   Coord <200, 200>, <140, 230>, <220, 335>, <210, 380>, 
                      <300, 390>, <420, 390>, <340, 500>, <550, 480>
signPosition	Coord <0, 0>, <34, 0>, <34, 105>, <86, 51>, <-15, 51>

; Monster编号规则：ID+方向+状态
; 例如：101表示ID：1，方向：0，状态：1
IDB_MONSTER1	EQU 200	
IDB_MONSTER100	EQU 200
IDB_MONSTER101	EQU 201
IDB_MONSTER110	EQU 202
IDB_MONSTER111	EQU 203
IDB_MONSTER120	EQU 204
IDB_MONSTER121	EQU 205
IDB_MONSTER130	EQU 206
IDB_MONSTER131	EQU 207
IDB_MONSTER140	EQU 208
IDB_MONSTER141	EQU 209

; 计时器信息
TIMER_ID = 1
TIMER_INTERVAL = 100

; 血条信息
lifeHeight          = 5
currentLifeColor    = 000FF00h  ; Green
hCurrentLifePen     dd ?
totalLifeColor      = 00000FFh  ; Red
hTotalLifePen       dd ?
hOldPen             dd ?

ENDIF
