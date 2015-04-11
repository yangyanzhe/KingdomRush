IFNDEF MAIN_INC
MAIN_INC     equ <1>

INCLUDE     struct.inc
INCLUDE     data.inc

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

; 图片资源
tcolor	        dd 0FFFFFFh				; transparent color

mapNum      	= 1
towerNum    	= 5
signNum         = 8
blankSignNum    = 5
towerSignNum    = 3
monsterNum	 	= 1
bulletNum		= 2

mapHandler      BitmapInfo mapNum DUP(<>)
blankSet        PositionSet mapNum DUP(<>)
signHandler     BitmapInfo signNum DUP(<>)
towerHandler    BitmapInfo towerNum DUP(<>)
bulletHandler   BitmapInfo bulletNum DUP(<>) 
monsterHandler  MonsterBitmapInfo monsterNum DUP(<>)

IDI_ICON        EQU 101
IDB_MAP         EQU 102
IDB_MAPONE      EQU 102

blankIndex      dd 0, 8
blankPosition   Coord <200, 200>, <140, 230>, <220, 335>, <210, 380>, 
                      <300, 390>, <420, 390>, <340, 500>, <550, 480>
signPosition	Coord <0, 0>, <34, 0>, <34, 105>, <86, 51>, <-15, 51>

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
IDB_BULLET_BOMB  EQU 127
IDB_BULLET_MAGIC EQU 128

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

ENDIF
