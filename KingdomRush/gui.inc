IFNDEF GUI_INC

INCLUDE struct.inc
INCLUDE data.inc

; º¯ÊýÉùÃ÷
InitiateImages	PROTO :DWORD

DrawMonster PROTO,
	mPos:Coord,			; position on window
	mType:DWORD,		; monster type
	mStep:DWORD			; monster step

towerNum  =	 5
monsterOneNum = 5
IDI_ICON 		EQU 101
IDB_ARROW 		EQU 103
IDB_ARROW_SIGN	EQU 104
IDB_CIRCLE		EQU 105
IDB_MAGIC		EQU 106
IDB_MAGIC_SIGN	EQU 107
IDB_SODIER		EQU 109
IDB_TURRET		EQU 111
IDB_TURRET_SIGN	EQU 112
IDB_SODIER_SIGN	EQU 113
IDB_MAPONE		EQU 115
IDB_BLANK		EQU 116
IDB_MONSTER101	EQU 118
IDB_MONSTER102	EQU 119
IDB_MONSTER103	EQU 120
IDB_MONSTER104	EQU 121
IDB_MONSTER105	EQU 122

.data
tcolor	dd 0FFFFFFh				; transparent color
bgBrush	HBRUSH ?
hDC		dd ?
map1	dd ?					; map handler

towerHandler dd towerNum DUP(?) 

monsterOneHandler dd monsterOneNum DUP(?)

towerSignHandler dd towerNum DUP(?) 


hBitmap	dd ?
memDC	dd ?
imgDC	dd ?
temp    dd ?
rect    RECT <>

ENDIF