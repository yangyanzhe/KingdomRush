IFNDEF GUI_INC

INCLUDE struct.inc
INCLUDE data.inc

; º¯ÊýÉùÃ÷
InitiateImages	PROTO :DWORD

towerNum  =	 5


.data
bgBrush	HBRUSH ?
hDC		dd ?
map1	dd ?					; map handler

towerHandler dd towerNum DUP(?) 
towerSignHandler dd towerNum DUP(?) 


hBitmap	dd ?
memDC	dd ?
imgDC	dd ?
temp    dd ?
rect    RECT <>

ENDIF