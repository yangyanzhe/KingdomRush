IFNDEF GUI_INC
GUI_INC     equ <2>

INCLUDE     struct.inc
INCLUDE     data.inc

.data
mapNum      = 1
towerNum    = 5
signNum     = 5

mapHandler      BitmapInfo mapNum DUP(<>)
towerHandler    BitmapInfo towerNum DUP(<>) 
signHandler     BitmapInfo signNum DUP(<>)

tcolor	    dd 0FFFFFFh				; transparent color
hDC		    dd ?
hBitmap	    dd ?
memDC	    dd ?
imgDC	    dd ?
temp        dd ?
rect        RECT <>

ENDIF