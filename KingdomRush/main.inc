IFNDEF MAIN_INC
MAIN_INC     equ <2>

INCLUDE     struct.inc
INCLUDE     data.inc

.data
; ��������
window_x        dd 0
window_y        dd 0
window_w        dd 700
window_h        dd 600

; �豸������
hDC		        dd ?
memDC	        dd ?
imgDC	        dd ?

; ͼƬ��Դ
tcolor	        dd 0FFFFFFh				; transparent color

mapNum      	= 1
towerNum    	= 5
signNum     	= 5
monsterNum	 	= 1

mapHandler      BitmapInfo mapNum DUP(<>)
towerHandler    BitmapInfo towerNum DUP(<>) 
signHandler     BitmapInfo signNum DUP(<>)
monsterHandler  MonsterBitmapInfo monsterNum DUP(<>)

ENDIF