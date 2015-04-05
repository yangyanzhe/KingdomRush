IFNDEF  DATA_INT

.data
bgstart location <0, 0, 700, 600>
board   location <50, 50, 144, 142>
window  location <850, 150, 700, 640>
client  location <0, 0, 700, 600>

Game GameInfo <>

ROUND_NUMBER dd 4
ROUND_TRIGGER_TIME dd 3000, 6000, 9000, 12000
EACH_ROUND_ENEMY_NUMBER dd 3,4,5,6
EACH_ROUND_ENEMY_TYPE dd 0,0,0,
                         0,0,0,0,
                         0,0,0,0,0,
                         0,0,0,0,0,0
ENEMY_LIFE_0 = 40
ENEMY_MONEY_0 = 10

TOWERPOSITION dd 248, 312, 246, 365

; Ëþ×óÏÂ½Ç×ø±ê
towerLocation Coord <200, 200>, <140, 180>, <210, 275>,
					<210, 330>, <280, 330>, <420, 340>, 
					<340, 420>, <530, 400>

sodierSize	Coord <80, 60>
turretSize	Coord <80, 56>
arrowSize	Coord <80, 70>
magicSize	Coord <80, 70>
blankSize	Coord <70, 50>
signSize	Coord <46, 64>

ENDIF
