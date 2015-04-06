IFNDEF  DATA_INC
DATA_INC    equ <1>

EXTERNDEF Game: GameInfo

.data
bgstart location <0, 0, 700, 600>
board   location <50, 50, 144, 142>
window  location <850, 150, 700, 640>
client  location <0, 0, 700, 600>

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

; �����½�����
towerLocation Coord <200, 200>, <140, 230>, <220, 335>,
					<210, 380>, <300, 390>, <420, 390>, 
					<340, 500>, <550, 480>
locationNum = 8

ENDIF