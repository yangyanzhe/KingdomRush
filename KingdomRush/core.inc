IFNDEF CORE_INC
CORE_INC     equ <2>

INCLUDE     struct.inc

.data
EXTERNDEF Game: GameInfo


EACH_ROUND_ENEMY_TYPE dd 3 dup(0),
                         5 dup(0),
                         20 dup(0),
                         10 dup(0), 5 dup(1),
                         20 dup(1),
                         8 dup(1), 4 dup(2),     
                         1 dup(3)
ROUND_NUMBER dd 7
EACH_ROUND_ENEMY_NUMBER dd 3,  5,  20, 15, 20, 12, 1
APPEAR_INTERVAL         dd 30, 20, 8,  10, 10, 15, 30
ROUND_TRIGGER_TIME      dd 30, 200,400,600,800,1050,1250
ENEMY_LIFE dd 40, 80, 120, 200
ENEMY_MONEY dd 10, 20, 40, 200

TOWER_COST dd 0, 70, 70, 90, 110, 110, 110, 160, 220

Station_Num = 12
Station Coord <320, 175>, <291, 215>, <193, 233>, <165, 345>, <194, 398>, 
              <388, 407>, <491, 418>, <526, 406>, <546, 375>, <577, 340>, <674, 340>, <699, 340>,
              <699, 340>, <699, 340>, <699, 340>, <699, 340>

.code
LoadGameInfo PROTO

LoadGameMap PROTO

EnemyDie PROTO,
    _EnemyNumber:DWORD

CheckMovable PROTO,
    pPoint: DWORD,
    Dir: DWORD

UpdateEnemies PROTO

UpdateTowers PROTO

UpdateBullets PROTO

GetRound PROTO,
    _RoundNumber:DWORD

GetRoundEnemy PROTO,
    pRound: DWORD,
    _EnemyNumber: DWORD

ActivateEnemy PROTO,
    pEnemy: DWORD

EnemyMove PROTO,
    pEnemy: DWORD

EnemyCheckDie PROTO

DeleteEnemy PROTO,
    _EnemyNumber: DWORD

SearchAndAttack PROTO,
    pTower: DWORD

LoadGameInfo PROTO

UpdateTimer PROTO

GetNextStation PROTO,
    pEnemy: DWORD

CreateBullet PROTO,
    pTower: DWORD,
    pEnemy: DWORD

BulletMove PROTO,
    pBullet: DWORD

DeleteBullet PROTO,
    _BulletNumber: DWORD

InsertAnimate PROTO,
    px: DWORD,
    py: DWORD,
    _Type: DWORD

DeleteAnimate PROTO,
    _AnimateNumber: DWORD

UpdateAnimates PROTO

BulletMoveBoom PROTO

BulletMoveMagic PROTO

AOEAttack PROTO,
    pBullet: DWORD

AddMoney PROTO,
    Money: DWORD

CheckWinGame PROTO

CheckLoseGame PROTO

GetEnemyMoney PROTO,
    _number: DWORD

GetEnemyLife PROTO,
    _number: DWORD

GetTowerCost PROTO,
    _number: DWORD
ENDIF