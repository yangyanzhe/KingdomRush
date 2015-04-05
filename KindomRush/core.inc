LoadGameInfo PROTO

LoadGameMap PROTO

EnemyDie PROTO,
    _EnemyNumber:DWORD

CheckMovable PROTO,
    pPoint: DWORD,
    Dir: DWORD

UpdateEnemies PROTO

UpdateTowers PROTO

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

SearchAndAttack PROTO,
    pTower: DWORD

LoadGameInfo PROTO

UpdateTimer PROTO

TIMER_ID = 5
TIMER_INTERVAL = 1000