IFNDEF STRUCT_INC

location struct
    x DWORD ?
    y DWORD ?
    w DWORD ?
    h DWORD ?
location ends

Coord struct
    x dd ?
    y dd ?
Coord ends

;塔
Tower struct
    Tower_Type			dd ?
    Degree				dd ?
    Cost				dd ?
    Sell_Cost			dd ?
    Attack				dd ?
    Range				dd ?
    Is_aoe				dd ?
    Aoe_range			dd ?
    Pos		            Coord <>
Tower ends

;怪物
Enemy struct 
    Enemy_type			dd ?
    Speed				dd ?
    Start_Pos			Coord <>
    End_Pos				Coord <>
    Current_Pos			Coord <>
    Current_Dirt		dd ?
    Current_Life		dd ?
    Max_life			dd ?
    Money				dd ?
Enemy ends

;轮次
Round struct
    Trigger_tick		dd ?
    EnemyNumber			dd ?
    EnemyArray			Enemy 50 dup(<>)
Round ends

;游戏信息
GameInfo struct
    State				dd ?
    Player_Life			dd ?
    Player_Money		dd ?
    Start_Pos           Coord <>
    End_Pos             Coord <>
    Round				dd ?                    ;当前Round编号
    Round_Num			dd ?                    ;总轮次数
    Cur_Tick			dd ?
    Tower_Num           dd ?
    Enemy_Num           dd ?
    TowerArray			Tower 100 dup(<>)       ;当前所有塔
    pEnemyArray			dd 100 dup(?)		    ;当前所有怪物的指针
    RoundArray			Round 20 dup(<>)		;游戏所有轮次
GameInfo ends

ENDIF