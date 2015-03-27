IFNDEF STRUCT_INT

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
    Tower_type			dd ?
    Degree				dd ?
    Cost				dd ?
    Sellcost			dd ?
    Attack				dd ?
    Range				dd ?
    Is_aoe				dd ?
    Aoe_range			dd ?
    Tower_position		Coord <>
Tower ends

;怪物
Enemy struct 
    Enemy_type			dd ?
    Speed				dd ?
    Start_pos			Coord <>
    End_pos				Coord <>
    Current_pos			Coord <>
    Current_dir			Coord <>
    Current_life		dd ?
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
    state				dd ?
    Player_life			dd ?
    Player_money		dd ?
    Round				dd ?                   ;当前Round编号
    Num_round			dd ?                   ;总轮次数
    Cur_tick			dd ?
    TowerArray			Tower 100 dup(<>)       ;当前所有塔
    EnemyArray			Enemy 100 dup(<>)		;当前所有怪物
    RoundArray			Round 20 dup(<>)		;游戏所有轮次
GameInfo ends

ENDIF