IFNDEF STRUCT_INC
STRUCT_INC  equ <0>

; 点
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
    Enemy_Type			dd ?
    Speed				dd ?
    Start_Pos			Coord <>
    End_Pos				Coord <>
    Current_Pos			Coord <>
    Current_Dir 		dd ?
    Current_Life		dd ?
    Money				dd ?
    Gesture             dd ?
Enemy ends

;轮次
Round struct
    EnemyArray			Enemy 50 dup(<>)        ;本轮怪物序列
    Enemy_Num			dd ?                    ;本轮怪物数量
    Now_Enemy           dd ?                    ;本轮当前即将出现的怪物编号
    Interval            dd ?                    ;两个怪物出现时间间隔
    Tick                dd ?                    ;本轮专用计时器
    Trigger_Tick		dd ?                    ;本轮触发时间
    state               dd ?                    ;本轮状态
Round ends

;游戏信息
GameInfo struct
    Tick                dd ?                    ;游戏计时器
    State				dd ?
    Player_Life			dd ?
    Player_Money		dd ?
    Start_Pos           Coord <>
    End_Pos             Coord <>
    Now_Round	        dd ?                    ;当前Round编号
    Next_Round          dd ?                    ;下一轮Round编号
    Round_Num			dd ?                    ;总轮次数
    Cur_Tick			dd ?
    Tower_Num           dd ?
    Enemy_Num           dd ?
    TowerArray			Tower 100 dup(<>)       ;当前所有塔
    pEnemyArray			dd 100 dup(?)		    ;当前所有怪物的指针
    RoundArray			Round 20 dup(<>)		;游戏所有轮次
GameInfo ends

; 图片信息
BitmapInfo struct
    bWidth              dd ?
    bHeight             dd ?
    bHandler            dd ?
BitmapInfo ends

; 怪物图片信息
MonsterBitmapInfo struct
	up 					BitmapInfo <>, <>
	left 				BitmapInfo <>, <>
	right 				BitmapInfo <>, <>
	down 				BitmapInfo <>, <>
	die					BitmapInfo <>, <>
MonsterBitmapInfo ends

; 地图中空地位置信息
PositionSet struct
    number              dd 0
    position            Coord <>, <>, <>, <>, <>, <>, <>, <>, <>, <>,
                              <>, <>, <>, <>, <>, <>, <>, <>, <>, <>
PositionSet ends

ENDIF