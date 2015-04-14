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
    Total_Life          dd ?
    Money				dd ?
    Gesture             dd ?
    Station             dd ?
Enemy ends

;轮次
Round struct
    EnemyArray			Enemy 100 dup(<>)        ;本轮怪物序列
    Enemy_Num			dd ?                    ;本轮怪物数量
    Now_Enemy           dd ?                    ;本轮当前即将出现的怪物编号
    Interval            dd ?                    ;两个怪物出现时间间隔
    Tick                dd ?                    ;本轮专用计时器
    Trigger_Tick		dd ?                    ;本轮触发时间
    state               dd ?                    ;本轮状态
Round ends

Bullet struct
    Bullet_Type         dd ?
    Pos                 Coord <>
    Target              dd ?
    Gesture             dd ?
    Step                dd ?
    Attack              dd ?
Bullet ends

Animate struct
	Animate_Type		dd ?
	Pos					Coord <>
	Gesture				dd ?
Animate ends

;游戏信息
GameInfo struct
    Tick                dd ?                    ;游戏计时器
    TowerTick           dd ?                    ;塔攻击间隔计时器
    State				dd ?
    Player_Life			dd ?
    Player_Money		dd ?
    Start_Pos           Coord <>
    End_Pos             Coord <>
    Now_Round	        dd ?                    ;当前Round编号
    Next_Round          dd ?                    ;下一轮Round编号
    Round_Num			dd ?                    ;总轮次数                                           
    Tower_Num           dd ?                    ;塔的总数
    Enemy_Num           dd ?                    ;怪物总数
    Bullet_Num          dd ?                    ;子弹总数
    Animate_Num         dd ?                    ;动画总数
    AnimateArray        Animate 100 dup(<>)     ;动画队列
    BulletArray         Bullet 100 dup (<>)     ;子弹队列
    TowerArray			Tower 100 dup(<>)       ;塔的队列
    pEnemyArray			dd 100 dup(?)		    ;怪物指针队列
    RoundArray			Round 20 dup(<>)		;游戏所有轮次
    Station_Num         dd ?
    StationArray        Coord 20 dup(<>)
    ; 记录是否点击以及点击位置
    IsClicked           dd 0
    ClickedIndex        dd 0
    ; 记录State=0时(Instruction)的状态
    InstructionIndex    dd 0
    ButtonIndex         dd 0
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