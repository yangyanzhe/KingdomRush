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

;��
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

;����
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

;�ִ�
Round struct
    Trigger_tick		dd ?
    EnemyNumber			dd ?
    EnemyArray			Enemy 50 dup(<>)
Round ends

;��Ϸ��Ϣ
GameInfo struct
    State				dd ?
    Player_Life			dd ?
    Player_Money		dd ?
    Start_Pos           Coord <>
    End_Pos             Coord <>
    Round				dd ?                    ;��ǰRound���
    Round_Num			dd ?                    ;���ִ���
    Cur_Tick			dd ?
    Tower_Num           dd ?
    Enemy_Num           dd ?
    TowerArray			Tower 100 dup(<>)       ;��ǰ������
    pEnemyArray			dd 100 dup(?)		    ;��ǰ���й����ָ��
    RoundArray			Round 20 dup(<>)		;��Ϸ�����ִ�
GameInfo ends

ENDIF