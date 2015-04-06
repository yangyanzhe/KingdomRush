IFNDEF STRUCT_INC
STRUCT_INC  equ <0>

location struct
    x dd ?
    y dd ?
    w dd ?
    h dd ?
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

;�ִ�
Round struct
    EnemyArray			Enemy 50 dup(<>)        ;���ֹ�������
    Enemy_Num			dd ?                    ;���ֹ�������
    Now_Enemy           dd ?                    ;���ֵ�ǰ�������ֵĹ�����
    Interval            dd ?                    ;�����������ʱ����
    Tick                dd ?                    ;����ר�ü�ʱ��
    Trigger_Tick		dd ?                    ;���ִ���ʱ��
Round ends

;��Ϸ��Ϣ
GameInfo struct
    Tick                dd ?                    ;��Ϸ��ʱ��
    State				dd ?
    Player_Life			dd ?
    Player_Money		dd ?
    Start_Pos           Coord <>
    End_Pos             Coord <>
    Now_Round	        dd ?                    ;��ǰRound���
    Next_Round          dd ?                    ;��һ��Round���
    Round_Num			dd ?                    ;���ִ���
    Cur_Tick			dd ?
    Tower_Num           dd ?
    Enemy_Num           dd ?
    TowerArray			Tower 100 dup(<>)       ;��ǰ������
    pEnemyArray			dd 100 dup(?)		    ;��ǰ���й����ָ��
    RoundArray			Round 20 dup(<>)		;��Ϸ�����ִ�
GameInfo ends

; ͼƬ��Ϣ
BitmapInfo struct
    bWidth              dd ?
    bHeight             dd ?
    bHandler            dd ?
BitmapInfo ends

; ����ͼƬ��Ϣ
MonsterBitmapInfo struct
	up 					BitmapInfo <>, <>
	left 				BitmapInfo <>, <>
	right 				BitmapInfo <>, <>
	down 				BitmapInfo <>, <>
	die					BitmapInfo <>, <>
MonsterBitmapInfo ends

ENDIF