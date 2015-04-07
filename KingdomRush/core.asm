TITLE core.asm      

.386
.model flat,stdcall 
option casemap:none 

INCLUDE Irvine32.inc
INCLUDE core.inc

.data
Game GameInfo <>

MAP_WIDTH   =   700
MAP_HEIGHT  =   600
MAP_SIZE    =   MAP_WIDTH * MAP_HEIGHT
UP          =   0
LEFT        =   1
RIGHT       =   2
DOWN        =   3
MapFileName     db "map.data",0
Game_Map        db MAP_SIZE dup(?)
fileHandle      HANDLE ?
DirectionX      dd 0, 1, 1, 0
DirectionY      dd 1, 0, 0, 1

.code
;==========================     Game      =============================
;----------------------------------------------------------------------     
UpdateTimer PROC
    inc     Game.Tick
    ;mov     eax, Game.Tick
    ;call    WriteDec
    ;call    Crlf
    ret
UpdateTimer ENDP

LoadGameInfo PROC USES ecx ebx esi edi eax edx
    LOCAL pEnemy_number: DWORD,
          pEnemy_type: DWORD,
          pRound_time: DWORD
;载入游戏信息
;----------------------------------------------------------------------
    INVOKE  LoadGameMap
    mov     Game.State, 0
    mov     Game.Player_Life, 20
    mov     Game.Player_Money, 220
    mov     Game.Start_Pos.x, 320
    mov     Game.Start_Pos.y, 0
    mov     Game.End_Pos.x, 699
    mov     Game.End_Pos.y, 400
    mov     Game.Station_Num, Station_Num

    mov     edx, OFFSET Station
    mov     ebx, OFFSET Game.StationArray
    mov     ecx, Station_Num
LoadStation_Loop:
    mov     eax, (Coord PTR [edx]).x
    mov     (Coord PTR [ebx]).x, eax
    mov     eax, (Coord PTR [edx]).y
    mov     (Coord PTR [ebx]).y, eax
    add     ebx, TYPE Coord
    add     edx, TYPE Coord
    loop    LoadStation_Loop
;初始化所有轮次信息
    mov     eax, ROUND_NUMBER
    mov     Game.Round_Num, eax
    mov     Game.Now_Round, 0
    mov     Game.Next_Round, 0
    mov     ebx, OFFSET EACH_ROUND_ENEMY_NUMBER
    mov     pEnemy_number, ebx
    mov     ebx, OFFSET EACH_ROUND_ENEMY_TYPE   
    mov     pEnemy_type, ebx
    mov     ebx, OFFSET ROUND_TRIGGER_TIME
    mov     pRound_time, ebx

    mov     esi, OFFSET Game.RoundArray         ;esi指向每局轮次数组  
    mov     ecx, eax
Initialize_Round_Loop:
    mov     (Round PTR [esi]).Interval, APPEAR_INTERVAL
    mov     (Round PTR [esi]).Now_Enemy, 0
    mov     (Round PTR [esi]).state, 0
    mov     ebx, pRound_time
    mov     eax, [ebx]
    mov     (Round PTR [esi]).Trigger_Tick, eax  ;设置每轮触发时间
    mov     ebx, pEnemy_number
    mov     eax, [ebx]
    mov     (Round PTR [esi]).Enemy_Num, eax    ;设置每轮怪物数量
    mov     eax, esi
    mov     edi, eax                            ;edi指向每轮怪物数组
    push    ecx
    mov     ecx, [ebx]
    ;初始化每轮怪物
    Initialize_Round_Enemy_Loop:
        mov     ebx, pEnemy_type    
        mov     eax, [ebx]
        mov     (Enemy PTR [edi]).Enemy_Type, eax

        mov     eax, ENEMY_LIFE_0
        mov     (Enemy PTR [edi]).Current_Life, eax

        mov     eax, ENEMY_MONEY_0
        mov     (Enemy PTR [edi]).Money, eax

        mov     (Enemy PTR [edi]).Gesture, 0
        mov     (Enemy PTR [edi]).Station, 0
        add     pEnemy_type, 4
        add     edi, TYPE Enemy
        loop    Initialize_Round_Enemy_Loop
    pop     ecx
    ;指针移动
    add     esi, TYPE Round
    add     pEnemy_number, TYPE DWORD
    add     pRound_time, TYPE DWORD
    loop    Initialize_Round_Loop

    ret
LoadGameInfo ENDP

;----------------------------------------------------------------------   
StartGame PROC
;
;开始游戏
;----------------------------------------------------------------------
    mov     Game.State, 1
    mov     Game.Tick, 0 ;计时器清零
    ret
StartGame ENDP

;----------------------------------------------------------------------     
ResetGame PROC      
;
;重置游戏
;----------------------------------------------------------------------
    ret
ResetGame ENDP

;----------------------------------------------------------------------     
QuitGame PROC      
;
;离开游戏
;----------------------------------------------------------------------
    ret
QuitGame ENDP

;----------------------------------------------------------------------     
PauseGame PROC      
;
;暂停游戏
;----------------------------------------------------------------------
    mov     Game.State, 2
    ret
PauseGame ENDP

;----------------------------------------------------------------------   
UpdateEnemies PROC
    LOCAL pRound:DWORD
; 更新所有怪物信息
;---------------------------------------------------------------------- 
    pushad

    ;判断是否触发下一轮
    mov eax, Game.Next_Round
    .IF eax >= Game.Round_Num
        jmp Jump1
    .ENDIF
    INVOKE GetRound, Game.Next_Round
    mov ebx, eax  
    mov eax, (Round PTR [ebx]).Trigger_Tick
    .IF Game.Tick >= eax
        .IF Game.Next_Round != 0
            inc Game.Now_Round
        .ENDIF
        inc Game.Next_Round
        mov (Round PTR [ebx]).state, 1
        mov (Round PTR [ebx]).Tick, 0 ;新一轮计时器清零
        mov eax, Game.Round_Num
    .ENDIF

Jump1:
    mov eax, Game.Now_Round
    .IF eax == 1
        mov eax, eax
    .ENDIF
    INVOKE GetRound, Game.Now_Round ;获取本轮句柄
    mov pRound, eax
    mov ebx, eax
    mov eax, (Round PTR [ebx]).state
    .IF eax == 0
        jmp UpdateEnemiesExit
    .ENDIF
    ;判断是否加入一只新的怪物
    mov eax, (Round PTR [ebx]).Now_Enemy
    mov edx, (Round PTR [ebx]).Enemy_Num
    .IF edx <= eax
        jmp Jump2
    .ENDIF
    inc (Round PTR [ebx]).Tick
    mov eax, (Round PTR [ebx]).Interval
    mov edx, (Round PTR [ebx]).Tick
    .IF edx == eax
        mov (Round PTR [ebx]).Tick, 0
        INVOKE GetRoundEnemy, ebx, (Round PTR [ebx]).Now_Enemy
        INVOKE ActivateEnemy, eax
        inc (Round PTR [ebx]).Now_Enemy
    .ENDIF

Jump2:
    ;移动场上所有怪物
    mov ebx, OFFSET Game.pEnemyArray
    mov ecx, Game.Enemy_Num
    .IF ecx == 0
        jmp UpdateEnemiesExit
    .ENDIF
Loop_EnemyMove:
    INVOKE EnemyMove, [ebx]
    INVOKE EnemyMove, [ebx]
    INVOKE EnemyMove, [ebx]
    INVOKE EnemyMove, [ebx]
    INVOKE EnemyMove, [ebx]
    INVOKE EnemyMove, [ebx]
    INVOKE EnemyMove, [ebx]
    INVOKE EnemyMove, [ebx]
    INVOKE EnemyMove, [ebx]
    add ebx, TYPE DWORD
    loop Loop_EnemyMove

UpdateEnemiesExit:
    popad
    ret
UpdateEnemies ENDP

;----------------------------------------------------------------------   
UpdateTowers PROC
; 更新所有塔的信息
;---------------------------------------------------------------------- 
    ;所有塔进行攻击
    pushad
    mov ebx, OFFSET Game.TowerArray
Loop_TowerAttack:
    INVOKE SearchAndAttack, ebx
    add ebx, TYPE Tower
    popad
    ret
UpdateTowers ENDP

;==========================     Tower     =============================
;----------------------------------------------------------------------   
CreateTower PROC USES esi ebx ecx,
    _Type:DWORD, _TowerNumber:DWORD
;
;建造塔
;----------------------------------------------------------------------
    mov     ebx, OFFSET Game.TowerArray
    mov     esi, _TowerNumber
    sub     esi, 1
    mov     ecx, esi
    cmp     ecx, 0
    je      create_tower
Loop_CreateTower:
    add     ebx, TYPE Tower
    loop    Loop_CreateTower

create_tower:
    mov     esi, _Type
    mov     (Tower PTR [ebx]).Tower_Type, esi
    mov     (Tower PTR [ebx]).Degree, 1
    ret
CreateTower ENDP

;----------------------------------------------------------------------   
SellTower PROC USES eax esi,
    pTower: PTR Tower
;卖塔
;require: 特定塔的指针
;----------------------------------------------------------------------
    mov     esi, pTower
    mov     eax, (Tower PTR [esi]).Sell_Cost
    add     Game.Player_Money, eax
    ret
SellTower ENDP

;----------------------------------------------------------------------   
UpDegreeTower PROC USES esi,
    pTower: PTR Tower
;升级塔
;require: 特定塔的指针
;----------------------------------------------------------------------
    mov     esi, pTower
    inc     (Tower PTR [esi]).Degree
    ret
UpDegreeTower ENDP

;----------------------------------------------------------------------   
SearchAndAttack PROC USES eax ebx ecx esi edi edx,
    pTower: DWORD
;
;搜索并攻击目标
; require: 特定塔的指针
;----------------------------------------------------------------------
    mov     esi, pTower
    mov     ecx, Game.Enemy_Num

    mov     edi, OFFSET Game.pEnemyArray
Search_Enemy_Loop:
    mov     ebx, [edi]
    mov     edx, (Enemy PTR [ebx]).Current_Pos.x
    mov     eax, (Tower PTR [esi]).Pos.x
    .IF     eax < edx
        xchg  eax, edx
    .ENDIF
    sub     eax, edx
    mov     edx, (Tower PTR [esi]).Range
    .IF     eax > edx
        jmp   SearchEnemy_Continue
    .ENDIF

    mov     edx, (Enemy PTR [ebx]).Current_Pos.y
    mov     eax, (Tower PTR [esi]).Pos.y
    .IF     eax < edx
        xchg  eax, edx
    .ENDIF
    sub     eax, edx
    mov     edx, (Tower PTR [esi]).Range
    .IF     eax > edx
        jmp   SearchEnemy_Continue
    .ENDIF

    ; 找到第一个可以进行攻击的怪物，攻击并退出过程
    mov     edx, (Tower PTR [esi]).Attack
    mov     eax, (Enemy PTR [ebx]).Current_Life
    .IF     eax < edx
        mov   (Enemy PTR [ebx]).Current_Life, 0
    .ELSE
        sub   (Enemy PTR [ebx]).Current_Life, edx
    .ENDIF
    jmp     SearchEnemy_Exit
SearchEnemy_Continue:
    mov     edi, TYPE DWORD
    loop    Search_Enemy_Loop
SearchEnemy_Exit:
    ret
SearchAndAttack ENDP

;==========================     Enemy     =============================
;----------------------------------------------------------------------   
ActivateEnemy PROC USES esi ecx ebx,
    pEnemy: DWORD
;使怪物进入地图，开始移动。
;require: 特定怪物的指针
;----------------------------------------------------------------------
    ;将特定怪物指针加入当前游戏怪物指针队列中
    mov     esi, OFFSET Game.pEnemyArray
    inc     Game.Enemy_Num
    mov     ecx, Game.Enemy_Num
    dec     ecx
    mov     ebx, pEnemy
    mov     [esi + ecx * TYPE DWORD], ebx

    ;初始化怪物的初始、当前、终点位置
    mov     esi, pEnemy
    mov     ecx, Game.Start_Pos.x
    mov     (Enemy PTR [esi]).Start_Pos.x, ecx
    mov     (Enemy PTR [esi]).Current_Pos.x, ecx
    mov     ecx, Game.Start_Pos.y
    mov     (Enemy PTR [esi]).Start_Pos.y, ecx
    mov     (Enemy PTR [esi]).Current_Pos.y, ecx
    mov     ecx, Game.End_Pos.x
    mov     (Enemy PTR [esi]).End_Pos.x, ecx
    mov     ecx, Game.End_Pos.y
    mov     (Enemy PTR [esi]).End_Pos.y, ecx
    ;初始化怪物朝向：向下
    mov     ecx, DOWN
    mov     (Enemy PTR [esi]).Current_Dir, ecx
    ret
ActivateEnemy ENDP

;----------------------------------------------------------------------   
EnemyMove PROC,
        pEnemy: DWORD
        LOCAL p_x: DWORD,  
              p_y: DWORD,
              e_x: DWORD,
              e_y: DWORD,
              choosed_dir: DWORD
;移动怪物
;寻路算法： 选择（一），选择沿着原先的移动方向进行移动。若移动失败，则：
;           选择（二），向与原先方向不同的两个方向移动。（如，原先往下走，则选择往左或右移动）
;                       选择方式：依据当前点和终点相对位置。若失败，则：
;           选择（三），选择与（二）相反的方向走。若失败, 则
;           选择（四），选择与原先方向相反的方向移动。
;require: 特定怪物的指针
;----------------------------------------------------------------------
    pushad
    mov     edi, pEnemy
    ;变更动作
    xor     (Enemy PTR [edi]).Gesture, 1
    mov     ebx, 0
    mov     ecx, (Enemy PTR [edi]).Current_Pos.y
    cmp     ecx, 0
    je      GETY_Done
GETY:
    add     ebx, MAP_WIDTH
    loop    GETY
GETY_Done:
    mov     ecx, (Enemy PTR [edi]).Current_Pos.x
    cmp     ecx, 0
    je      STEP1
GETX:
    add     ebx, 1
    loop    GETX
STEP1:
    mov     ecx, (Enemy PTR [edi]).Current_Dir
    mov     choosed_dir, ecx
    jmp     STEP2
    ;check if current direction is movable

    INVOKE  CheckMovable, ebx, ecx
    cmp     eax, 0
    je      STEP2
    mov     choosed_dir, ecx
    jmp     EnemyMove_Exit
STEP2:
    mov     eax, (Enemy PTR [edi]).Current_Pos.x
    mov     p_x, eax
    mov     eax, (Enemy PTR [edi]).Current_Pos.y
    mov     p_y, eax

    INVOKE  GetNextStation, edi
    mov     edx, eax

    mov     eax, (Coord PTR [edx]).x
    mov     e_x, eax
    mov     eax, (Coord PTR [edx]).y
    mov     e_y, eax

    mov     ecx, e_x
    mov     edx, p_x

    INVOKE CheckMovable, ebx, LEFT
    .IF ecx < edx && eax == 1
        mov choosed_dir, LEFT
        jmp EnemyMove_Exit
    .ENDIF

     INVOKE CheckMovable, ebx, RIGHT
    .IF ecx > edx && eax == 1
        mov choosed_dir, RIGHT
        jmp EnemyMove_Exit
    .ENDIF

    mov     ecx, e_y
    mov     edx, p_y

    INVOKE CheckMovable, ebx, UP
    .IF ecx < edx && eax == 1
        mov choosed_dir, UP
        jmp EnemyMove_Exit
    .ENDIF

     INVOKE CheckMovable, ebx, DOWN
    .IF ecx > edx && eax == 1
        mov choosed_dir, DOWN
        jmp EnemyMove_Exit
    .ENDIF
    add     (Enemy PTR [edi]).Station, 1
    mov choosed_dir, 4
    jmp EnemyMove_Exit


    .IF ecx == UP || ecx == DOWN
      mov     eax, e_x
      mov     edx, p_x
      .IF eax < edx
        mov     choosed_dir, LEFT
      .ELSE
        mov     choosed_dir, RIGHT
      .ENDIF 
    .ELSE
      mov     eax, e_y
      mov     edx, p_y
      .IF eax < edx
        mov     choosed_dir, UP
      .ELSE
        mov     choosed_dir, DOWN
      .ENDIF
    .ENDIF
    mov     edx, choosed_dir
    INVOKE  CheckMovable, ebx, edx
    cmp     eax, 0
    je      STEP3
    jmp     EnemyMove_Exit
STEP3:
    mov     eax, edx
    mov     edx, 3
    sub     edx, eax
    mov     choosed_dir, edx
    INVOKE  CheckMovable, ebx, edx
    cmp     eax, 0
    je      STEP4
    jmp     EnemyMove_Exit
STEP4:
    mov     eax, ecx
    mov     edx, 3
    sub     edx, eax
    mov     choosed_dir, edx
EnemyMove_Exit:    
    mov     eax, choosed_dir
    .IF eax == UP
      dec     (Enemy PTR [edi]).Current_Pos.y
      mov     (Enemy PTR [edi]).Current_Dir, UP
    .ELSEIF eax == LEFT
      dec     (Enemy PTR [edi]).Current_Pos.x
      mov     (Enemy PTR [edi]).Current_Dir, LEFT
    .ELSEIF eax == RIGHT
      inc     (Enemy PTR [edi]).Current_Pos.x
      mov     (Enemy PTR [edi]).Current_Dir, RIGHT
    .ELSEIF eax == DOWN
      inc     (Enemy PTR [edi]).Current_Pos.y
      mov     (Enemy PTR [edi]).Current_Dir, DOWN
    .ENDIF
    popad
    ret
EnemyMove ENDP

;----------------------------------------------------------------------   
EnemyCheckDie PROC USES eax ebx ecx edi edx,
;
;检查所有怪物的死亡情况，删去死亡的怪物
;----------------------------------------------------------------------
    mov     ebx, OFFSET Game.pEnemyArray
    mov     ecx, Game.Enemy_Num
    cmp     ecx, 0
    je      EnemyCheckDie_Exit
    mov     eax, 0
CheckAllDie:
    mov     edi, [ebx]
    mov     edx, (Enemy PTR [edi]).Current_Life
    .IF edx == 0
        INVOKE EnemyDie, eax
    .ENDIF
    add     ebx, TYPE DWORD
    loop    CheckAllDie
EnemyCheckDie_Exit:
    ret
EnemyCheckDie ENDP

;----------------------------------------------------------------------   
EnemyDie PROC USES ebx edi ecx eax,
    _EnemyNumber: DWORD
;
;怪物死亡，将怪物从队列中删除
;require:怪物在怪物队列中的编号（从0开始）
;----------------------------------------------------------------------
    mov     ebx, OFFSET Game.pEnemyArray
    mov     edi, _EnemyNumber
    shl     edi, 2
    add     ebx, edi
    mov     edi, Game.Enemy_Num
    mov     ecx, _EnemyNumber
    sub     edi, ecx
    mov     ecx, edi
    sub     ecx, 1
EnemyQueueMoveForward:
    mov     edi, [ebx+4]
    mov     [ebx], edi
    add     ebx, 4
    loop    EnemyQueueMoveForward
    ret
EnemyDie ENDP

;=========================== player ==================================
;----------------------------------------------------------------------     
AddMoney PROC USES eax,
    money:DWORD
;
;增加玩家金钱
;require: money
;----------------------------------------------------------------------
    mov     eax, money
    add     Game.Player_Money, eax
    ret
AddMoney ENDP

;----------------------------------------------------------------------     
SubMoney PROC USES eax,
    money:DWORD
;
;减少玩家金钱
;require: money
;----------------------------------------------------------------------
    mov     eax, money
    cmp     Game.Player_Money, eax
    ja      subFully
    mov     Game.Player_Money, 0
    jmp     ret_submoney
subFully:
    sub     Game.Player_Money, eax
    jmp     ret_submoney
ret_submoney:
    ret
SubMoney ENDP

;=========================== private ==================================
;----------------------------------------------------------------------     
LoadGameMap PROC USES eax ecx edx
;载入游戏地图
;----------------------------------------------------------------------
    mov     edx, OFFSET MapFileName
    call    OpenInputFile
    mov     fileHandle, eax

    mov     edx, OFFSET Game_Map
    mov     ecx, MAP_SIZE
    call    ReadFromFile
    mov     Game_Map[eax], 0
    call    WriteDec
    call    Crlf
    ret
LoadGameMap ENDP

;----------------------------------------------------------------------     
CheckMovable PROC USES edi ebx ecx,
    Pos: DWORD,
    Dir: DWORD
;判断某点是否可以移动
;require: 当前点坐标，想移动的方位
;----------------------------------------------------------------------
    mov     edi, Pos
    mov     ebx, Dir
    .IF ebx == UP
      .IF edi < MAP_WIDTH
        mov eax, 0
        ret
      .ENDIF
      sub   edi, MAP_WIDTH
    .ELSEIF ebx == DOWN
      add   edi, MAP_WIDTH
      .IF edi >= MAP_SIZE
        mov eax, 0
        ret
      .ENDIF
    .ELSEIF ebx == LEFT
      .IF edi < 1
        mov eax, 0
        ret
      .ENDIF
      sub   edi, 1
    .ELSE
      add   edi, 1
      .IF edi >= MAP_SIZE
        mov eax, 0
        ret
      .ENDIF
    .ENDIF
    mov ecx, 0
    mov cl, Game_Map[edi]
    .IF ecx == '1'
      mov   eax, 1
    .ELSE
      mov   eax, 0
    .ENDIF
    ret
CheckMovable ENDP

;----------------------------------------------------------------------     
GetRound PROC USES ecx ebx,
    _RoundNumber: DWORD
;获取轮次的句柄
;require: 轮次编号
;return: eax: Round句柄
;----------------------------------------------------------------------     
    mov eax, _RoundNumber
    mov ebx, OFFSET Game.RoundArray
    mov ecx, 0
GetRound_Loop:
    .IF ecx < eax
        inc ecx
        add ebx, TYPE Round
        jmp GetRound_Loop
    .ENDIF
    mov eax, ebx
    ret
GetRound ENDP

;----------------------------------------------------------------------
GetRoundEnemy PROC USES ebx esi ecx,
    pRound: DWORD,
    _EnemyNumber: DWORD
;获取某轮某怪的句柄
;require: 轮次句柄、怪物编号
;return: eax: 怪物句柄
;----------------------------------------------------------------------
    mov ebx, pRound
    mov ecx, _EnemyNumber
    .IF ecx == 0
        jmp GetRoundEnemy_Exit
    .ENDIF
GetRoundEnemy_Loop:
    add ebx, TYPE Enemy
    loop GetRoundEnemy_Loop
GetRoundEnemy_Exit:
    mov eax, ebx
    ret
GetRoundEnemy ENDP

;----------------------------------------------------------------------
GetNextStation PROC USES ebx ecx edx esi,
    pEnemy: DWORD
;获取怪物的下一个移动目标位置
;require: 怪物句柄
;return: eax: 坐标句柄
;----------------------------------------------------------------------
    mov ebx, pEnemy
    mov ecx, Game.Station_Num
    mov edx, OFFSET Game.StationArray
    mov eax, (Enemy PTR [ebx]).Station
    mov esi, 0
GetNexStation_Loop:
    .IF esi == eax
        jmp GetNextStation_Exit
    .ENDIF
    add edx, TYPE Coord
    add esi, 1
    loop GetNexStation_Loop
GetNextStation_Exit:
    mov eax, edx
    ret
GetNextStation ENDP

END
