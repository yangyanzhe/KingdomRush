TITLE core.asm      
    .386         
    option casemap:none 

INCLUDE struct.inc
INCLUDE Irvine32.inc
INCLUDE core.inc
.data

MAP_WIDTH   =   700
MAP_HEIGHT  =   600
MAP_SIZE    =   MAP_WIDTH * MAP_HEIGHT
UP          =   0
DOWN        =   1
LEFT        =   2
RIGHT       =   3
MapFileName     db "data/map.data",0
Game            GameInfo <>
Game_Map        db MAP_SIZE dup(?)
fileHandle      HANDLE ?

.code
;==========================     Game      =============================
;----------------------------------------------------------------------     
LoadGameInfo PROC USES ecx
;
;载入游戏信息
;----------------------------------------------------------------------
    INVOKE  LoadGameMap
    mov     Game.Round_Num, 2
    mov     Game.Round, 0
    mov     Game.State, 0
    mov     Game.Player_Life, 20
    mov     Game.Player_Money, 220
    mov     Game.Start_Pos.x, 320
    mov     Game.Start_Pos.y, 0
    mov     Game.End_Pos.x, 699
    mov     Game.End_Pos.y, 400
    mov     Game.Tower_Num, 7
    mov     ecx, 7
    mov     ebx, OFFSET Game.TowerArray
    mov     esi, 0

;初始化所有塔
Initialize_Tower_Loop:  
    mov     (Tower PTR [ebx]).Tower_Type, 0     ;塔的初始类型为0（空地）
    mov     (Tower PTR [ebx]).Range, 100
    add     ebx, TYPE Tower
    loop    Initialize_Tower_Loop

    ret
LoadGameInfo ENDP

;----------------------------------------------------------------------   
StartGame PROC
;
;开始游戏
;----------------------------------------------------------------------
    mov     Game.State, 1
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
    sub     Game.Player_Money, eax
    ret
SellTower ENDP

;----------------------------------------------------------------------   
UpdateTower PROC USES esi,
    pTower: PTR Tower
;升级塔
;require: 特定塔的指针
;----------------------------------------------------------------------
    mov     esi, pTower
    inc     (Tower PTR [esi]).Degree
    ret
UpdateTower ENDP

;----------------------------------------------------------------------   
SearchAndAttack PROC USES eax ebx ecx esi edi edx,
    pTower: PTR Tower
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
    pEnemy: PTR Enemy
;使怪物进入地图，开始移动。
;require: 特定怪物的指针
;----------------------------------------------------------------------
    ;将特定怪物指针加入当前游戏怪物指针队列中
    mov     esi, OFFSET Game.pEnemyArray
    mov     ecx, Game.Enemy_Num
    dec     ecx
    mov     ebx, pEnemy
    mov     [esi + ecx * TYPE DWORD], ebx
    inc     Game.Enemy_Num

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
    mov     (Enemy PTR [esi]).Current_Dirt, ecx
    ret
ActivateEnemy ENDP

;----------------------------------------------------------------------   
EnemyMove PROC USES edi,
;移动怪物
;require: 特定怪物的指针
;----------------------------------------------------------------------
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

END