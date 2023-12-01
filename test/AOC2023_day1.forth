

: NUMSINLINES  DUP VARIABLE numarrs numarrs SWAP CELLS ALLOT 0 numarrs  !  VARIABLE nums 0  nums  !  VARIABLE incurr 0 incurr ! VARIABLE curr curr 2 CELLS ALLOT  SWAP DUP VARIABLE arrstart arrstart ! SWAP 1 + 0 DO  arrstart @ I +  DUP   @ DUP DUP 47 > SWAP 58 < AND IF DUP curr incurr @  + ! curr 1  + ! 1 incurr ! ELSE arrstart @ I + @ 0 = IF curr 2 NUMBER numarrs nums @ + ! nums @ 1 + nums ! 0 incurr ! THEN THEN LOOP curr 2 NUMBER numarrs nums @ + ! nums @ 1 + nums ! ;

: ARRSUM 0 SWAP 0 DO OVER I + @ + LOOP ;



S"test/Day01.txt" R/W OPEN-FILE
50000 SWAP READ-FILE
NUMSINLINES
numarrs nums @
: ARRSUM 0 SWAP 0 DO OVER I + @ + LOOP ;
ARRSUM