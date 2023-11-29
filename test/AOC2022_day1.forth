

: SPLITAONBLANKLINES DUP DUP VARIABLE numarrs numarrs SWAP CELLS ALLOT 0 numarrs   ! VARIABLE pntarr pntarr SWAP CELLS ALLOT numarrs pntarr ! VARIABLE nums 0  nums  ! VARIABLE arrs 1 arrs ! VARIABLE  arrstart VARIABLE startcurr VARIABLE stopcurr SWAP DUP DUP startcurr ! arrstart ! SWAP 1 + 0 DO  arrstart @ I +  DUP startcurr @ - stopcurr !  DUP DUP  @ 0 = IF startcurr @ stopcurr @ NUMBER  nums @ numarrs + ! 1 + startcurr !  nums @ 1 + nums ! 1 + DUP DUP @ 0 = IF numarrs nums @ + arrs @ pntarr + ! 1 + startcurr ! arrs @ 1 + arrs ! 2 ELSE 1 THEN ELSE DROP DROP  1 THEN +LOOP startcurr @ stopcurr @ NUMBER  nums @ numarrs + ! numarrs nums @ + arrs @ pntarr + ! ;

: ARRSUM 0 SWAP 0 DO OVER I + @ + LOOP ;

: SUMARRS 0 SWAP 1 + BEGIN DUP 1 - @ OVER @ OVER - ARRSUM SWAP DROP ROT SWAP  1 + SWAP  1 + DUP @ 0 = UNTIL DROP ;

: max OVER SWAP DUP ROT > IF  DROP ELSE SWAP DROP THEN ;

: day1a 1 DO max LOOP ;

S"test/day1example.txt" R/W OPEN-FILE
50 SWAP READ-FILE
SPLITAONBLANKLINES
pntarr SUMARRS
: day1a 1 DO max LOOP ;
day1a
