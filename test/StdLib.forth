: LINES2NUMBERS  DUP VARIABLE numarr numarr SWAP CELLS ALLOT VARIABLE arrstart VARIABLE nums 0 nums ! VARIABLE start VARIABLE stop SWAP DUP DUP start ! arrstart ! SWAP 1 + 0 DO arrstart @  I +  DUP DUP @ 0 = IF start @ - stop ! start @ stop @ NUMBER  nums @ numarr + ! 1 + start !  nums @ 1 + nums ! ELSE DROP THEN LOOP ;


: SPLITAONBLANKLINES DUP DUP VARIABLE numarrs numarrs SWAP CELLS ALLOT 0 numarrs   ! VARIABLE pntarr pntarr SWAP CELLS ALLOT numarrs pntarr ! VARIABLE nums 0  nums  ! VARIABLE arrs 1 arrs ! VARIABLE  arrstart VARIABLE startcurr VARIABLE stopcurr SWAP DUP DUP startcurr ! arrstart ! SWAP 1 + 0 DO I. arrstart @ I +  DUP startcurr @ - stopcurr !  DUP DUP  @ 0 = IF startcurr @ stopcurr @ NUMBER  nums @ numarrs + ! 1 + startcurr !  nums @ 1 + nums ! 1 + DUP DUP @ 0 = IF numarrs nums @ + arrs @ pntarr + ! 1 + startcurr ! arrs @ 1 + arrs ! 2 ELSE 1 THEN ELSE DROP DROP  1 THEN +LOOP startcurr @ stopcurr @ NUMBER  nums @ numarrs + ! numarrs nums @ + arrs @ pntarr + ! ;

: ARRSUM 0 SWAP 0 DO OVER I + @ + LOOP ;


: LSHIFT 0 DO 2 * LOOP ;

: RSHIFT 0 DO 2 / LOOP ;

: bin2ascii SWAP 1 LSHIFT + SWAP 2 LSHIFT + SWAP 3 LSHIFT + SWAP 4 LSHIFT + SWAP 5 LSHIFT + SWAP 6 LSHIFT + EMIT ;