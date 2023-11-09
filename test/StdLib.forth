: LINES2NUMBERS  DUP VARIABLE numarr numarr SWAP CELLS ALLOT VARIABLE arrstart VARIABLE nums 0 nums ! VARIABLE start VARIABLE stop SWAP DUP DUP start ! arrstart ! SWAP 1 + 0 DO arrstart @  I +  DUP DUP @ 0 = IF start @ - stop ! start @ stop @ NUMBER  nums @ numarr + ! 1 + start !  nums @ 1 + nums ! ELSE DROP THEN LOOP ;

S"test/numberfile.txt" R/W OPEN-FILE
12 1 READ-FILE
