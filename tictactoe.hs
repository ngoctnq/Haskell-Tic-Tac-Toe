{-
Haskell Tic-Tac-Toe
@author: Ngoc Tran (ngoc@underlandian.com)
-}

module Main
where

data Player	= X | O | Z

-- generate a new board
board			:: [Player]
board			= [y | (x, y) <- zip [1..9] (repeat Z)]

-- change a box
change			:: [Player] -> Player -> Int -> [Player]
change xs y n	= take n xs ++ [y] ++ drop (n+1) xs

-- switch player
switch			:: Player -> Player
switch X 		= O
switch O 		= X
switch Z 		= Z

-- gives the string denoting the player, with color
toChar			:: Player -> [Char]
toChar X 		= "\ESC[32mX\ESC[0m"
toChar O 		= "\ESC[31mO\ESC[0m"
toChar Z 		= " "

-- gives the char denoting the player just for if clauses
-- this is me giving up on pattern-matching and going straight to OOP
wut			:: Player -> Char
wut X 		= 'X'
wut O 		= 'O'
wut Z 		= 'Z'

-- comparing players with wut
cmp 		:: Player -> Player -> Bool
cmp x y 	= wut x == wut y

-- clrscr
cls 			:: IO ()
cls 			= do
					putStr "\ESC[2J"
					goto 1 1

-- move the cursor
goto			:: Int -> Int -> IO ()
goto x y 		= putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

-- fill empties with a certain player
fill			:: [Player] -> Player ->[Player]
fill []	_		= []
fill (Z:xs) y 	= y: (fill xs y)
fill (x:xs) y 	= x: (fill xs y)

-- check if the game can be won
canWin			:: [Player] -> Bool
canWin xs		= fst (won (fill xs X)) || fst (won (fill xs O))

-- check if someone won, then return who
won			:: [Player] -> (Bool, Player)
won [X, X, X, _, _, _, _, _, _] 	= (True, X)
won [_, _, _, X, X, X, _, _, _] 	= (True, X)
won [_, _, _, _, _, _, X, X, X] 	= (True, X)
won [X, _, _, X, _, _, X, _, _] 	= (True, X)
won [_, X, _, _, X, _, _, X, _] 	= (True, X)
won [_, _, X, _, _, X, _, _, X] 	= (True, X)
won [X, _, _, _, X, _, _, _, X] 	= (True, X)
won [_, _, X, _, X, _, X, _, _] 	= (True, X)
won [O, O, O, _, _, _, _, _, _] 	= (True, O)
won [_, _, _, O, O, O, _, _, _] 	= (True, O)
won [_, _, _, _, _, _, O, O, O] 	= (True, O)
won [O, _, _, O, _, _, O, _, _] 	= (True, O)
won [_, O, _, _, O, _, _, O, _] 	= (True, O)
won [_, _, O, _, _, O, _, _, O] 	= (True, O)
won [O, _, _, _, O, _, _, _, O] 	= (True, O)
won [_, _, O, _, O, _, O, _, _] 	= (True, O)
won _								= (False, Z)

-- check if game ended, then who won
ended			:: [Player] -> (Bool, Player)
ended xs		= if (fst (won xs)) then (won xs)
					else if (canWin xs) then (False, Z)
						else (True, Z)

-- highlight the box with given coordinates
highlight_		:: Int -> Int -> IO ()
highlight_ x y 	= do
					putStr "\ESC[34m"
					goto x y
					putStr "+---+"
					goto x (y + 1)
					putStr "|"
					goto (x + 4) (y + 1)
					putStr "|"
					goto x (y + 2)
					putStr "+---+"					
					putStr "\ESC[0m"

-- highlight the box being selected
highlight		:: Int -> IO ()
highlight 0		= highlight_ 37 09
highlight 1		= highlight_ 41 09
highlight 2		= highlight_ 45 09
highlight 3		= highlight_ 37 11
highlight 4		= highlight_ 41 11
highlight 5		= highlight_ 45 11
highlight 6		= highlight_ 37 13
highlight 7		= highlight_ 41 13
highlight 8		= highlight_ 45 13

-- highlight the winning box
highlightwon_	:: Int -> Int -> Player -> IO ()
highlightwon_ x y X = do
					putStr "\ESC[32m"
					goto x y
					putStr "+---+"
					goto x (y + 1)
					putStr "|"
					goto (x + 4) (y + 1)
					putStr "|"
					goto x (y + 2)
					putStr "+---+"					
					putStr "\ESC[0m"
-- just in case the hoomin feeds on purpose
highlightwon_ x y O = do
					putStr "\ESC[31m"
					goto x y
					putStr "+---+"
					goto x (y + 1)
					putStr "|"
					goto (x + 4) (y + 1)
					putStr "|"
					goto x (y + 2)
					putStr "+---+"					
					putStr "\ESC[0m"

-- find the winning boxes with the given winning pattern
highlight_m		:: Int -> Player -> IO ()
highlight_m 0 x	= highlightwon_ 37 09 x
highlight_m 1 x	= highlightwon_ 41 09 x
highlight_m 2 x	= highlightwon_ 45 09 x
highlight_m 3 x	= highlightwon_ 37 11 x
highlight_m 4 x	= highlightwon_ 41 11 x
highlight_m 5 x	= highlightwon_ 45 11 x
highlight_m 6 x	= highlightwon_ 37 13 x
highlight_m 7 x	= highlightwon_ 41 13 x
highlight_m 8 x	= highlightwon_ 45 13 x

-- highlight if someone actually won (i.e. the machine)
highlightwon	:: [Player] -> IO ()
highlightwon [X, X, X, _, _, _, _, _, _] 	= do
	highlight_m 0 X
	highlight_m 1 X
	highlight_m 2 X
highlightwon [_, _, _, X, X, X, _, _, _] 	= do
	highlight_m 4 X
	highlight_m 5 X
	highlight_m 3 X
highlightwon [_, _, _, _, _, _, X, X, X] 	= do
	highlight_m 6 X
	highlight_m 7 X
	highlight_m 8 X
highlightwon [X, _, _, X, _, _, X, _, _] 	= do
	highlight_m 0 X
	highlight_m 3 X
	highlight_m 6 X
highlightwon [_, X, _, _, X, _, _, X, _] 	= do
	highlight_m 1 X
	highlight_m 4 X
	highlight_m 7 X
highlightwon [_, _, X, _, _, X, _, _, X] 	= do
	highlight_m 2 X
	highlight_m 5 X
	highlight_m 8 X
highlightwon [X, _, _, _, X, _, _, _, X] 	= do
	highlight_m 0 X
	highlight_m 4 X
	highlight_m 8 X
highlightwon [_, _, X, _, X, _, X, _, _] 	= do
	highlight_m 2 X
	highlight_m 4 X
	highlight_m 6 X
highlightwon [O, O, O, _, _, _, _, _, _] 	= do
	highlight_m 0 O
	highlight_m 1 O
	highlight_m 2 O
highlightwon [_, _, _, O, O, O, _, _, _] 	= do
	highlight_m 4 O
	highlight_m 5 O
	highlight_m 3 O
highlightwon [_, _, _, _, _, _, O, O, O] 	= do
	highlight_m 6 O
	highlight_m 7 O
	highlight_m 8 O
highlightwon [O, _, _, O, _, _, O, _, _] 	= do
	highlight_m 0 O
	highlight_m 3 O
	highlight_m 6 O
highlightwon [_, O, _, _, O, _, _, O, _] 	= do
	highlight_m 1 O
	highlight_m 4 O
	highlight_m 7 O
highlightwon [_, _, O, _, _, O, _, _, O] 	= do
	highlight_m 2 O
	highlight_m 5 O
	highlight_m 8 O
highlightwon [O, _, _, _, O, _, _, _, O] 	= do
	highlight_m 0 O
	highlight_m 4 O
	highlight_m 8 O
highlightwon [_, _, O, _, O, _, O, _, _] 	= do
	highlight_m 2 O
	highlight_m 4 O
	highlight_m 6 O
highlightwon _								= return ()

-- print out the current board
display				:: [Player] -> (Bool, Player) -> Int -> IO ()
display xs (b, p) i = do
						cls
						goto 37 09
						putStr "+---+---+---+"
						goto 37 10
						putStr "|   |   |   |"
						goto 37 11
						putStr "+---+---+---+"
						goto 37 12
						putStr "|   |   |   |"
						goto 37 13
						putStr "+---+---+---+"
						goto 37 14
						putStr "|   |   |   |"
						goto 37 15
						putStr "+---+---+---+"
						goto 39 10
						putStr (toChar (xs !! 0))
						goto 43 10
						putStr (toChar (xs !! 1))
						goto 47 10
						putStr (toChar (xs !! 2))
						goto 39 12
						putStr (toChar (xs !! 3))
						goto 43 12
						putStr (toChar (xs !! 4))
						goto 47 12
						putStr (toChar (xs !! 5))
						goto 39 14
						putStr (toChar (xs !! 6))
						goto 43 14
						putStr (toChar (xs !! 7))
						goto 47 14
						putStr (toChar (xs !! 8))
						goto 01 25
						if b then do
							if t == 'Z' then putStr "Game ended, no one won. Nice try though."
							else do
								putStr "You lost. Told you."
								-- regardless of X or O, the human WILL NOT WIN.
								highlightwon xs
							goto 01 01
							x <- getChar
							cls
						else do
							putStr "Your turn."
							highlight i
						where t = wut p

-- pattern of winning
pattern 		:: Int -> [Int]
pattern 1		= [0,1,2]
pattern 2		= [3,4,5]
pattern 3		= [6,7,8]
pattern 4		= [0,3,6]
pattern 5		= [1,4,7]
pattern 6		= [2,5,8]
pattern 7		= [0,4,8]
pattern 8		= [2,4,6]

-- relevant patterns of a move
pttrns 			:: Int -> [Int]
pttrns 0		= [1,4,7]
pttrns 1		= [1,5]
pttrns 2		= [1,6,8]
pttrns 3		= [2,4]
pttrns 4		= [2,5,8]
pttrns 5		= [2,6]
pttrns 6		= [3,4,8]
pttrns 7		= [3,5]
pttrns 8		= [3,6,7]

-- count the weight of each winning pattern
-- normally winnable = 1
-- two-filled = 3
-- draw = 0
-- normally losable = -1 // not implemented, wait to see if works without
-- two-filled = -3
check		 	:: Player -> Player -> Player -> Player -> Int
check p p p p 	= 999
check p p2 p2 p2= -999
check p p p Z 	= 3
check p p Z p 	= 3
check p Z p p 	= 3
check p p2 p2 Z = -3
check p p2 Z p2 = -3
check p Z p2 p2 = -3
check p p p2 _	= 0
check p p _ p2	= 0
check p _ p p2 	= 0
check p p2 p _	= 0
check p p2 _ p	= 0
check p _ p2 p 	= 0
check p p Z Z 	= 1
check p Z p Z 	= 1
check p Z Z p 	= 1
check _ _ _ _	= 0
	where p2 = switch p

-- board assessment for bot, calc weight of choice
weight			:: [Player] -> Player -> Int -> Int
weight xs p i	= if not (wut (xs !! i) == 'Z') then -9999	-- invalid choice
	else sum [check p (n !! x) (n !! y) (n !! z) | [x, y, z] <- map pttrns [1..8]]
	where 	z = switch p;
			n = change xs p i

-- begin stage, ask for whether going first
pick			:: IO ()
pick			= do
	cls
	goto 48 15
	putStr "From Ngoc Tran, with hate."
	goto 09 10
	putStrLn "Haskell Tic-Tac-Toe: proven tactics keeping you on your toes."
	goto 09 11
	putStrLn "Rules are simple. X go first. Move with WASD. You will never win."
	goto 09 12
	putStr "Now choose to go first or second (f/s): "

-- play
-- placeholder
play 			:: [Player] -> Player -> Int -> Int -> IO ()
play xs p i 0	= do
	display xs t i
	goto 01 01
	x <- getChar
	if x == 'w' || x == 'W' then
		if i < 3 then play xs p (i + 6) 0
		else play xs p (i - 3) 0
	else if x == 's' || x == 'S' then
		if i > 5 then play xs p (i - 6) 0
		else play xs p (i + 3) 0
	else if x == 'a' || x == 'A' then
		if i `mod` 3 == 0 then play xs p (i + 2) 0
		else play xs p (i - 1) 0
	else if x == 'd' || x == 'D' then
		if i `mod` 3 == 2 then play xs p (i - 2) 0
		else play xs p (i + 1) 0
	else if x == '\n' then
		if wut (xs !! i) == 'Z' then
			if b then return()
			else play n (switch p) i 1
		else play xs p i 0
	else play xs p i 0
	where 	n = change xs p i;	-- n for new board, not natural number
			t = ended xs;		-- t for temp pair, not time
			b = fst t 			-- b for boolean, nothing strange
play xs p i 1	= do 
	-- AI ALGORITHM HERE
	display xs t i
	goto 01 01
	if b then return()
	-- CHANGE XS
	else play xs (switch p) i 0
	where 	t = ended xs;
			b = fst t

-- test function: print out the board, only for debugging purposes
toStr			:: [Player] -> [Char]
toStr []		= []
toStr (x:xs)	= toChar x ++ toStr xs

-- main
main = do
	pick
	x <- getChar
	if x == 'f' || x == 'F' then play board X 0 0
	else if x == 's' || x == 'S' then play board X 0 1
	else main