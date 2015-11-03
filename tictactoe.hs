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

-- gives the string denoting the player, with color
toChar			:: Player -> [Char]
toChar X 		= "\ESC[32mX\ESC[0m"
toChar O 		= "\ESC[31mO\ESC[0m"
toChar Z 		= "-"

-- gives the char denoting the player just for if clauses
-- this is me giving up on pattern-matching and going straight to OOP
wut			:: Player -> Char
wut X 		= 'X'
wut O 		= 'O'
wut Z 		= 'Z'

-- clrscr
cls 			:: IO ()
cls 			= do
					putStr "\ESC[2J"
					goto 1 1

-- move the cursor
goto			:: Int -> Int -> IO ()
goto x y 		= putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

-- returns the positions of empty boxes
empties			::	[Player] -> [Integer]
empties []		= []
empties (Z:xs)	= [0] ++ map (1+) (empties xs)
empties (_:xs)	= map (1+) (empties xs)

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

-- begin stage, ask for whether going first
pick			:: IO ()
pick			= do
	cls
	goto 48 16
	putStr "From Ngoc Tran, with hate."
	goto 09 10
	putStrLn "Haskell Tic-Tac-Toe: proven tactics keeping you on your toes."
	goto 09 11
	putStrLn "Rules are simple. X go first. Move with WASD. You will never win."
	goto 09 12
	putStr "Now choose to go first or second (f/s): "

-- play
-- placeholder
play 			:: [Player] -> Int -> IO ()
play xs 0		= putStrLn "Hoomin go"
play xs 1		= putStrLn "AI first"

--	play 0

-- unnecessary stuff

-- test function
toStr			:: [Player] -> [Char]
toStr []		= []
toStr (x:xs)	= toChar x ++ toStr xs

-- main
main = do
	pick
	x <- getChar
	if x == 'f' || x == 'F' then play board 0
	else if x == 's' || x == 'S' then play board 1
	else main