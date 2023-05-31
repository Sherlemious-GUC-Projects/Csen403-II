type Location = (Char, Int)

data Player = White | Black deriving (Show, Eq)
data Piece = P Location | N Location | K Location | Q Location | R Location
  | B Location deriving (Show, Eq)
type Board = (Player, [Piece], [Piece])

printPiecew :: Board -> [Piece]
printPiecew (player, whitePieces, blackPieces) = whitePieces

printPieceb :: Board -> [Piece]
printPieceb (player, whitePieces, blackPieces) = blackPieces

setBoard :: Board
setBoard = (White, [P ('a', 2), P ('b', 2), P ('c', 2), P ('d', 2), P ('e', 2),
  P ('f', 2), P ('g', 2), P ('h', 2), R ('a', 1), R ('h', 1), N ('b', 1),
  N ('g', 1), B ('c', 1), B ('f', 1), Q ('d', 1), K ('e', 1)], [P ('a', 7),
  P ('b', 7), P ('c', 7), P ('d', 7), P ('e', 7), P ('f', 7), P ('g', 7),
  P ('h', 7), R ('a', 8), R ('h', 8), N ('b', 8), N ('g', 8), B ('c', 8),
  B ('f', 8), Q ('d', 8), K ('e', 8)])


--     a	   b    c    d    e    f    g    h     
-- 8 | RB | NB | BB | QB | KB | BB | NB | RB |
-- 7 | PB | PB | PB | PB | PB | PB | PB | PB |
-- 6 |    |    |    |    |    |    |    |    |
-- 5 |    |    |    |    |    |    |    |    |
-- 4 |    |    |    |    |    |    |    |    |
-- 3 |    |    |    |    |    |    |    |    |
-- 2 | PW | PW | PW | PW | PW | PW | PW | PW |
-- 1 | RW | NW | BW | QW | KW | BW | NW | RW |
--
--Turn: White

visualizeBoard:: Board->String
visualizeBoard (player, whitePieces, blackPieces) = (visualizeBoard' whitePieces blackPieces ('a',8) ("    a	   b    c    d    e    f    g    h  \n8 | ") ++ "\n\nTurn: " ++ show(player) )

visualizeBoard' :: [Piece] -> [Piece] -> Location -> String -> String
visualizeBoard' whitePieces blackPieces (a,y) str
  |a == 'm' && y == 1 = str
  |a == 'm' = visualizeBoard' whitePieces blackPieces ('a',y-1) (str ++ "\n" ++ show(y-1) ++ " | ")
  |(elem (P (a, y)) whitePieces) = visualizeBoard' whitePieces blackPieces (a',y) (str ++ "PW" ++ " | ")
  |(elem (P (a, y)) blackPieces) = visualizeBoard' whitePieces blackPieces (a',y) (str ++ "PB" ++ " | ")
  |(elem (N (a, y)) whitePieces) = visualizeBoard' whitePieces blackPieces (a',y) (str ++ "NW" ++ " | ")
  |(elem (N (a, y)) blackPieces) = visualizeBoard' whitePieces blackPieces (a',y) (str ++ "NB" ++ " | ")
  |(elem (B (a, y)) whitePieces) = visualizeBoard' whitePieces blackPieces (a',y) (str ++ "BW" ++ " | ")
  |(elem (B (a, y)) blackPieces) = visualizeBoard' whitePieces blackPieces (a',y) (str ++ "BB" ++ " | ")
  |(elem (R (a, y)) whitePieces) = visualizeBoard' whitePieces blackPieces (a',y) (str ++ "RW" ++ " | ")
  |(elem (R (a, y)) blackPieces) = visualizeBoard' whitePieces blackPieces (a',y) (str ++ "RB" ++ " | ")
  |(elem (Q (a, y)) whitePieces) = visualizeBoard' whitePieces blackPieces (a',y) (str ++ "QW" ++ " | ")
  |(elem (Q (a, y)) blackPieces) = visualizeBoard' whitePieces blackPieces (a',y) (str ++ "QB" ++ " | ")
  |(elem (K (a, y)) whitePieces) = visualizeBoard' whitePieces blackPieces (a',y) (str ++ "KW" ++ " | ")
  |(elem (K (a, y)) blackPieces) = visualizeBoard' whitePieces blackPieces (a',y) (str ++ "KB" ++ " | ")
  |otherwise = visualizeBoard' whitePieces blackPieces (a',y) (str ++ "   | ")
  where
    x = convertFromCharToInt a
    a' = convertFromIntToChar (x + 1)



visEmptyBoard = [[" "| x <- [1..8]] | y <- [1..8]]

convertFromCharToInt :: Char -> Int
convertFromCharToInt x = case x of
  'a' -> 1
  'b' -> 2
  'c' -> 3
  'd' -> 4
  'e' -> 5
  'f' -> 6
  'g' -> 7
  'h' -> 8
  'm' -> 9

convertFromIntToChar :: Int -> Char
convertFromIntToChar x = case x of
  1 -> 'a'
  2 -> 'b'
  3 -> 'c'
  4 -> 'd'
  5 -> 'e'
  6 -> 'f'
  7 -> 'g'
  8 -> 'h'
  9 -> 'm'

isThere :: [Piece] -> [Piece] -> Location -> Bool
isThere whitePieces blackPieces (a', y')
  |isTherew whitePieces  (a', y') && isThereb blackPieces (a', y') = True
  |otherwise =False

isTherew :: [Piece] -> Location -> Bool
isTherew whitePieces  (a', y')
  |not (elem (P (a', y')) whitePieces)  && not (elem (N (a', y')) whitePieces) && not (elem (B (a', y')) whitePieces) && not (elem (R (a', y')) whitePieces) && not (elem (Q (a', y')) whitePieces) && not (elem (K (a', y')) whitePieces) = True
  |otherwise =False

isThereb :: [Piece] -> Location -> Bool
isThereb blackPieces (a', y')
  |not (elem (P (a', y')) blackPieces)  && not (elem (N (a', y')) blackPieces) && not (elem (B (a', y')) blackPieces) && not (elem (R (a', y')) blackPieces) && not (elem (Q (a', y')) blackPieces) && not (elem (K (a', y')) blackPieces) = True
  |otherwise =False

isLegal:: Piece -> Board -> Location -> Bool
-- pawn
isLegal (P (a, y)) (player, whitePieces, blackPieces) (a', y')
  | x' == x && y' == y = False
  | x'> 8 || x' < 1 || y' > 8 || y' < 1 = False
  | (elem (P (a, y)) whitePieces) && y' == y + 1 && x' == x && isThere whitePieces blackPieces (a', y')= True
  | (elem (P (a, y)) blackPieces) && y' == y - 1 && x' == x && isThere whitePieces blackPieces (a', y')= True
  | (elem (P (a, y)) whitePieces) && y == 2 && y' == y + 2 && x' == x && not (elem (P (a', y')) whitePieces) && not (elem (P (a', y')) blackPieces) && not (elem (P (a', y' - 1)) whitePieces) && not (elem (P (a', y' - 1)) blackPieces) && not (elem (N (a', y')) whitePieces) && not (elem (N (a', y')) blackPieces) && not (elem (N (a', y' - 1)) whitePieces) && not (elem (N (a', y' - 1)) blackPieces)&& not (elem (B (a', y')) whitePieces) && not (elem (B (a', y')) blackPieces) && not (elem (B (a', y' - 1)) whitePieces) && not (elem (B (a', y' - 1)) blackPieces)&& not (elem (R (a', y')) whitePieces) && not (elem (R (a', y')) blackPieces) && not (elem (R (a', y' - 1)) whitePieces) && not (elem (R (a', y' - 1)) blackPieces)&& not (elem (Q (a', y')) whitePieces) && not (elem (Q (a', y')) blackPieces) && not (elem (Q (a', y' - 1)) whitePieces) && not (elem (Q (a', y' - 1)) blackPieces)&& not (elem (K (a', y')) whitePieces) && not (elem (K (a', y')) blackPieces) && not (elem (K (a', y' - 1)) whitePieces) && not (elem (K (a', y' - 1)) blackPieces)  = True
  | (elem (P (a, y)) blackPieces) && y == 7 && y' == y - 2 && x' == x && not (elem (P (a', y')) whitePieces) && not (elem (P (a', y')) blackPieces) && not (elem (P (a', y' + 1)) whitePieces) && not (elem (P (a', y' + 1)) blackPieces) && not (elem (N (a', y')) whitePieces) && not (elem (N (a', y')) blackPieces) && not (elem (N (a', y' + 1)) whitePieces) && not (elem (N (a', y' + 1)) blackPieces)&& not (elem (B (a', y')) whitePieces) && not (elem (B (a', y')) blackPieces) && not (elem (B (a', y' + 1)) whitePieces) && not (elem (B (a', y' + 1)) blackPieces)&& not (elem (R (a', y')) whitePieces) && not (elem (R (a', y')) blackPieces) && not (elem (R (a', y' + 1)) whitePieces) && not (elem (R (a', y' + 1)) blackPieces)&& not (elem (Q (a', y')) whitePieces) && not (elem (Q (a', y')) blackPieces) && not (elem (Q (a', y' + 1)) whitePieces) && not (elem (Q (a', y' + 1)) blackPieces)&& not (elem (K (a', y')) whitePieces) && not (elem (K (a', y')) blackPieces) && not (elem (K (a', y' + 1)) whitePieces) && not (elem (K (a', y' + 1)) blackPieces)  = True
  | (elem (P (a, y)) whitePieces) && y' == y + 1 && x' == x + 1 && elem (P (a', y')) blackPieces = True
  | (elem (P (a, y)) whitePieces) && y' == y + 1 && x' == x - 1 && elem (P (a', y')) blackPieces = True
  | (elem (P (a, y)) blackPieces) && y' == y - 1 && x' == x + 1 && elem (P (a', y')) whitePieces = True
  | (elem (P (a, y)) blackPieces) && y' == y - 1 && x' == x - 1 && elem (P (a', y')) whitePieces = True
  | otherwise = False
  where
    x = convertFromCharToInt a
    x' = convertFromCharToInt a'
--Knight
isLegal (N (a, y)) (player, whitePieces, blackPieces) (a', y')
  | x' == x && y' == y = False
  | x'> 8 || x' < 1 || y' > 8 || y' < 1 = False
  | (elem (N (a, y)) blackPieces) && y' == y + 2 && ( x' == x+1 || x' == x-1) && isThereb blackPieces (a', y') = True
  | (elem (N (a, y)) blackPieces) && y' == y - 2 && ( x' == x+1 || x' == x-1) && isThereb blackPieces (a', y') = True
  | (elem (N (a, y)) blackPieces) && x' == x + 2 && ( y' == y+1 || y' == y-1) && isThereb blackPieces (a', y') = True
  | (elem (N (a, y)) blackPieces) && x' == x - 2 && ( y' == y+1 || y' == y-1) && isThereb blackPieces (a', y') = True
  | (elem (N (a, y)) whitePieces) && y' == y + 2 && ( x' == x+1 || x' == x-1) && isTherew whitePieces (a', y') = True
  | (elem (N (a, y)) whitePieces) && y' == y - 2 && ( x' == x+1 || x' == x-1) && isTherew whitePieces (a', y') = True
  | (elem (N (a, y)) whitePieces) && x' == x + 2 && ( y' == y+1 || y' == y-1) && isTherew whitePieces (a', y') = True
  | (elem (N (a, y)) whitePieces) && x' == x - 2 && ( y' == y+1 || y' == y-1) && isTherew whitePieces (a', y') = True
  | otherwise = False
  where
    x = convertFromCharToInt a
    x' = convertFromCharToInt a'

--Bishop
isLegal (B (a, y)) (player, whitePieces, blackPieces) (a', y')
  | x' == x || y' == y = False
  | x'> 8 || x' < 1 || y' > 8 || y' < 1 = False
  | (elem (B (a, y)) blackPieces) && ((y - y' ) `div` ( x - x') == 1 || (y - y' ) `div` ( x - x') == -1 ) && isThereb blackPieces (a', y') = isLegal'' (B (a, y)) (player, whitePieces, blackPieces) (a', y')
  | (elem (B (a, y)) whitePieces) && ((y - y' ) `div` ( x - x') == 1 || (y - y' ) `div` ( x - x') == -1 ) && isTherew whitePieces (a', y') = isLegal'' (B (a, y)) (player, whitePieces, blackPieces) (a', y')
  | otherwise = False
  where
    x = convertFromCharToInt a
    x' = convertFromCharToInt a'

--Rook
isLegal (R (a, y)) (player, whitePieces, blackPieces) (a', y')
  | x' == x && y' == y = False
  | x'> 8 || x' < 1 || y' > 8 || y' < 1 = False
  | (elem (R (a, y)) blackPieces) && (x' == x || y' == y) && isThereb blackPieces (a', y') = isLegal'' (R (a, y)) (player, whitePieces, blackPieces) (a', y')
  | (elem (R (a, y)) whitePieces) && (x' == x || y' == y) && isTherew whitePieces (a', y') = isLegal'' (R (a, y)) (player, whitePieces, blackPieces) (a', y')
  | otherwise = False
  where
    x = convertFromCharToInt a
    x' = convertFromCharToInt a'

--Queen
isLegal (Q (a, y)) (player, whitePieces, blackPieces) (a', y') 
  | x' == x && y' == y = False
  | (elem (Q (a, y)) blackPieces) && (x' == x || y' == y) && isThereb blackPieces (a', y') = isLegal'' (R (a, y)) (player, whitePieces, blackPieces) (a', y')
  | (elem (Q (a, y)) whitePieces) && (x' == x || y' == y) && isTherew whitePieces (a', y') = isLegal'' (R (a, y)) (player, whitePieces, blackPieces) (a', y')
  |(x' /= x && y' /= y ) && (elem (Q (a, y)) blackPieces) && ((y - y' ) `div` ( x - x') == 1 || (y - y' ) `div` ( x - x') == -1 ) && isThereb blackPieces (a', y') = isLegal'' (B (a, y)) (player, whitePieces, blackPieces) (a', y')
  |(x' /= x && y' /= y ) && (elem (Q (a, y)) whitePieces) && ((y - y' ) `div` ( x - x') == 1 || (y - y' ) `div` ( x - x') == -1 ) && isTherew whitePieces (a', y') = isLegal'' (B (a, y)) (player, whitePieces, blackPieces) (a', y')
  | x'> 8 || x' < 1 || y' > 8 || y' < 1 = False
  | otherwise = False
  where
    x = convertFromCharToInt a
    x' = convertFromCharToInt a'

--King
isLegal (K (a, y)) (player, whitePieces, blackPieces) (a', y')
  | x' == x && y' == y = False
  | x'> 8 || x' < 1 || y' > 8 || y' < 1 = False
  | (elem (P (a, y)) blackPieces) && y' == y + 1 && x' == x + 1 && isThereb blackPieces (a', y') = True
  | (elem (P (a, y)) blackPieces) && y' == y + 1 && x' == x - 1 && isThereb blackPieces (a', y') = True
  | (elem (P (a, y)) blackPieces) && y' == y - 1 && x' == x + 1 && isThereb blackPieces (a', y') = True
  | (elem (P (a, y)) blackPieces) && y' == y - 1 && x' == x - 1 && isThereb blackPieces (a', y') = True
  | (elem (P (a, y)) blackPieces) && y' == y + 1 && x' == x     && isThereb blackPieces (a', y') = True
  | (elem (P (a, y)) blackPieces) && y' == y - 1 && x' == x     && isThereb blackPieces (a', y') = True
  | (elem (P (a, y)) blackPieces) && y' == y     && x' == x + 1 && isThereb blackPieces (a', y') = True
  | (elem (P (a, y)) blackPieces) && y' == y     && x' == x - 1 && isThereb blackPieces (a', y') = True
  | (elem (P (a, y)) whitePieces) && y' == y + 1 && x' == x + 1 && isTherew whitePieces (a', y') = True
  | (elem (P (a, y)) whitePieces) && y' == y + 1 && x' == x - 1 && isTherew whitePieces (a', y') = True
  | (elem (P (a, y)) whitePieces) && y' == y - 1 && x' == x + 1 && isTherew whitePieces (a', y') = True
  | (elem (P (a, y)) whitePieces) && y' == y - 1 && x' == x - 1 && isTherew whitePieces (a', y') = True
  | (elem (P (a, y)) whitePieces) && y' == y + 1 && x' == x     && isTherew whitePieces (a', y') = True
  | (elem (P (a, y)) whitePieces) && y' == y - 1 && x' == x     && isTherew whitePieces (a', y') = True
  | (elem (P (a, y)) whitePieces) && y' == y     && x' == x + 1 && isTherew whitePieces (a', y') = True
  | (elem (P (a, y)) whitePieces) && y' == y     && x' == x - 1 && isTherew whitePieces (a', y') = True
  | otherwise = False
  where
    x = convertFromCharToInt a
    x' = convertFromCharToInt a'    


--checks if the path to the location is clear
isLegal':: Piece -> Board -> Location -> Bool
--Bishop
isLegal' (B (a, y)) (player, whitePieces, blackPieces) (a', y')
  | y == y' && x == x' = True 
  | y > y' && x > x'   && isThere whitePieces blackPieces (a, y) = isLegal' (B (b', y- 1)) (player, whitePieces, blackPieces) (a', y')
  | y < y' && x < x'   && isThere whitePieces blackPieces (a, y) = isLegal' (B (b, y + 1)) (player, whitePieces, blackPieces) (a', y')
  | y > y' && x < x'   && isThere whitePieces blackPieces (a, y) = isLegal' (B (b, y - 1)) (player, whitePieces, blackPieces) (a', y')
  | y < y' && x > x'   && isThere whitePieces blackPieces (a, y) = isLegal' (B (b', y+ 1)) (player, whitePieces, blackPieces) (a', y')
  |otherwise = False
  where
    x = convertFromCharToInt a
    x' = convertFromCharToInt a'
    b = convertFromIntToChar (x + 1)
    b' = convertFromIntToChar (x - 1)
--Rook
isLegal' (R (a, y)) (player, whitePieces, blackPieces) (a', y')
  | y == y' && x == x' = True 
  | y > y'   && isThere whitePieces blackPieces (a, y) = isLegal' (R (a, y- 1)) (player, whitePieces, blackPieces) (a', y')
  | y < y'   && isThere whitePieces blackPieces (a, y) = isLegal' (R (a, y+ 1)) (player, whitePieces, blackPieces) (a', y')
  | x < x'   && isThere whitePieces blackPieces (a, y) = isLegal' (R (b , y)) (player, whitePieces, blackPieces) (a', y')
  | x > x'   && isThere whitePieces blackPieces (a, y) = isLegal' (R (b', y)) (player, whitePieces, blackPieces) (a', y')
  |otherwise = False
  where
    x = convertFromCharToInt a
    x' = convertFromCharToInt a'
    b = convertFromIntToChar (x + 1)
    b' = convertFromIntToChar (x - 1)

--first step
isLegal'':: Piece -> Board -> Location -> Bool
--Bishop
isLegal'' (B (a, y)) (player, whitePieces, blackPieces) (a', y')
  | y == y' && x == x' = True 
  | y > y' && x > x' = isLegal' (B (b', y- 1)) (player, whitePieces, blackPieces) (a', y')
  | y < y' && x < x' = isLegal' (B (b, y + 1)) (player, whitePieces, blackPieces) (a', y')
  | y > y' && x < x' = isLegal' (B (b, y - 1)) (player, whitePieces, blackPieces) (a', y')
  | y < y' && x > x' = isLegal' (B (b', y+ 1)) (player, whitePieces, blackPieces) (a', y')
  |otherwise = False
  where
    x = convertFromCharToInt a
    x' = convertFromCharToInt a'
    b = convertFromIntToChar (x + 1)
    b' = convertFromIntToChar (x - 1)
--Rook
isLegal'' (R (a, y)) (player, whitePieces, blackPieces) (a', y')
  | y == y' && x == x' = True 
  | y > y' = isLegal' (R (a, y- 1)) (player, whitePieces, blackPieces) (a', y')
  | y < y' = isLegal' (R (a, y+ 1)) (player, whitePieces, blackPieces) (a', y')
  | x < x' = isLegal' (R (b , y)) (player, whitePieces, blackPieces) (a', y')
  | x > x' = isLegal' (R (b', y)) (player, whitePieces, blackPieces) (a', y')
  |otherwise = False
  where
    x = convertFromCharToInt a
    x' = convertFromCharToInt a'
    b = convertFromIntToChar (x + 1)
    b' = convertFromIntToChar (x - 1)
    
suggestMove:: Piece -> Board -> [Location]
suggestMove (P (a, y)) board = suggestMove' (P (a, y)) board ('a',1) []
suggestMove (R (a, y)) board = suggestMove' (R (a, y)) board ('a',1) []
suggestMove (N (a, y)) board = suggestMove' (N (a, y)) board ('a',1) []
suggestMove (K (a, y)) board = suggestMove' (K (a, y)) board ('a',1) []
suggestMove (Q (a, y)) board = suggestMove' (Q (a, y)) board ('a',1) []
suggestMove (B (a, y)) board = suggestMove' (B (a, y)) board ('a',1) []


suggestMove':: Piece -> Board -> Location -> [Location] -> [Location]
--pawn
suggestMove' (P (a, y)) board (a', y') list
  | a' == 'm' = list
  | y' == 9 = suggestMove' (P (a, y)) board (b,1) list
  | isLegal (P (a, y)) board (a', y') = suggestMove' (P (a, y)) board (a', y' + 1) (list ++ [(a', y')])
  | otherwise = suggestMove' (P (a, y)) board (a', y' + 1) list
  where
    x = convertFromCharToInt a
    x' = convertFromCharToInt a'
    b = convertFromIntToChar (x' + 1)
--bishop
suggestMove' (B (a, y)) board (a', y') list
  | a' == 'm' = list
  | y' == 9 = suggestMove' (B (a, y)) board (b,1) list
  | isLegal (B (a, y)) board (a', y') = suggestMove' (B (a, y)) board (a', y' + 1) (list ++ [(a', y')])
  | otherwise = suggestMove' (B (a, y)) board (a', y' + 1) list
  where
    x = convertFromCharToInt a
    x' = convertFromCharToInt a'
    b = convertFromIntToChar (x' + 1)
--rook
suggestMove' (R (a, y)) board (a', y') list
  | a' == 'm' = list
  | y' == 9 = suggestMove' (R (a, y)) board (b,1) list
  | isLegal (R (a, y)) board (a', y') = suggestMove' (R (a, y)) board (a', y' + 1) (list ++ [(a', y')])
  | otherwise = suggestMove' (R (a, y)) board (a', y' + 1) list
  where
    x = convertFromCharToInt a
    x' = convertFromCharToInt a'
    b = convertFromIntToChar (x' + 1)
--knight
suggestMove' (N (a, y)) board (a', y') list
  | a' == 'm' = list
  | y' == 9 = suggestMove' (N (a, y)) board (b,1) list
  | isLegal (N (a, y)) board (a', y') = suggestMove' (N (a, y)) board (a', y' + 1) (list ++ [(a', y')])
  | otherwise = suggestMove' (N (a, y)) board (a', y' + 1) list
  where
    x = convertFromCharToInt a
    x' = convertFromCharToInt a'
    b = convertFromIntToChar (x' + 1)
--king
suggestMove' (K (a, y)) board (a', y') list
  | a' == 'm' = list
  | y' == 9 = suggestMove' (K (a, y)) board (b,1) list
  | isLegal (K (a, y)) board (a', y') = suggestMove' (K (a, y)) board (a', y' + 1) (list ++ [(a', y')])
  | otherwise = suggestMove' (K (a, y)) board (a', y' + 1) list
  where
    x = convertFromCharToInt a
    x' = convertFromCharToInt a'
    b = convertFromIntToChar (x' + 1)
--queen
suggestMove' (Q (a, y)) board (a', y') list
  | a' == 'm' = list
  | y' == 9 = suggestMove' (Q (a, y)) board (b,1) list
  | isLegal (Q (a, y)) board (a', y') = suggestMove' (Q (a, y)) board (a', y' + 1) (list ++ [(a', y')])
  | otherwise = suggestMove' (Q (a, y)) board (a', y' + 1) list
  where
    x = convertFromCharToInt a
    x' = convertFromCharToInt a'
    b = convertFromIntToChar (x' + 1)

--