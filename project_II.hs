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


--     a	b    c    d    e    f    g    h
-- 8 | RB | NB | BB | QB | KB | BB | NB | RB |
-- 7 | PB | PB | PB | PB | PB | PB | PB | PB |
-- 6 |    |    |    |    |    |    |    |    |
-- 5 |    |    |    |    |    |    |    |    |
-- 4 |    |    |    |    |    |    |    |    |
-- 3 |    |    |    |    |    |    |    |    |
-- 2 | PW | PW | PW | PW | PW | PW | PW | PW |
-- 1 | RW | NW | BW | QW | KW | BW | NW | RW |


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
  | player == White && y' == y + 1 && x' == x && isThere whitePieces blackPieces (a', y')= True
  | player == Black && y' == y - 1 && x' == x && isThere whitePieces blackPieces (a', y')= True
  | player == White && y == 2 && y' == y + 2 && x' == x && not (elem (P (a', y')) whitePieces) && not (elem (P (a', y')) blackPieces) && not (elem (P (a', y' - 1)) whitePieces) && not (elem (P (a', y' - 1)) blackPieces) && not (elem (N (a', y')) whitePieces) && not (elem (N (a', y')) blackPieces) && not (elem (N (a', y' - 1)) whitePieces) && not (elem (N (a', y' - 1)) blackPieces)&& not (elem (B (a', y')) whitePieces) && not (elem (B (a', y')) blackPieces) && not (elem (B (a', y' - 1)) whitePieces) && not (elem (B (a', y' - 1)) blackPieces)&& not (elem (R (a', y')) whitePieces) && not (elem (R (a', y')) blackPieces) && not (elem (R (a', y' - 1)) whitePieces) && not (elem (R (a', y' - 1)) blackPieces)&& not (elem (Q (a', y')) whitePieces) && not (elem (Q (a', y')) blackPieces) && not (elem (Q (a', y' - 1)) whitePieces) && not (elem (Q (a', y' - 1)) blackPieces)&& not (elem (K (a', y')) whitePieces) && not (elem (K (a', y')) blackPieces) && not (elem (K (a', y' - 1)) whitePieces) && not (elem (K (a', y' - 1)) blackPieces)  = True
  | player == Black && y == 7 && y' == y - 2 && x' == x && not (elem (P (a', y')) whitePieces) && not (elem (P (a', y')) blackPieces) && not (elem (P (a', y' + 1)) whitePieces) && not (elem (P (a', y' + 1)) blackPieces) && not (elem (N (a', y')) whitePieces) && not (elem (N (a', y')) blackPieces) && not (elem (N (a', y' + 1)) whitePieces) && not (elem (N (a', y' + 1)) blackPieces)&& not (elem (B (a', y')) whitePieces) && not (elem (B (a', y')) blackPieces) && not (elem (B (a', y' + 1)) whitePieces) && not (elem (B (a', y' + 1)) blackPieces)&& not (elem (R (a', y')) whitePieces) && not (elem (R (a', y')) blackPieces) && not (elem (R (a', y' + 1)) whitePieces) && not (elem (R (a', y' + 1)) blackPieces)&& not (elem (Q (a', y')) whitePieces) && not (elem (Q (a', y')) blackPieces) && not (elem (Q (a', y' + 1)) whitePieces) && not (elem (Q (a', y' + 1)) blackPieces)&& not (elem (K (a', y')) whitePieces) && not (elem (K (a', y')) blackPieces) && not (elem (K (a', y' + 1)) whitePieces) && not (elem (K (a', y' + 1)) blackPieces)  = True
  | player == White && y' == y + 1 && x' == x + 1 && elem (P (a', y')) blackPieces = True
  | player == White && y' == y + 1 && x' == x - 1 && elem (P (a', y')) blackPieces = True
  | player == Black && y' == y - 1 && x' == x + 1 && elem (P (a', y')) whitePieces = True
  | player == Black && y' == y - 1 && x' == x - 1 && elem (P (a', y')) whitePieces = True
  | otherwise = False
  where
    x = convertFromCharToInt a
    x' = convertFromCharToInt a'
--Knight
isLegal (N (a, y)) (player, whitePieces, blackPieces) (a', y')
  | x' == x && y' == y = False
  | x'> 8 || x' < 1 || y' > 8 || y' < 1 = False
  | player == White && y' == y + 2 && ( x' == x+1 || x' == x-1) && isTherew whitePieces (a', y') = True
  | player == White && y' == y - 2 && ( x' == x+1 || x' == x-1) && isTherew whitePieces (a', y') = True
  | player == White && x' == x + 2 && ( y' == y+1 || y' == y-1) && isTherew whitePieces (a', y') = True
  | player == White && x' == x - 2 && ( y' == y+1 || y' == y-1) && isTherew whitePieces (a', y') = True
  | player == Black && y' == y + 2 && ( x' == x+1 || x' == x-1) && isThereb blackPieces (a', y') = True
  | player == Black && y' == y - 2 && ( x' == x+1 || x' == x-1) && isThereb blackPieces (a', y') = True
  | player == Black && x' == x + 2 && ( y' == y+1 || y' == y-1) && isThereb blackPieces (a', y') = True
  | player == Black && x' == x - 2 && ( y' == y+1 || y' == y-1) && isThereb blackPieces (a', y') = True
  | otherwise = False
  where
    x = convertFromCharToInt a
    x' = convertFromCharToInt a'

--Bishop
isLegal (B (a, y)) (player, whitePieces, blackPieces) (a', y')
  | x' == x || y' == y = False
  | x'> 8 || x' < 1 || y' > 8 || y' < 1 = False
  | player == White && ((y - y' ) `div` ( x - x') == 1 || (y - y' ) `div` ( x - x') == -1 ) && isTherew whitePieces (a', y') = isLegal' (B (a, y)) (player, whitePieces, blackPieces) (a', y')
  | player == Black && ((y - y' ) `div` ( x - x') == 1 || (y - y' ) `div` ( x - x') == -1 ) && isThereb blackPieces (a', y') = isLegal' (B (a, y)) (player, whitePieces, blackPieces) (a', y')
  | otherwise = False
  where
    x = convertFromCharToInt a
    x' = convertFromCharToInt a'

--Rook
isLegal (R (a, y)) (player, whitePieces, blackPieces) (a', y')
  | x' == x && y' == y = False
  | x'> 8 || x' < 1 || y' > 8 || y' < 1 = False
  | player == White && (x' == x || y' == y) && isTherew whitePieces (a', y') = isLegal' (R (a, y)) (player, whitePieces, blackPieces) (a', y')
  | player == Black && (x' == x || y' == y) && isThereb blackPieces (a', y') = isLegal' (R (a, y)) (player, whitePieces, blackPieces) (a', y')
  | otherwise = False
  where
    x = convertFromCharToInt a
    x' = convertFromCharToInt a'

--Queen
isLegal (Q (a, y)) (player, whitePieces, blackPieces) (a', y') 
  | x' == x && y' == y = False
  | x'> 8 || x' < 1 || y' > 8 || y' < 1 = False
  | isLegal (B (a, y)) (player, whitePieces, blackPieces) (a', y') = True
  | isLegal (R (a, y)) (player, whitePieces, blackPieces) (a', y') = True
  | otherwise = False
  where
    x = convertFromCharToInt a
    x' = convertFromCharToInt a'

--King
isLegal (K (a, y)) (player, whitePieces, blackPieces) (a', y')
  | x' == x && y' == y = False
  | x'> 8 || x' < 1 || y' > 8 || y' < 1 = False
  | player == White && y' == y + 1 && x' == x + 1 && isTherew whitePieces (a', y') = True
  | player == White && y' == y + 1 && x' == x - 1 && isTherew whitePieces (a', y') = True
  | player == White && y' == y - 1 && x' == x + 1 && isTherew whitePieces (a', y') = True
  | player == White && y' == y - 1 && x' == x - 1 && isTherew whitePieces (a', y') = True
  | player == White && y' == y + 1 && x' == x     && isTherew whitePieces (a', y') = True
  | player == White && y' == y - 1 && x' == x     && isTherew whitePieces (a', y') = True
  | player == White && y' == y     && x' == x + 1 && isTherew whitePieces (a', y') = True
  | player == White && y' == y     && x' == x - 1 && isTherew whitePieces (a', y') = True
  | player == Black && y' == y + 1 && x' == x + 1 && isThereb blackPieces (a', y') = True
  | player == Black && y' == y + 1 && x' == x - 1 && isThereb blackPieces (a', y') = True
  | player == Black && y' == y - 1 && x' == x + 1 && isThereb blackPieces (a', y') = True
  | player == Black && y' == y - 1 && x' == x - 1 && isThereb blackPieces (a', y') = True
  | player == Black && y' == y + 1 && x' == x     && isThereb blackPieces (a', y') = True
  | player == Black && y' == y - 1 && x' == x     && isThereb blackPieces (a', y') = True
  | player == Black && y' == y     && x' == x + 1 && isThereb blackPieces (a', y') = True
  | player == Black && y' == y     && x' == x - 1 && isThereb blackPieces (a', y') = True
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
