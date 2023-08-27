module Lib
    ( someFunc
    ) where
import Control.Monad (guard)


data Content = Black | White | Empty | WhiteDama | BlackDama deriving(Eq)

instance Show Content where
    show Black = " ● "
    show White = " ○ "
    show Empty = "   "
    show BlackDama = " ⦿ "
    show WhiteDama = " ⓞ  "

type Coord = (Int, Int)
type Cell = (Content, String, Coord)  -- (String, Cor)

type Board = [[Cell]]

initialSetCell :: Int -> Int -> Int -> Cell
initialSetCell n x y
    | even n =  (Empty, "\x1b[41m", (x, y)) 
    | otherwise = if (y < 3) 
        then (Black, "\x1b[44m", (x, y)) 
        else 
            if (y > 4) 
                then (White, "\x1b[44m", (x, y))
                else (Empty, "\x1b[44m", (x, y))  

--Showing coordinates
-- initialSetCell :: Int -> Int -> Int -> Cell
-- initialSetCell n x y
--     | even n = (" x: " ++ show x ++ " y:" ++ show y ++ " ", "\x1b[41m", (x, y))  
--     | otherwise = (" x: " ++ show x ++ " y:" ++ show y ++ " ", "\x1b[44m", (x, y))  

emptyCell :: Int -> Int -> Int -> Cell
emptyCell n r c
    | even n = (Empty, "\x1b[41m", (r, c))  -- Célula com fundo vermelho
    | otherwise = (Empty, "\x1b[44m", (r, c))  -- Célula com fundo azul

createCell :: Int -> Int -> Content -> Cell
createCell r c content = if content == White then (White, "\x1b[44m", (r, c)) else (Black, "\x1b[44m", (r, c))

emptyBoard :: Int -> Board
emptyBoard size = [[emptyCell (y + x) x y | x <- [0..size-1]] | y <- [0..size-1]]

createBoard :: Int -> Board
createBoard size = [[initialSetCell (y + x) x y | x <- [0..size-1]] | y <- [0..size-1]]



showCell :: Cell -> String
showCell (content, color, coord) = color ++ show content ++ "\x1b[0m"  -- Reset da cor

showBoard :: Board -> IO ()
showBoard board = do
    let letters = "01234567"
    let numberedRows = zip [1..] board
    putStrLn "   0  1  2  3  4  5  6  7"
    putStr (unlines (map (showRow letters) numberedRows))

  where
    showRow letters (rowNum, row) = letters !! (rowNum - 1) : " " ++ concatMap (\cell -> showCell cell ++ "") row

showContent :: Content -> String
showContent n = show n

movePiece :: Board -> Coord -> Coord -> Board
movePiece board (x, y) (x', y') = 
    [[if 
        (r == y && c == x) then emptyCell 1 c r 
        else if (r == y' && c == x') 
            then createCell c r (getContent(getCell board x y))
            else getCell board c r | c <- [0..7]] | r <- [0..7]]

ehMovimentoValido :: Coord -> Coord -> Content -> Bool
ehMovimentoValido inicial movimento valor = 
    if (length(casas) >= 2)
        then ((casas)!!0 == movimento) || ((casas)!!1 == movimento)
        else (casas)!!0 == movimento
    where casas = casasPossiveis inicial valor


ehCasaLivre :: Board -> Coord -> Bool
ehCasaLivre board (c, r) = getContent (getCell board c r) == Empty

getCell :: Board -> Int -> Int -> Cell
getCell board col row = (board !! row) !! col

getContent :: Cell -> Content
getContent (x, _, _) = x

verifyContent :: Content -> Bool
verifyContent content = content == White

casasPossiveis :: Coord -> Content -> [Coord] 
casasPossiveis (c,r) valor = do
    (c1, r1) <- [(c+1, r-1),(c-1, r-1)]
    (c2, r2) <- [(c+1, r+1),(c-1, r+1)]
    guard (c1 `elem` [0..7] && r1 `elem`[0..7])
    guard (c2 `elem` [0..7] && r2 `elem`[0..7])
    if valor == White then return (c1, r1) else return (c2, r2)

showCasas :: [Coord] -> String
showCasas x = show x

moverPeca :: Board -> Coord -> Coord -> Board
moverPeca board (c, r) movimento = 
    if ehMovimentoValido (c, r) movimento (getContent(getCell board c r)) 
        then movePiece board (c, r) movimento 
        else board

someFunc :: IO ()
someFunc = do
    let newBoard = (createBoard 8)
    let cell = getCell newBoard 0 0
    let content = getContent (getCell newBoard 5 4)
    showBoard (newBoard)
    let board1 = moverPeca newBoard (6,5) (7,4)
    showBoard (board1)
    let board2 = moverPeca board1 (1,2) (0,3)
    showBoard (board2)
    -- putStrLn (showCell cell)
    -- putStrLn (showContent content)
    let resultado = (ehMovimentoValido (1,2) (0,3) Black)
    -- let resultado2 = (ehMovimentoValido (6, 5) (7, 4))
    let arrayCasasPossiveis = casasPossiveis (1, 2) Black
    -- putStrLn (show (ehCasaLivre newBoard (5,5)))
    putStrLn (showCasas arrayCasasPossiveis)
    putStrLn (show resultado)
    -- putStrLn (show resultado2)