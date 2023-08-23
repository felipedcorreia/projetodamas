module Lib
    ( someFunc
    ) where

data Content = Black | White | Empty 
instance Show Content where
    show Black = " ● "
    show White = " ○ "
    show Empty = "   "

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

----Showing coordinates
-- emptyCell :: Int -> Int -> Int -> Cell
-- emptyCell n x y
--     | even n = (" x: " ++ show x ++ " y:" ++ show y ++ " ", "\x1b[41m", (x, y))  
--     | otherwise = (" x: " ++ show x ++ " y:" ++ show y ++ " ", "\x1b[44m", (x, y))  
    
-- emptyCell :: Int -> Int -> Int -> Cell
-- emptyCell n x y
--     | even n = ("   ", "\x1b[41m", (x, y))  -- Célula com fundo vermelho
--     | otherwise = ("   ", "\x1b[44m", (x, y))  -- Célula com fundo azul

emptyBoard :: Int -> Board
emptyBoard size = [[initialSetCell (x + y) x y | x <- [0..size-1]] | y <- [0..size-1]]


showCell :: Cell -> String
showCell (content, color, coord) = color ++ show content ++ "\x1b[0m"  -- Reset da cor

showBoard :: Board -> IO ()
showBoard board = do
    let letters = "12345678"
    let numberedRows = zip [1..] board
    putStrLn "   A  B  C  D  E  F  G  H"
    putStr (unlines (map (showRow letters) numberedRows))
    
  where
    showRow letters (rowNum, row) = letters !! (rowNum - 1) : " " ++ concatMap (\cell -> showCell cell ++ "") row

-- movePiece :: Board -> Coord -> Coord -> Board
-- movePiece (x, y) (x,y) (x', y') = 

someFunc :: IO ()
someFunc = showBoard (emptyBoard 8)