module Lib
    ( someFunc
    ) where

-- instance Show Content where
--     show Black = " ● "
--     show White = " ○ "
--     show Empty = "   "

type Coord = (Int, Int)
type Cell = (String, String, Coord)  -- (String, Cor)

type Board = [[Cell]]

emptyCell :: Int -> Int -> Int -> Cell
emptyCell n x y
    | even n =  ("   ", "\x1b[41m", (x, y))  -- Célula com fundo vermelho
    | otherwise = if (y < 3) 
        then (" ● ", "\x1b[44m", (x, y)) 
        else 
            if (y > 4) 
                then (" ○ ", "\x1b[44m", (x, y))
                else ("   ", "\x1b[44m", (x, y))  -- Célula com fundo azul


emptyBoard :: Int -> Board
emptyBoard size = [[emptyCell (x + y) x y | x <- [0..size-1]] | y <- [0..size-1]]


showCell :: Cell -> String
showCell (content, color, coord) = color ++ content ++ "\x1b[0m"  -- Reset da cor

showBoard :: Board -> IO ()
showBoard board = do
    let letters = "87654321"
    let numberedRows = zip [1..] board
    putStr (unlines (map (showRow letters) numberedRows))
    putStrLn "   A  B  C  D  E  F  G  H"
  where
    showRow letters (rowNum, row) = letters !! (rowNum - 1) : " " ++ concatMap (\cell -> showCell cell ++ "") row
    

someFunc :: IO ()
someFunc = showBoard (emptyBoard 8)