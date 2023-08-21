module Lib
    ( someFunc
    ) where

type Cell = (String, String)  -- (String, Cor)

type Board = [[Cell]]

emptyCell :: Int -> Cell
emptyCell n
    | even n = ("   ", "\x1b[41m")  -- Célula com fundo vermelho
    | otherwise = ("   ", "\x1b[44m")  -- Célula com fundo azul


emptyBoard :: Int -> Board
emptyBoard size = [[emptyCell (x + y) | x <- [0..size-1]] | y <- [0..size-1]]


showCell :: Cell -> String
showCell (content, color) = color ++ content ++ "\x1b[0m"  -- Reset da cor

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