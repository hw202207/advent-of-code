module Day1 (go) where

import Data.Char

go :: IO ()
go =
    readFile "data/day1-input.txt"
        >>= print
        . sum
        . map inputStrToNumber
        . lines

inputStrToNumber :: String -> Int
inputStrToNumber str =
    let xs = filter isDigit str
     in case length xs of
            0 -> 0
            1 -> read (xs ++ xs)
            _ -> read (head xs : last xs : [])
