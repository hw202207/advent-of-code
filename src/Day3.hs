module Day3 where

import Data.Char (isDigit)
import Data.List (nub, sort)
import Data.Vector (Vector, (!), (!?))
import Data.Vector qualified as V

go :: IO ()
go =
    do
        readFile "data/day3-input.txt"
        >>= print
        . goInternal
        . lines

goTest :: IO ()
goTest = print $ goInternal testData

testData :: [String]
testData =
    [ "467..114.."
    , "...*......"
    , "..35..633."
    , "......#..."
    , "617*......"
    , ".....+.58."
    , "..592....."
    , "......755."
    , "...$.*...."
    , ".664.598.."
    ]

goInternal :: [[Char]] -> Int
goInternal rawInputs =
    let inputs = V.fromList (fmap V.fromList rawInputs)
     in sum $ findNumber inputs $ findDigitsIndex inputs

isSymbol :: Char -> Bool
isSymbol c = not (isDigit c) && c /= '.'

isDot :: Char -> Bool
isDot = (== '.')

-- | Find all indexs of Digits that are adjacent of symbol
findDigitsIndex :: Vector (Vector Char) -> [(Int, Int)]
findDigitsIndex inputs =
    sort
        $ filter
            ( \(i, j) ->
                case inputs !? i of
                    Just row -> case row !? j of
                        Just e -> not (isDot e)
                        Nothing -> False
                    Nothing -> False
            )
        $ concat
        $ V.toList
        $ V.concat
        $ V.toList
        $ V.imap
            ( \i v ->
                V.imap
                    ( \j a ->
                        if isSymbol a
                            then
                                [ (i - 1, j)
                                , (i - 1, j - 1)
                                , (i - 1, j + 1)
                                , (i + 1, j)
                                , (i + 1, j - 1)
                                , (i + 1, j + 1)
                                , (i, j - 1)
                                , (i, j + 1)
                                ]
                            else []
                    )
                    v
            )
            inputs

findNumber :: Vector (Vector Char) -> [(Int, Int)] -> [Int]
findNumber inputs = nub . map (findNumberAroundIndex inputs)

findNumberAroundIndex :: Vector (Vector Char) -> (Int, Int) -> Int
findNumberAroundIndex inputs (i, j) =
    read
        $ findNumberAtIndexLeft inputs (i, j - 1) []
        ++ [(inputs ! i) ! j]
        ++ findNumberAtIndexRight inputs (i, j + 1) []

findNumberAtIndexLeft :: Vector (Vector Char) -> (Int, Int) -> [Char] -> [Char]
findNumberAtIndexLeft inputs (i, j) accum =
    case inputs !? i of
        Just row -> case row !? j of
            Just el -> if isDigit el then findNumberAtIndexLeft inputs (i, j - 1) (el : accum) else accum
            Nothing -> accum
        Nothing -> accum

findNumberAtIndexRight :: Vector (Vector Char) -> (Int, Int) -> [Char] -> [Char]
findNumberAtIndexRight inputs (i, j) accum =
    case inputs !? i of
        Just row -> case row !? j of
            Just el -> if isDigit el then findNumberAtIndexRight inputs (i, j + 1) (accum ++ [el]) else accum
            Nothing -> accum
        Nothing -> accum
