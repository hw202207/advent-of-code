module Day3 where

import Control.Monad
import Data.Char (isDigit)
import Data.List (sort)
import Data.Vector (Vector, (!?))
import Data.Vector qualified as V
import Test.Hspec

go :: IO ()
go =
    do
        readFile "data/day3-input.txt"
        >>= print
        . goInternal
        . lines

-- debug = do
--     rawInputs <- lines <$> readFile "data/day3-input.txt"
--     let inputs = V.fromList (fmap V.fromList rawInputs)
--     writeFile "/tmp/2.txt" (unlines $ fmap show $ sort $ findNumber inputs $ findDigitsIndex inputs)

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
    removeIndexesForSameNumber
        $ sort
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

removeIndexesForSameNumber :: [(Int, Int)] -> [(Int, Int)]
removeIndexesForSameNumber xs =
    case xs of
        [] -> []
        [x] -> [x]
        (x : y : rest) ->
            [x | not (fst x == fst x && snd y - snd x == 1)]
                ++ removeIndexesForSameNumber (y : rest)

findNumber :: Vector (Vector Char) -> [(Int, Int)] -> [Int]
findNumber inputs = map (findNumberAroundIndex inputs)

findNumberAroundIndex :: Vector (Vector Char) -> (Int, Int) -> Int
findNumberAroundIndex inputs (i, j) =
    read
        $ findNumberAtIndexLeft inputs (i, j) []
        ++ tail (findNumberAtIndexRight inputs (i, j) [])

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

-------------------------------------------------------------------------------
--                                    Test                                   --
-------------------------------------------------------------------------------

testMain :: IO ()
testMain = hspec $ do
    describe "Day3" $ do
        let testDataWithIndex =
                zipWith
                    (\(a, b) i -> (a, b, i))
                    testData
                    ([1 ..] :: [Int])
        forM_ testDataWithIndex $ \(inputs, expected, index) -> do
            it (show index) $ do
                goInternal inputs `shouldBe` expected

testData :: [([String], Int)]
testData =
    [ (sampleData, 4361)
    , (["*67..114.."], 67)
    ,
        (
            [ "67...114.."
            , "+........."
            ]
        , 67
        )
    ,
        (
            [ "&....114.."
            , ".8........"
            ]
        , 8
        )
    ,
        (
            [ "&......8.."
            , ".8.....+.."
            ]
        , 16
        )
    , (["..658.509*378.."], 887)
    ]

sampleData :: [String]
sampleData =
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
