{-# LANGUAGE OverloadedStrings #-}

module Day14 where

import Control.Monad
import Data.List
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import Test.Hspec

-- import Data.Text (Text)
-- import Debug.Trace

goInternalWithFile :: (String -> Int) -> IO ()
goInternalWithFile fn =
    T.readFile "data/day14-input.txt"
        >>= print
            . fn
            . T.unpack

type Platform = Vector (Vector Char)
parseInput :: String -> Platform
parseInput =
    V.fromList
        . fmap V.fromList
        . lines

-- | turns list of rows into list of columns
transformIntoColumns :: Vector (Vector b) -> Vector (Vector b)
transformIntoColumns vss =
    let colLength = V.length (vss V.! 1)
     in V.fromList $
            fmap
                (\column -> V.map (V.! column) vss)
                [0 .. colLength - 1]

-- | turns list of colmuns into list of rows
transformIntoRows :: Vector (Vector b) -> Vector (Vector b)
transformIntoRows vss =
    let rowLength = V.length (vss V.! 1)
     in V.fromList $
            fmap
                (\row -> V.map (V.! row) vss)
                [0 .. rowLength - 1]

-------------------------------------------------------------------------------
--                                   Part 1                                  --
-------------------------------------------------------------------------------

go1 :: IO ()
go1 = goInternalWithFile run1

run1 :: String -> Int
run1 =
    calculateLoad
        . transformIntoRows
        . moveRoundRockToNorth
        . transformIntoColumns
        . parseInput

calculateLoad :: Platform -> Int
calculateLoad vss =
    let totalRows = V.length vss
     in sum $ V.imap (\i vs -> (totalRows - i) * V.length (V.filter (== 'O') vs)) vss

moveRoundRockToNorth :: Vector (Vector Char) -> Vector (Vector Char)
moveRoundRockToNorth = V.map mvNorthForColumn
  where
    mvNorthForColumn :: Vector Char -> Vector Char
    mvNorthForColumn vs = foldl' mvCharNorth vs [1 .. length vs - 1]

    mvCharNorth :: Vector Char -> Int -> Vector Char
    mvCharNorth vs i
        | i <= 0 = vs
        | otherwise = case (vs V.! i, vs V.! (i - 1)) of
            ('O', '.') ->
                let newVs = vs V.// [(i, '.'), (i - 1, 'O')]
                 in mvCharNorth newVs (i - 1)
            (_, _) -> vs

-------------------------------------------------------------------------------
--                                   Part 2                                  --
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
--                                    Test                                   --
-------------------------------------------------------------------------------

testMain :: IO ()
testMain = do
    hspec $ do
        describe "Day14 Test" $ do
            let testDataWithIndex =
                    zipWith
                        (\(a, b, c) i -> (a, b, c, i))
                        testData
                        ([1 ..] :: [Int])
            forM_ testDataWithIndex $ \(inputs, expected1, _expected2, index) -> do
                it (show index) $ do
                    run1 inputs `shouldBe` expected1

testData :: [(String, Int, Int)]
testData =
    [ (sample, 136, 0)
    ]

sample :: String
sample =
    unlines
        [ "O....#...."
        , "O.OO#....#"
        , ".....##..."
        , "OO.#O....O"
        , ".O.....O#."
        , "O.#..O.#.#"
        , "..O..#O..O"
        , ".......O.."
        , "#....###.."
        , "#OO..#...."
        ]
