{-# LANGUAGE OverloadedStrings #-}

module Day14 where

import Data.Text (Text)
import Data.Text.IO qualified as T
import Data.Vector (Vector, (!?))
import Data.Vector qualified as V
import Test.Hspec
import Debug.Trace

goInternalWithFile :: (Text -> Int) -> IO ()
goInternalWithFile fn =
    T.readFile "data/day14-input.txt"
        >>= print
            . fn

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

-------------------------------------------------------------------------------
--                                   Part 1                                  --
-------------------------------------------------------------------------------

go1 :: IO ()
go1 = goInternalWithFile run1

run1 :: Text -> Int
run1 _ = 0

moveRoundRockToNorth :: Vector (Vector Char) -> Vector (Vector Char)
moveRoundRockToNorth = V.map mvNorth
  where
    mvNorth :: Vector Char -> Vector Char
    mvNorth vs = vs

-------------------------------------------------------------------------------
--                                   Part 2                                  --
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
--                                    Test                                   --
-------------------------------------------------------------------------------

-- testMain :: IO ()
-- testMain = do
--     hspec $ do
--         describe "Day14 Test" $ do
--             let testDataWithIndex =
--                     zipWith
--                         (\(a, b, c) i -> (a, b, c, i))
--                         testData
--                         ([1 ..] :: [Int])
--             forM_ testDataWithIndex $ \(inputs, expected1, expected2, index) -> do
--                 it (show index) $ do
--                     run1 inputs `shouldBe` expected1

-- testData :: [(Text, Int, Int)]
-- testData =
--     [ (sample, 1320, 145)
--     ]

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
