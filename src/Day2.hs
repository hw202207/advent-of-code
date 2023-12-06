{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Day2 where

import Data.List
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T

type Count = Int
data CubeColor = Blue | Red | Green
    deriving (Eq, Show)
newtype Cube = Cube [(CubeColor, Count)]
    deriving (Eq, Show)

data Game = Game Int [Cube]
    deriving (Eq, Show)

-- Shall you use Parser but string split seems good enough
-- Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
--
parseGame :: Text -> Game
parseGame input =
    let [game, info] = T.split (== ':') input
        [_, indexStr] = T.words game
        subCubes = T.split (== ';') info
     in Game (read $ T.unpack indexStr) (fmap parseSubCube subCubes)

-- 3 blue, 4 red --> Cube
parseSubCube :: Text -> Cube
parseSubCube = Cube . fmap (parseCube . T.strip) . T.split (== ',')

-- 3 blue --> (Blue, 3)
parseCube :: Text -> (CubeColor, Count)
parseCube input =
    let [countStr, colorStr] = T.words input
     in (parseColor colorStr, read $ T.unpack countStr)

parseColor :: Text -> CubeColor
parseColor input = case input of
    "blue" -> Blue
    "red" -> Red
    "green" -> Green
    _ -> error $ T.unpack $ "unable to parse color string: " <> input

-- 12 red cubes, 13 green cubes, and 14 blue cube
elfConfig :: Cube
elfConfig =
    Cube
        [ (Red, 12)
        , (Green, 13)
        , (Blue, 14)
        ]
isCubePossible :: Cube -> Bool
isCubePossible (Cube inputCube) =
    let (Cube ec) = elfConfig
     in intersectBy (\(color1, count1) (color2, count2) -> color1 == color2 && count1 <= count2) inputCube ec == inputCube

isGamePossible :: Game -> Bool
isGamePossible (Game _ cs) = all isCubePossible cs

go :: IO ()
go =
    T.readFile "data/day2-input.txt"
        >>= print
        . sum
        . fmap (\(Game index _) -> index)
        . filter isGamePossible
        . fmap parseGame
        . T.lines
