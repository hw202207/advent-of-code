{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Day1 where

import Data.Char
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T

goInternal :: (Text -> Int) -> IO ()
goInternal calFn =
    T.readFile "data/day1-input.txt"
        >>= print
        . sum
        . map calFn
        . T.lines

go :: IO ()
go = go1 >> go2

go1 :: IO ()
go1 = goInternal calibrateStr

go2 :: IO ()
go2 = goInternal (calibrateStr . numberStrToNumber)

calibrateStr :: Text -> Int
calibrateStr str =
    let xs = T.filter isDigit str
     in case T.length xs of
            0 -> 0
            1 -> read $ T.unpack $ xs <> xs
            _ -> read (T.head xs : T.last xs : [])

numberStrToNumber :: Text -> Text
numberStrToNumber input
    | T.null input = input
    | otherwise =
        let result =
                foldr
                    ( \(str, num) val ->
                        if T.take (T.length str) input == str
                            then T.pack (show num)
                            else val
                    )
                    (T.take 1 input)
                    numberStr
         in result <> numberStrToNumber (T.drop 1 input)

numberStr :: [(Text, Int)]
numberStr =
    [ ("one", 1)
    , ("two", 2)
    , ("three", 3)
    , ("four", 4)
    , ("five", 5)
    , ("six", 6)
    , ("seven", 7)
    , ("eight", 8)
    , ("nine", 9)
    ]

test2 :: IO ()
test2 =
    print
        $ sum
        $ map (calibrateStr . numberStrToNumber)
        $ [ "two1nine"
          , "eightwothree"
          , "abcone2threexyz"
          , "xtwone3four"
          , "4nineeightseven2"
          , "zoneight234"
          , "7pqrstsixteen"
          , "3onefqltjzdrfourcpkfhceightwomc" -- shall be 32
          ]
