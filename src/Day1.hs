{-# LANGUAGE ImportQualifiedPost #-}

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

-- numberStrToNumber :: Text -> Text
-- numberStrToNumber input =
--   let result = foldr
--                 (\(str, num) val ->
--                    if T.take (T.length str) input == str
--                    then val <> (T.pack $ show num) <> numberStrToNumber (T.drop (T.length str) input)
--                    else val
--                 )
--                 ""
--                 numberStr
--    in
--     if result == "" then (T.take 1 input <> numberStrToNumber (T.drop 1 input) ) else result

numberStrToNumber :: Text -> Text
numberStrToNumber str
    | T.length str < 3 = str
    | otherwise =
        let (x, i) = case T.take 3 str of
                "one" -> ("1", 3)
                "two" -> ("2", 3)
                "six" -> ("6", 3)
                _ -> case T.take 4 str of
                    "four" -> ("4", 4)
                    "five" -> ("5", 4)
                    "nine" -> ("9", 4)
                    _ -> case T.take 5 str of
                        "three" -> ("3", 5)
                        "eight" -> ("8", 5)
                        "seven" -> ("7", 5)
                        _ -> (T.take 1 str, 1)
         in x <> numberStrToNumber (T.drop i str)

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

-- T.replace "nine" "9"
--     . T.replace "eight" "8"
--     . T.replace "seven" "7"
--     . T.replace "six" "6"
--     . T.replace "five" "5"
--     . T.replace "four" "4"
--     . T.replace "three" "3"
--     . T.replace "two" "2"
--     . T.replace "one" "1"
