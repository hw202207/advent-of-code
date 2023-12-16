{-# LANGUAGE OverloadedStrings #-}

module Day15 where

import Control.Monad
import Data.Char
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Test.Hspec

-- import Debug.Trace

goInternalWithFile :: (Text -> Int) -> IO ()
goInternalWithFile fn =
    T.readFile "data/day15-input.txt"
        >>= print
            . fn

run :: Text -> Int
run =
    sum
        . fmap hash
        . T.splitOn ","
            . T.init -- ignore line break

hash :: Text -> Int
hash = T.foldl' hashInternal 0

hashInternal :: Int -> Char -> Int
hashInternal initVal c =
    (ord c + initVal) * 17 `rem` 256

-------------------------------------------------------------------------------
--                                   Part 1                                  --
-------------------------------------------------------------------------------

go1 :: IO ()
go1 = goInternalWithFile run

-------------------------------------------------------------------------------
--                                    Test                                   --
-------------------------------------------------------------------------------

testMain :: IO ()
testMain = do
    hspec $ do
        describe "Day5 Test" $ do
            let testDataWithIndex =
                    zipWith
                        (\(a, b, c) i -> (a, b, c, i))
                        testData
                        ([1 ..] :: [Int])
            forM_ testDataWithIndex $ \(inputs, expected1, _expected2, index) -> do
                it (show index) $ do
                    run inputs `shouldBe` expected1

testData :: [(Text, Int, Int)]
testData =
    [ (sample, 1320, 0)
    ]

sample :: Text
sample = "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7\n"
