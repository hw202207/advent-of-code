{-# LANGUAGE OverloadedStrings #-}

module Day15 where

import Control.Monad
import Data.Char
import Data.List
import Data.Map (Map)
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Test.Hspec
import Text.Parsec
import Text.Parsec.String (Parser)

import Debug.Trace

goInternalWithFile :: (Text -> Int) -> IO ()
goInternalWithFile fn =
    T.readFile "data/day15-input.txt"
        >>= print
            . fn

hash :: Text -> Int
hash =
    T.foldl' hashInternal 0
  where
    hashInternal :: Int -> Char -> Int
    hashInternal initVal c =
        (ord c + initVal) * 17 `rem` 256

-------------------------------------------------------------------------------
--                                   Part 1                                  --
-------------------------------------------------------------------------------

go1 :: IO ()
go1 = goInternalWithFile run1

run1 :: Text -> Int
run1 =
    sum
        . fmap hash
        . T.splitOn ","
        . T.takeWhile (/= '\n')

-------------------------------------------------------------------------------
--                                   Part 2                                  --
-------------------------------------------------------------------------------

type Label = Text
type FocalLength = Int
data Lens = Remove Label | Add Label FocalLength
    deriving (Show)

printerHasSameLabel :: Lens -> Lens -> Bool
printerHasSameLabel (Remove l1) (Remove l2) = l1 == l2
printerHasSameLabel (Remove l1) (Add l2 _) = l1 == l2
printerHasSameLabel (Add l1 _) (Remove l2) = l1 == l2
printerHasSameLabel (Add l1 _) (Add l2 _) = l1 == l2

defMap :: Map Int [Lens]
defMap = M.fromList $ [(i, []) | i <- [1 .. 256]]

boxNumber :: Lens -> Int
boxNumber (Remove l1) = hash l1 + 1
boxNumber (Add l1 _) = hash l1 + 1

addBoxNumber :: Lens -> (Int, Lens)
addBoxNumber p = (boxNumber p, p)

parseSingle :: Text -> Lens
parseSingle input =
    case parse printerParser "" (T.unpack input) of
        Left err -> error (show err)
        Right r -> r

printerParser :: Parser Lens
printerParser = try parseRemoveOp <|> parseAddOp
  where
    parseRemoveOp = do
        label1 <- many1 lower
        _ <- char '-'
        pure $ Remove (T.pack label1)
    parseAddOp = do
        label2 <- many1 lower
        _ <- char '='
        focalL <- many1 digit
        pure $ Add (T.pack label2) (read focalL)

run2 :: Text -> Int
run2 =
    sum
        . concat
        . M.elems
        . M.mapWithKey calculateFocalLength
        . (\m -> trace (show $ M.take 2 m) m)
        . M.map printOnBoxes
        . (\m -> trace (show $ M.take 2 m) m)
        . foldl' pushToMap defMap
        . fmap (addBoxNumber . parseSingle)
        . T.splitOn ","
        . T.takeWhile (/= '\n')

pushToMap :: Map Int [Lens] -> (Int, Lens) -> Map Int [Lens]
pushToMap m1 (boxNum, printer) = M.update (\xs -> Just (xs ++ [printer])) boxNum m1

-- do computation for list of printer on same box
printOnBoxes :: [Lens] -> [Lens]
printOnBoxes = foldl' go []
  where
    go :: [Lens] -> Lens -> [Lens]
    go [] (Remove _) = []
    go [] p@(Add _ _) = [p]
    go (x : rest) p =
        if printerHasSameLabel p x
            then case p of
                Remove _ -> rest
                Add _ _ -> p : rest
            else x : go rest p

calculateFocalLength :: Int -> [Lens] -> [Int]
calculateFocalLength boxNum xs = uncurry calc <$> zip xs [1 ..]
  where
    calc (Add _ f) index = boxNum * f * index
    calc (Remove _) _ = 0

go2 :: IO ()
go2 = goInternalWithFile run2

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
            forM_ testDataWithIndex $ \(inputs, expected1, expected2, index) -> do
                it (show index) $ do
                    run1 inputs `shouldBe` expected1
                    run2 inputs `shouldBe` expected2

testData :: [(Text, Int, Int)]
testData =
    [ (sample, 1320, 145)
    , (sample2, 2927, 138)
    ]

sample :: Text
sample = "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7\n"

sample2 :: Text
sample2 =
    "fntz-,pr-,cxmnl-,pr-,jvb=7,jvb-,rfj-,jvb-,fntz=9,cxmnl-,fntz-,rfj-,fntz-,fntz-,cxmnl-,jvb=7,rfj=3,cxmnl-,rfj-,jvb=2,jvb-,jvb=1,jvb=5,jvb-,pr-,rfj=1,cxmnl=3,fntz=6,fntz-,cxmnl-,cxmnl=2,cxmnl-,jvb=3,pr=9,jvb=9,fntz-"
