module Main where

import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5

main :: IO ()
main = do
  putStrLn "Hello, Advent of code 2023!"
  putStrLn "= Day1"
  Day1.go1
  Day1.go2
  putStrLn "= Day2"
  Day2.go1
  Day2.go2
  putStrLn "= Day3"
  Day3.go1
  Day3.go2
  Day3.testMain
  putStrLn "= Day4"
  Day4.go1
  Day4.go2
  Day4.testMain
  putStrLn "= Day5"
  Day5.go1
  Day5.testMain
