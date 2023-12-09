{-# LANGUAGE RecordWildCards #-}

module Day5 where

import Text.Parsec
import Text.Parsec.String (Parser)

data Config = Config
    { seeds :: [Int]
    , seedToSoilMap :: [[Int]]
    , soilToFertilizerMap :: [[Int]]
    , fertilizerToWaterMap :: [[Int]]
    , waterToLightMap :: [[Int]]
    , lightToTemperatureMap :: [[Int]]
    , temperatureToHumidityMap :: [[Int]]
    , humidityToLocationMap :: [[Int]]
    }
    deriving (Show)

-- Parse a list of integers
parseIntList :: Parser [Int]
parseIntList =
    sepEndBy1 (read <$> many1 digit) spaces

-- Parse a matrix of integers
parseIntMatrix :: Parser [[Int]]
parseIntMatrix = many1 parseIntList

-- Parse the entire configuration
parseConfig :: Parser Config
parseConfig = do
    _ <- string "seeds:" >> spaces
    seeds <- parseIntList
    skipMany newline

    _ <- string "seed-to-soil map:" >> newline
    seedToSoilMap <- parseIntMatrix

    _ <- string "soil-to-fertilizer map:" >> newline
    soilToFertilizerMap <- parseIntMatrix

    _ <- string "fertilizer-to-water map:" >> newline
    fertilizerToWaterMap <- parseIntMatrix

    _ <- string "water-to-light map:" >> newline
    waterToLightMap <- parseIntMatrix

    _ <- string "light-to-temperature map:" >> newline
    lightToTemperatureMap <- parseIntMatrix

    _ <- string "temperature-to-humidity map:" >> newline
    temperatureToHumidityMap <- parseIntMatrix

    _ <- string "humidity-to-location map:" >> newline
    humidityToLocationMap <- parseIntMatrix

    return Config{..}

go1 :: IO ()
go1 = goInternal >>= print

goInternal :: IO (Either ParseError Config)
goInternal =
    parse parseConfig "" <$> readFile "data/day5-input.txt"

sample :: [String]
sample =
    [ "seeds: 79 14 55 13"
    , ""
    , "seed-to-soil map:"
    , "50 98 2"
    , "52 50 48"
    , ""
    , "soil-to-fertilizer map:"
    , "0 15 37"
    , "37 52 2"
    , "39 0 15"
    , ""
    , "fertilizer-to-water map:"
    , "49 53 8"
    , "0 11 42"
    , "42 0 7"
    , "57 7 4"
    , ""
    , "water-to-light map:"
    , "88 18 7"
    , "18 25 70"
    , ""
    , "light-to-temperature map:"
    , "45 77 23"
    , "81 45 19"
    , "68 64 13"
    , ""
    , "temperature-to-humidity map:"
    , "0 69 1"
    , "1 0 69"
    , ""
    , "humidity-to-location map:"
    , "60 56 37"
    , "56 93 4"
    ]
