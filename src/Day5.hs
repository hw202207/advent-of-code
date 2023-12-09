{-# LANGUAGE RecordWildCards #-}

module Day5 where

-- import Debug.Trace
import Text.Parsec
import Text.Parsec.String (Parser)

data Config = Config
    { destRangeStart :: Int
    , sourceRangeStart :: Int
    , rangeLength :: Int
    }
    deriving (Show)

data Inputs = Inputs
    { seedsToPlant :: [Int]
    , seedToSoilMap :: [Config]
    , soilToFertilizerMap :: [Config]
    , fertilizerToWaterMap :: [Config]
    , waterToLightMap :: [Config]
    , lightToTemperatureMap :: [Config]
    , temperatureToHumidityMap :: [Config]
    , humidityToLocationMap :: [Config]
    }
    deriving (Show)

-- Parse a list of integers
parseIntList :: Parser [Int]
parseIntList = do
    xs <- sepEndBy1 (read <$> many1 digit) (satisfy (== ' '))
    _ <- newline
    pure xs

-- Parse a matrix of integers
parseIntMatrix :: Parser [Config]
parseIntMatrix = many1 $ do
    config <- parseIntList
    skipMany newline
    case config of
        [x, y, z] -> pure (Config x y z)
        _ -> error "Fail to parse config line"

-- Parse the entire configuration
parseInputs :: Parser Inputs
parseInputs = do
    _ <- string "seeds:" >> spaces
    seedsToPlant <- parseIntList
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

    return Inputs{..}

go1 :: IO ()
go1 = goInternal >>= print

goInternal :: IO (Either ParseError Inputs)
goInternal =
    parse parseInputs "" <$> readFile "data/day5-input.txt"

-------------------------------------------------------------------------------
--                                    Test                                   --
-------------------------------------------------------------------------------

testMain :: Either ParseError Inputs
testMain = parse parseInputs "" (unlines sample)

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
