{-# LANGUAGE RecordWildCards #-}

module Day5 where

import Control.Monad
import Data.Map.Lazy (Map)
import Data.Map.Lazy qualified as Map
import Data.Maybe
import Debug.Trace
import Test.Hspec
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
    , seedToSoilMapList :: [Config]
    , soilToFertilizerMapList :: [Config]
    , fertilizerToWaterMapList :: [Config]
    , waterToLightMapList :: [Config]
    , lightToTemperatureMapList :: [Config]
    , temperatureToHumidityMapList :: [Config]
    , humidityToLocationMapList :: [Config]
    }
    deriving (Show)

data AllMaps = AllMaps
    { seedToSoilMap :: Map Int Int
    , soilToFertilizerMap :: Map Int Int
    , fertilizerToWaterMap :: Map Int Int
    , waterToLightMap :: Map Int Int
    , lightToTemperatureMap :: Map Int Int
    , temperatureToHumidityMap :: Map Int Int
    , humidityToLocationMap :: Map Int Int
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
        _ -> fail $ "Fail to parse config line: expect 3 Int but got " ++ show config

-- Parse the entire configuration
parseInputs :: Parser Inputs
parseInputs = do
    _ <- string "seeds:" >> spaces
    seedsToPlant <- parseIntList
    skipMany newline

    _ <- string "seed-to-soil map:" >> newline
    seedToSoilMapList <- parseIntMatrix

    _ <- string "soil-to-fertilizer map:" >> newline
    soilToFertilizerMapList <- parseIntMatrix

    _ <- string "fertilizer-to-water map:" >> newline
    fertilizerToWaterMapList <- parseIntMatrix

    _ <- string "water-to-light map:" >> newline
    waterToLightMapList <- parseIntMatrix

    _ <- string "light-to-temperature map:" >> newline
    lightToTemperatureMapList <- parseIntMatrix

    _ <- string "temperature-to-humidity map:" >> newline
    temperatureToHumidityMapList <- parseIntMatrix

    _ <- string "humidity-to-location map:" >> newline
    humidityToLocationMapList <- parseIntMatrix

    return Inputs{..}

inputsToMaps :: Inputs -> AllMaps
inputsToMaps Inputs{..} =
    let seedToSoilMap = configsToMap seedToSoilMapList
        soilToFertilizerMap = configsToMap soilToFertilizerMapList
        fertilizerToWaterMap = configsToMap fertilizerToWaterMapList
        waterToLightMap = configsToMap waterToLightMapList
        lightToTemperatureMap = configsToMap lightToTemperatureMapList
        temperatureToHumidityMap = configsToMap temperatureToHumidityMapList
        humidityToLocationMap = configsToMap humidityToLocationMapList
     in AllMaps{..}
  where
    configsToMap :: [Config] -> Map Int Int
    configsToMap = Map.unions . fmap configToMap

    configToMap :: Config -> Map Int Int
    configToMap Config{..} =
        Map.fromList
            [(sourceRangeStart + i, destRangeStart + i) | i <- [0 .. rangeLength - 1]]

seedToLocationWithMap ::
    AllMaps ->
    -- | Seed
    Int ->
    Int
seedToLocationWithMap AllMaps{..} i =
    let soil = Map.findWithDefault i i seedToSoilMap
        fertilizer = Map.findWithDefault soil soil soilToFertilizerMap
        water = Map.findWithDefault fertilizer fertilizer fertilizerToWaterMap
        light = Map.findWithDefault water water waterToLightMap
        temperature = Map.findWithDefault light light lightToTemperatureMap
        humidity = Map.findWithDefault temperature temperature temperatureToHumidityMap
        location = Map.findWithDefault humidity humidity humidityToLocationMap
     in location

seedToLocation :: Inputs -> Int -> Int
seedToLocation Inputs{..} seed =
    let soil = findDestInConfigs seed seedToSoilMapList
        fertilizer = findDestInConfigs soil soilToFertilizerMapList
        water = findDestInConfigs fertilizer fertilizerToWaterMapList
        light = findDestInConfigs water waterToLightMapList
        temperature = findDestInConfigs light lightToTemperatureMapList
        humidity = findDestInConfigs temperature temperatureToHumidityMapList
        location = findDestInConfigs humidity humidityToLocationMapList
     in -- in trace
        --        ( "Seed: "
        --            ++ show seed
        --            ++ ", soil: "
        --            ++ show soil
        --            ++ ", fertilizer: "
        --            ++ show fertilizer
        --            ++ ", water: "
        --            ++ show water
        --            ++ ", light: "
        --            ++ show light
        --            ++ ", temperature: "
        --            ++ show temperature
        --            ++ ", humidity: "
        --            ++ show humidity
        --            ++ ", location: "
        --            ++ show location
        --        )
        location
findDestInConfigs :: Int -> [Config] -> Int
findDestInConfigs s configs =
    let re = mapMaybe (findDestInConfig s) configs
     in case re of
            [r] -> r
            _ -> s

findDestInConfig :: Int -> Config -> Maybe Int
findDestInConfig s Config{..} =
    if s >= sourceRangeStart && s <= sourceRangeStart + rangeLength - 1
        then Just $ destRangeStart + (s - sourceRangeStart)
        else Nothing

goInternalWithFile :: (String -> Int) -> IO ()
goInternalWithFile fn =
    readFile "data/day5-input.txt"
        >>= print
        . fn

goInternal :: (Inputs -> Int) -> String -> Int
goInternal fn inputs =
    case parse parseInputs "" inputs of
        Left err -> error (show err)
        Right r -> fn r

-------------------------------------------------------------------------------
--                                   Part 1                                  --
-------------------------------------------------------------------------------

go1 :: IO ()
go1 = goInternalWithFile goInternal1

goInternal1 :: String -> Int
goInternal1 = goInternal go1'
  where
    go1' :: Inputs -> Int
    go1' r = minimum $ fmap (seedToLocation r) (seedsToPlant r)

-------------------------------------------------------------------------------
--                                   Part 2                                  --
-------------------------------------------------------------------------------
go2 :: IO ()
go2 = goInternalWithFile goInternal2

goInternal2 :: String -> Int
goInternal2 = goInternal go2'
  where
    go2' :: Inputs -> Int
    go2' r = minimum $ fmap (seedToLocation r) (seedsToSeeds $ seedsToPlant r)

    seedsToSeeds :: [Int] -> [Int]
    seedsToSeeds [] = []
    seedsToSeeds [_] = error "seedsToSeeds error: data shall come as pairs"
    seedsToSeeds (x : y : rest) =
        seedRangeToSeeds x y ++ seedsToSeeds rest

    seedRangeToSeeds :: Int -> Int -> [Int]
    seedRangeToSeeds seed range = [seed + i | i <- [0 .. range - 1]]

-------------------------------------------------------------------------------
--                                    Test                                   --
-------------------------------------------------------------------------------

testMain :: IO ()
testMain = do
    day5 <- readFile "data/day5-input.txt"
    hspec $ do
        describe "Day5 Test" $ do
            let testDataWithIndex =
                    zipWith
                        (\(a, b, c) i -> (a, b, c, i))
                        (testData ++ [(day5, 323142486, 0)])
                        ([1 ..] :: [Int])
            forM_ testDataWithIndex $ \(inputs, expected1, _expected2, index) -> do
                it (show index) $ do
                    goInternal1 inputs `shouldBe` expected1

testData :: [(String, Int, Int)]
testData =
    [(unlines sample, 35, 0)]

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
