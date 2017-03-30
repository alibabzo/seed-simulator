module Lib
  ( simulation
  ) where

import Control.Monad (when)
import Data.Array
import Data.List (transpose)
import Data.List.Split (chunksOf)
import System.Random (randomRIO)

data Square
  = Soil
  | Seed
  | Plant
  | Rock
  deriving (Eq)

-- TODO: make this a manually-implemented instance of Read instead
-- I couldn't really be bothered to read docs
read' :: Char -> Square
read' char =
  case char of
    'S' -> Seed
    'P' -> Plant
    'X' -> Rock
    '.' -> Soil
    _ -> error "parseFile: Unrecognised character"

instance Show Square where
  show Soil = "."
  show Seed = "S"
  show Plant = "P"
  show Rock = "X"

data Options = Options
  { step :: Bool
  , yearsToRun :: Int
  , loadFile :: Bool
  }

fieldLength = 20

fieldWidth = 35

type Field = Array (Int, Int) Square

getOptions :: IO Options
getOptions = do
  putStrLn
    "Welcome to the Plant Growing Simulation.\n\
        \You can step through the simulation a year at a time\n\
        \or run the simulation for 1 to n years.\n\
        \How many years do you want the simulation to run for?"
  years <- getLine
  putStrLn "Do you want to enable stepping mode? (N)"
  doStep <- getLine
  putStrLn "Do you want to load a file with seed positions? (Y/N)"
  doFile <- getLine
  return (Options (parseBool doStep) (read years :: Int) (parseBool doFile))
  where
    parseBool x
      | x == "Y" || x == "y" = True
      | otherwise = False

simulation :: IO ()
simulation = do
  options <- getOptions
  if step options
    then stepSim
    else sim options
  where
    sim options = do
      field <- years options
      printField field
      putStrLn "End of Simulation"
    stepSim = undefined
    years options =
      last . take (yearsToRun options) $
      iterate (>>= simulateYear) (initialField options)
    initialField options
      | not (loadFile options) = return $ createNewField 17 10
      | otherwise = parseFile

parseFile :: IO Field
parseFile = do
  putStrLn "Enter file name"
  filePath <- getLine
  contents <- readFile filePath
  return $ arr . flatten $ contents
  where
    indexes = [(x, y) | x <- [0 .. fieldWidth], y <- [0 .. fieldLength]]
    arr = array ((0, 0), (fieldWidth, fieldLength)) . zip indexes
    flatten = concatMap (map read') . transpose . lines

createNewField :: Int -> Int -> Field
createNewField xpos ypos = seededSoil
  where
    soil =
      array
        ((0, 0), (fieldWidth, fieldLength))
        [((x, y), Soil) | x <- [0 .. fieldWidth], y <- [0 .. fieldLength]]
    seededSoil = soil // [((xpos, ypos), Seed)]

printField :: Field -> IO ()
printField =
  putStrLn .
  unlines .
  map (concatMap show) . transpose . chunksOf (fieldLength + 1) . elems

simulateYear :: Field -> IO Field
simulateYear x = do
  r1 <- randomRIO (0, 1) :: IO Int
  r2 <- randomRIO (0, 2) :: IO Int
  let (spr, frost) = simulateSpring r1 x
  when frost $ putStrLn "There has been a frost"
  printField spr
  let (summ, drought) = simulateSummer r2 spr
  when drought $ putStrLn "There has been a severe drought"
  printField summ
  let aut = simulateAutumn summ
  printField aut
  return $ simulateWinter aut

-- Seeds become plants, 50% chance of frost (kills every 3rd plant)
simulateSpring :: Int -> Field -> (Field, Bool)
simulateSpring rnd field
  | rnd == 1 = (spring field, False)
  | otherwise = (frost $ spring field, True)
  where
    spring fld =
      fld //
      [ ((x, y), Plant)
      | x <- [0 .. fieldWidth]
      , y <- [0 .. fieldLength]
      , fld ! (x, y) == Seed
      ]
    frost fld =
      fld //
      each
        3
        [ ((x, y), Soil)
        | x <- [0 .. fieldWidth]
        , y <- [0 .. fieldLength]
        , fld ! (x, y) == Plant
        ]

-- 50% chance of drought (every other plant becomes soil)
simulateSummer :: Int -> Field -> (Field, Bool)
simulateSummer rnd field
  | rnd == 0 = (drought field, True)
  | otherwise = (field, False)
  where
    drought fld =
      fld //
      each
        2
        [ ((x, y), Soil)
        | x <- [0 .. fieldWidth]
        , y <- [0 .. fieldLength]
        , fld ! (x, y) == Plant
        ]

-- Every plant drops seeds in radius 1 around it
simulateAutumn :: Field -> Field
simulateAutumn field = field // replacements
  where
    replacements =
      concatMap
        seed
        [ (x, y)
        | x <- [0 .. fieldWidth]
        , y <- [0 .. fieldLength]
        , field ! (x, y) == Plant
        ]
    seed (x, y) =
      [ ((a, b), Seed)
      | a <- [x - 1 .. x + 1]
      , b <- [y - 1 .. y + 1]
      , 0 <= a && a <= fieldWidth
      , 0 <= b && b <= fieldLength
      , field ! (a, b) == Soil
      ]

-- Every plant dies
simulateWinter :: Field -> Field
simulateWinter field =
  field //
  [ ((x, y), Soil)
  | x <- [0 .. fieldWidth]
  , y <- [0 .. fieldLength]
  , field ! (x, y) == Plant
  ]

each :: Int -> [a] -> [a]
each n l
  | length l > n = map last . chunksOf n $ l
  | otherwise = []
