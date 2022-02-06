import Data.Char
import Data.List
import Data.List.Split
import System.Environment

import qualified Data.Map.Strict as Map

listPointsBetween :: [Int] -> [[Int]]
listPointsBetween (x1 : y1 : x2 : y2 : _)
  | x1 /= x2 && y1 /= y2 = []
  | x1 == x2 && y1 > y2 = listPointsBetween [x2,y2,x1,y1]
  | y1 == y2 && x1 > x2 = listPointsBetween [x2,y2,x1,y1]
  | x1 == x2 = [[x1, n] | n <- [y1..y2]]
  | y1 == y2 = [[n, y1] | n <- [x1..x2]]
  | otherwise = error "Should be unreachable"

--addPointsToMap :: Map.Map k v -> [[Int]] -> Map.Map k v
--addPointsToMap map [] = map
--addPointsToMap map (point : points) = Map.insertWith (+) point 1 map

main = do
    file:args <- getArgs
    putStrLn $ "Reading " ++ file
    line <- readFile file
    putStrLn "line = "
    print line
    
    let vectors = chunksOf 4 $ map (read::String->Int) $ words $ map (\c -> if c==',' then ' ' else if isDigit c then c else ' ') line
    print vectors

    let allPoints = map (\x -> (x,1)) $ concat $ map listPointsBetween vectors
    putStrLn "allPoints = "
    print allPoints

    let multiPoints = Map.filter (>1) $ Map.fromListWith (+) allPoints
    putStrLn "multiPoints = "
    print multiPoints

    putStrLn "result = "
    print $ Map.size multiPoints
