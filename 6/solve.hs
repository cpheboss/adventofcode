import Data.Char
import Data.List
import Data.List.Split
import System.Environment

import Debug.Trace

nbFish :: Int -> Int -> Int
nbFish days age 
  | days <= 0 = 1
  | age > 0 = nbFish (days-age) 0
  | otherwise = nbFish (days-7) 0 + nbFish (days-9) 0

main = do
    file:args <- getArgs
    putStrLn $ "Reading " ++ file
    line <- readFile file
    putStrLn "line = "
    print line
    
    let fishes0 = map (read::String->Int) $ words $ map (\c -> if c==',' then ' ' else c) line
    putStrLn "fishes0 ="
    print fishes0
   
    let result = map (nbFish 80) fishes0
--    let result = map (\n -> map (nbFish n) [0]) [256] --fishes0
    putStrLn "result = "
--    mapM print result
    print result
    print $ sum result
