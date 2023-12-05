import Data.Char
import Data.List
import Data.List.Split
import System.Environment

import Debug.Trace

doBirth (f0:f1:f2:f3:f4:f5:f6:f7:f8:_) = traceShow (f1:f2:f3:f4:f5:f6:f7+f0:f8:f0:[]) (f1:f2:f3:f4:f5:f6:f7+f0:f8:f0:[])
doBirth _ = []

addFishes 0 fishes = fishes
addFishes n fishes = addFishes (n-1) newFishes
  where newFishes = doBirth fishes

main = do
    file:args <- getArgs
    putStrLn $ "Reading " ++ file
    line <- readFile file
    putStrLn "line = "
    print line
    
    let fishes0 = map (read::String->Int) $ words $ map (\c -> if c==',' then ' ' else c) line
    putStrLn "fishes0 ="
    print fishes0

    let t0 = map (\l -> length l - 1) $ group $ sort $ [0..8] ++ fishes0
    putStrLn "t0 = "
    print t0
   
    let result = addFishes 256 t0
    print result
    print $ sum result
