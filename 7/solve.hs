import Data.Char                                                                                                        
import Data.List                                                                                                        
import Data.List.Split                                                                                                  
import System.Environment                                                                                               
                                                                                                                        
import Debug.Trace                                                                                                      

conso from to
  | from > to = conso to from
  | otherwise = ((to-from)*(to-from+1)) `div` 2
                                                                                                                        
main = do                                                                                                               
    file:args <- getArgs                                                                                                
    putStrLn $ "Reading " ++ file                                                                                       
    line <- readFile file                                                                                               
    putStrLn "line = "                                                                                                  
    print line                                                                                                          

    let crabPos = map (read::String->Int) $ words $ map (\c -> if c==',' then ' ' else c) line
    putStrLn "crabPos ="
    print crabPos

    -- Calculate all possibilities and take the less consuming one
    let result1 = minimum $ map sum $ map (\n -> map (\x -> abs (x-n)) crabPos) [(minimum crabPos)..(maximum crabPos)]
    putStrLn "result1 ="
    print result1

    let result2 = minimum $ map sum $ map (\n -> map (\x -> conso x n) crabPos) [(minimum crabPos)..(maximum crabPos)]
    putStrLn "result2 ="
    print result2
