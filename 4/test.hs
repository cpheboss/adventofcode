import Debug.Trace
import Data.List
import Data.List.Split
import System.Environment

complete = [-1,-1,-1,-1,-1]

firstCompleteGrid :: [[[Int]]] -> Maybe [[Int]]
firstCompleteGrid [] = Nothing
firstCompleteGrid (grid : grids)
  | gridComplete grid = Just grid
  | otherwise = firstCompleteGrid grids

gridComplete :: [[Int]] -> Bool
gridComplete g = any (==complete) g

gridScore :: [[Int]] -> Int
-- replace -1 with 0 before summing. Divide by 2 because we have both columns and rows
-- it would also be possible to take the 5 first elements
gridScore g = div (sum $ map ( sum . ( map (\n -> if (n == -1) then 0 else n))) g) 2

getResult1 :: [[[Int]]] -> [Int] -> Int
getResult1 grids (draw : draws) =
  case firstCompleteGrid newGrid of
    Just completedGrid -> draw * gridScore completedGrid
    Nothing -> getResult1 newGrid draws
  where newGrid = map ( map ( map ( \n -> if (n==draw) then (-1) else n ))) grids

main = do
    file : args <- getArgs
    putStrLn $ "Reading " ++ file
    line <- readFile file
    putStrLn "line = "
    print line
    
    let drawStr : gridsStr = lines line
     
    let drawList = map (read::String->Int) $ splitOn "," drawStr
    putStrLn "drawList = "
    print drawList
    
    -- remove empty strings and convert to int
    let gridLines = map (map (read::String->Int) . words) $ filter (not . null) gridsStr
    putStrLn "gridLines = "
    mapM print gridLines
    
    let grids = chunksOf 5 gridLines
    putStrLn "grids = "
    mapM print grids

    -- each grid will contain one list for each line and one for each column
    let grids2 = map (\l -> l ++ transpose l) grids
    putStrLn "grids2 = "
    print grids2

    putStrLn "result = "
    print $ getResult1 grids2 drawList
