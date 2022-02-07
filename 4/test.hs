import Debug.Trace
import Data.List
import Data.List.Split
import System.Environment

data Slot = Crossed | Slot Int deriving(Eq,Show)
complete = replicate 5 Crossed

firstCompleteGrid :: [[[Slot]]] -> Maybe [[Slot]]
firstCompleteGrid [] = Nothing
firstCompleteGrid (grid : grids)
  | gridComplete grid = Just grid
  | otherwise = firstCompleteGrid grids

gridComplete :: [[Slot]] -> Bool
gridComplete g = any (==complete) g

slotScore :: Slot -> Int
slotScore Crossed = 0
slotScore (Slot n) = n

gridScore :: [[Slot]] -> Int
gridScore g = div (sum $ map ( sum . ( map (slotScore))) g) 2

getResult1 :: [[[Slot]]] -> [Int] -> Int
getResult1 grids (draw : draws) =
  case firstCompleteGrid newGrid of
    Just completedGrid -> draw * gridScore completedGrid
    Nothing -> getResult1 newGrid draws
  where newGrid = map ( map ( map ( \n -> if (n==Slot draw) then Crossed else n ))) grids

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
    let gridLines = map (map (Slot . (read::String->Int)) . words) $ filter (not . null) gridsStr
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
