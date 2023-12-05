import Data.List.Split
import           Data.Char(digitToInt)
import           Data.List
import qualified Data.Vector as V
import           System.Environment

compare3 :: [Int] -> [Int]
compare3 (a:b:[] ) = []
compare3 (a:b:c:s) = [compA + compC] ++ compare3 (b:c:s)
  where compA = if a <= b then 1 else 0
        compC = if c <= b then 1 else 0

surround9 l = [9] ++ l ++ [9]

main = do
    file:args <- getArgs
    putStrLn $ "Reading " ++ file
    line <- readFile file
    putStrLn "line = "
    print line

    let rawLines = lines line

    let intLines = map (map digitToInt) rawLines
    putStrLn "intLines ="
    mapM_ print $ intLines

    let depthsByWidth = map (compare3 . surround9) intLines

    let depthsByHeight = map (compare3 . surround9) $ transpose intLines

    let lowerNeighbours = map (\(l1,l2) -> zipWith (+) l1 l2) $ zip depthsByWidth $ transpose depthsByHeight

    putStrLn "result 1 ="
    print $ sum $ zipWith (\x y -> if y > 0 then 0 else x+1) (concat intLines) (concat lowerNeighbours)
