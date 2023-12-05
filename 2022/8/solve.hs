import Data.Char
import Data.List
import Data.List.Split
import Data.Maybe
import System.Environment
import qualified Data.Map as Map

import Data.Typeable
import Debug.Trace

mapNumbers (a:b:c:d:e:f:g:[]) = Map.fromList [
    (sort [a,b,c,e,f,g],   0),
    (sort [c,f],           1),
    (sort [a,c,d,e,g],     2),
    (sort [a,c,d,f,g],     3),
    (sort [b,c,d,f],       4),
    (sort [a,b,d,f,g],     5),
    (sort [a,b,d,e,f,g],   6),
    (sort [a,c,f],         7),
    (sort [a,b,c,d,e,f,g], 8),
    (sort [a,b,c,d,f,g],   9)]

main = do
    file:args <- getArgs
    putStrLn $ "Reading " ++ file
    line <- readFile file
    putStrLn "line = "
    print line

    let splitLines = map ((splitAt 10) . words . filter (/='|')) $ lines line
    putStrLn "splitLines ="
    print splitLines

    -- sort each string characters and split patterns / digits. Also sort patterns alphabeticallt
    let patterns = map(sort . map(concat . sort . map(:[])) . fst) splitLines
    let digits = map(map(concat . sort . map(:[])) . snd) splitLines

    putStrLn "patterns = "
    print patterns
    putStrLn "digits = "
    print digits

    let result1 = sum $ map ( length . filter (\s -> length s `elem` [2,3,4,7])) digits
    putStrLn "result1 = "
    print result1

    let allPermNumbers = map mapNumbers $ permutations "abcdefg"
    -- mapM_ (print . Map.elems) $ take 100 allPermNumbers

    -- allPerms is a Map [String] (Map String Int). For a given set of coded numbers, it gives the Map of each value for
    -- each number
    let allPerms = Map.fromList $ map (\l -> (Map.keys l, l)) allPermNumbers
    mapM_ print $ take 10 $ Map.keys allPerms
    mapM_ print $ take 10 $ Map.elems allPerms

    -- For each pattern, we want the encoded digits
    -- => 1. Get its decoder
    -- => 2. For each coded digit get its value
    let digitsAndDecodeKeys = zip digits $ map (fromJust . flip Map.lookup allPerms) patterns
    mapM_ print $ take 10 digitsAndDecodeKeys
    
    let decodedDigits = map (\(dig,decoder) -> map (fromJust . flip Map.lookup decoder) dig) digitsAndDecodeKeys
    print decodedDigits

    -- => 3. Join them into one number foldl1 (\x y -> 10*x+y) [1,2,3]
    -- => 4. Sum them
    print $ sum $ map (foldl1 (\x y -> 10*x+y)) decodedDigits

