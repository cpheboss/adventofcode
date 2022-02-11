import Data.Char                                                                                                        
import Data.List                                                                                                        
import Data.List.Split                                                                                                  
import System.Environment                                                                                               
                                                                                                                        
import Debug.Trace                                                                                                      

main = do                                                                                                               
    file:args <- getArgs                                                                                                
    putStrLn $ "Reading " ++ file                                                                                       
    line <- readFile file                                                                                               
    putStrLn "line = "                                                                                                  
    print line                                                                                                          

    let splitLines = map ((splitAt 10) . words . filter (/='|')) $ lines line
    putStrLn "splitLines ="
    print splitLines

    let patterns = map fst splitLines
    let digits = map snd splitLines

    putStrLn "patterns = "
    print patterns
    putStrLn "digits = "
    print digits

    let result1 = sum $ map ( length . filter (\s -> length s `elem` [2,3,4,7])) digits
    putStrLn "result1 = "
    print result1
