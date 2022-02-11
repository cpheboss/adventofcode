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

