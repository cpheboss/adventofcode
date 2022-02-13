import System.Environment

main = do
    file:args <- getArgs
    putStrLn $ "Reading " ++ file
    line <- readFile file
    putStrLn "line = "
    print line

