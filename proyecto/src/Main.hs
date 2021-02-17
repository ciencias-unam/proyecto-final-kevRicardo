module Main (main) where

    import System.IO
    import Data.List.Split
    
    main :: IO ()
    main = do
        putStrLn "Hola. Ingresa el nombre del archivo a procesar"
        file <- getLine
        graph <- lector file
        putStrLn $ head graph

    lector :: FilePath -> IO [String]
    lector file =
        do
            archivo <- readFile file
            return $ splitOn "\n" archivo