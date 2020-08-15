import System.IO



main = do
    putStrLn "Digite un nombre Plebe:"
    texto <- getLine
    putStrLn "Digite otro nombre Plebe:"
    output <- getLine
    handle <- openFile texto ReadMode
    contents <- hGetContents handle
    writeFile output contents
    putStrLn contents
    hClose handle