import System.IO
import Data.Char
import Data.List

main :: IO ()
main = do
    --putStrLn "Digite el nombre del archivo con los textos:"
    --titulos <- getLine
    putStrLn "Digite el nombre con el archivo con palabras no significativas:"
    palabrasNS <- getLine
    putStrLn "Digite el nombre con el archivo de salida:"
    output <- getLine
    handleTitulos <- openFile "titulos.txt" ReadMode
    contents <- hGetContents handleTitulos
    let todoTasks = lines contents     
        --numberedTasks = zipWith (\n line -> "como") [0..] todoTasks
        separarPalabras = map (split ' ') todoTasks
        --editarPalabra = zipWith (\n line -> line ++ "pollo\n") [0..] (map unlines separarPalabras)
        --editarPalabra = filter (=="airplane") (map unlines separarPalabras)
        --editarPalabra = (map (=="airplane") separarPalabras)

    putStrLn ""
    print (map (filter (=="teapot")) separarPalabras)
    print separarPalabras
    --putStrLn $ unlines editarPalabra
    --putStrLn $ unlines (map unlines separarPalabras)
    writeFile output contents
    hClose handleTitulos


split :: Eq a => a -> [a] -> [[a]]
split d [] = []
split d s = x : split d (drop 1 y) where (x,y) = span (/= d) s