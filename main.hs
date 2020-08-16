import System.IO
import Data.Char
import Data.List

main :: IO ()
main = do
    --putStrLn "Digite el nombre del archivo con los textos:"
    --titulos <- getLine
    --putStrLn "Digite el nombre con el archivo con palabras no significativas:"
    --palabrasNS <- getLine
    putStrLn "Digite el nombre con el archivo de salida:"
    output <- getLine
    handleTitulos <- openFile "titulosIngles.txt" ReadMode
    contentTitulos <- hGetContents handleTitulos
    handlePalabrasNS <- openFile "noSignificativasIngles.txt" ReadMode
    contentPalabrasNS <- hGetContents handlePalabrasNS

    let palabrasNS = lines contentPalabrasNS
    let texto = lines contentTitulos
        titulos = map (split ' ') texto
        titulosLower = [[map toLower palabra | palabra <- titulo] | titulo <- titulos]
        pSignificativas = sort (deleteDuplicate (concat (aplicarFiltro palabrasNS titulosLower)))
        listaRenovadas = filter (not.null) (aplicarAll pSignificativas titulosLower)
        hola = func ["Hola"]

    --print titulos
    --print titulosLower
    print hola
    --mapM_ (print . unwords) listaRenovadas
    --writeFile output (listaRenovadas)
    hClose handleTitulos
    hClose handlePalabrasNS

split :: Eq a => a -> [a] -> [[a]]
split d [] = []
split d s = x : split d (drop 1 y) where (x,y) = span (/= d) s


aplicarFiltro :: [String] -> [[String]] -> [[String]]
aplicarFiltro palabrasNS titulos
  = foldl (\ titulos palabraNS -> map (filter (/= palabraNS)) titulos) titulos palabrasNS

aplicarKWIC :: String -> [String] ->  [String]
aplicarKWIC palabra titulo = do
    let indice = elemIndices palabra titulo
    let kwic = if indice == [] then [] else take (head indice) titulo ++[map toUpper (titulo !! head indice)] ++ drop (head indice + 1 ) titulo
    kwic

aplicarKWICSimple :: String -> [String] ->  [String]
aplicarKWICSimple palabra titulo = do
    let indice = elemIndices palabra titulo
    let kwic = if indice == [] then [] else drop (head indice ) titulo ++ ["><"] ++ take (head indice) titulo
    kwic

aplicarForza :: String -> [[String]] -> [[String]]
aplicarForza palabraS titulos = filter (not.null) (map (aplicarKWIC palabraS) titulos)

aplicarAll :: [String] -> [[String]]-> [[String]]
aplicarAll [] _ = [[]]
aplicarAll (palabraS:palabrasS) titulos = aplicarForza palabraS titulos ++ aplicarAll palabrasS titulos

deleteDuplicate :: (Eq a) => [a] -> [a]
deleteDuplicate [] = []
deleteDuplicate (elemento:lista) = elemento : deleteDuplicate (filter (/= elemento) lista)

func :: [String] -> [[String]]
func lista = do
  let recorre p=if p<=50 then ["Elemento "]: [lista] else [["-"]]
  let cantidad_elementos= 0
  let salida=recorre cantidad_elementos
  salida
