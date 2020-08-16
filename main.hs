import System.IO
import Data.Char
import Data.List

main :: IO ()
main = do
    -- Se pide el nombre del archivo con los titulos
    putStrLn "Digite el nombre del archivo con los textos:"
    titulos <- getLine
    -- Se pide el nombre del archivo con las palabras no significativas
    putStrLn "Digite el nombre con el archivo con palabras no significativas:"
    palabrasNS <- getLine
    -- Se pide el nombre del archivo donse se va a guardar 
    putStrLn "Digite el nombre con el archivo de salida:"
    output <- getLine

    -- Agarra el contenido de los primeros dos inputs
    handleTitulos <- openFile titulos ReadMode
    contentTitulos <- hGetContents handleTitulos
    handlePalabrasNS <- openFile palabrasNS ReadMode
    contentPalabrasNS <- hGetContents handlePalabrasNS

    -- Los convierte a listas de Strings
    let palabrasNS = lines contentPalabrasNS
    let texto = lines contentTitulos
        -- Separa el titulo por palabras
        titulos = map (split ' ') texto
        -- Hace minuscucla cada palabra del titulo de todos los titulos
        titulosLower = [[map toLower palabra | palabra <- titulo] | titulo <- titulos]
        -- Saca las palabras significativas de cada titulo
        -- Une todas las palabras significativas, borra las duplicadas
        -- Las ordena alfabeticamente
        pSignificativas = sort (deleteDuplicate (concat (aplicarFiltro palabrasNS titulosLower)))
        -- Evalua cada palabraSignificativa en cada titulo y elimina donde hay repetidos
        -- Para cada KWIC en especifico
        listaKWICSimple = filter (not.null) (aplicarAllSimple pSignificativas titulosLower)
        listaKWICAlineada = filter (not.null) (aplicarAllAlineado pSignificativas titulosLower)
        -- Se preparan para el output file
        listaKWICSimpleIO = "KWIC Simple\n" : map unwords listaKWICSimple ++ ["\n----------------------------------------------------\n"]
        listaKWICAlineadoIO = "KWIC Alineado\n" : map unwords listaKWICAlineada


    -- Se unen las dos listas ya preparadas
    let listaKWICFinal = map (split ' ') listaKWICSimpleIO ++ map (split ' ') listaKWICAlineadoIO
    -- Se unen para que queden como una oracion
    writeFile output (unlines (map unwords listaKWICFinal))
    -- Se cierra el acceso a los archivos consultados
    hClose handleTitulos
    hClose handlePalabrasNS

-- Divide una lista por un parametro especificado
split :: Eq a => a -> [a] -> [[a]]
split separador [] = []
split separador lista = x : split separador (drop 1 y) where (x,y) = span (/= separador) lista

-- Quita todas las palabras no significativas de un titulo
aplicarFiltro :: [String] -> [[String]] -> [[String]]
aplicarFiltro palabrasNS titulos
  = foldl (\ titulos palabraNS -> map (filter (/= palabraNS)) titulos) titulos palabrasNS

-- Pone el mayuscula la palabra Significativa que va a usar el titulo para el KWIC
aplicarKWICAlineado :: String -> [String] ->  [String]
aplicarKWICAlineado palabra titulo = do
    let indice = elemIndices palabra titulo
    let kwic = if indice == [] then [] else alinear(take (head indice) titulo) ++[map toUpper (titulo !! head indice)] ++ drop (head indice + 1 ) titulo
    kwic

-- Pone el titulo en el formato del profesor para KWIC
aplicarKWICSimple :: String -> [String] -> [String]
aplicarKWICSimple palabra titulo = do
    let indice = elemIndices palabra titulo
    let kwic = if indice == [] then [] else drop (head indice ) titulo ++ ["><"] ++ take (head indice) titulo
    kwic

-- Le aplica KWICSimple a todos los titulos con todas las palabras significativas
aplicarAllSimple :: [String] -> [[String]] -> [[String]]
aplicarAllSimple [] _ = [[]]
aplicarAllSimple (palabraS:palabrasS) titulos = filter (not . null) (map (aplicarKWICSimple palabraS) titulos) ++ aplicarAllSimple palabrasS titulos

-- Le aplica KWICAlineado a todos los titulos con todas las palabras significativas
aplicarAllAlineado :: [String] -> [[String]] -> [[String]]
aplicarAllAlineado [] _ = [[]]
aplicarAllAlineado (palabraS:palabrasS) titulos = filter (not . null) (map (aplicarKWICAlineado palabraS) titulos) ++ aplicarAllAlineado palabrasS titulos

-- Elimina los duplicados de una lista
deleteDuplicate :: (Eq a) => [a] -> [a]
deleteDuplicate [] = []
deleteDuplicate (elemento:lista) = elemento : deleteDuplicate (filter (/= elemento) lista)

-- Le pone el formato deseado para el KWICAlineado
alinear :: [String] -> [String]
alinear lista = do
    let recorre p = if p > 0 then [""] ++ recorre(p-1) else lista
    let salida =  (recorre ( 60 - ((length (concat  lista)) + length lista)))
    salida