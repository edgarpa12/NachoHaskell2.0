-- aplicarFiltro :: [String] -> [[String]] -> [[String]]
-- aplicarFiltro [] titulo = titulo
-- aplicarFiltro (x:xs) titulo = aplicarFiltro (xs) (map (filter (/= x)) titulo)


aplicarForza :: String -> [[String]] -> [[String]]
aplicarForza palabraS titulos = filter (not.null)  map (aplicarKWIC (palabraS)) titulos

aplicarAll :: [[String]] -> [[String]]-> [[String]]
aplicarAll (x:xs) titulos = aplicarForza x titulos ++ aplicarAll xs titulos
