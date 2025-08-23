import Data.List
factorial :: Int -> Int -- dado un entero n ≥ 0, devuelve n!.
factorial 0 = 1
factorial 1 = 1
factorial n = n * factorial (n-1)

sumaN :: Int -> [Int] -> [Int] -- dado un entero k y una lista xs, devuelve la lista que resulta de sumarle k a cada elemento de xs.
sumaN _ [] = []
sumaN k (x:xs) = (k+x) : sumaN k xs

aparece :: Char -> String -> Bool -- dado un caracter c y un string s, devuelve un booleano que indica si c aparece en s.
aparece _ [] = False
aparece c s = c `elem` s

-- Mas en general:
apareceGen :: Eq a => a -> [a] -> Bool
apareceGen _ [] = False
apareceGen c (x:xs)
    | c == x = True
    | otherwise = apareceGen c xs

-- ordenar :: Ord a => [a] -> [a] -- dada una lista, devuelve su permutacion ordenada.

-- ordenar [] = []
-- ordenar (x:xs)
--     | x <= valorMinimo = x : ordenar xs
--     | otherwise = valorMinimo : ordenar (x : delete valorMinimo xs)
--     where
--         valorMinimo = minimum xs

-------------------------------------------------------------
-- TDAs:
-------------------------------------------------------------

data Direccion = Norte | Este | Sur | Oeste deriving Show

opuesta :: Direccion -> Direccion
opuesta Norte = Sur
opuesta Sur = Norte
opuesta Este = Oeste
opuesta Oeste = Este


-- data Maybe a = Nothing | Just a

-- ultimoIndiceDe :: Eq a => a -> [a] -> Maybe Int

