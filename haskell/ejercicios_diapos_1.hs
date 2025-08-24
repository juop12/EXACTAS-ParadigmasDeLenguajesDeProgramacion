import Data.List
import Data.Maybe (fromMaybe)

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

subsecuencias :: [a] -> [[a]] -- que dada una lista, devuelva la lista de todas sus posibles subsecuencias. 
subsecuencias [] = [[]]
subsecuencias (x:xs) = let subs = subsecuencias xs in subs ++ Data.List.map (x:) subs 

-- Por ejemplo, las subsecuencias de [1, 2, 3] son:
-- ~> [[], [1], [2], [3], [1, 2], [1, 3], [2, 3], [1, 2, 3]]

permutaciones :: [a] -> [[a]] -- que dada una lista, devuelva la lista de todas sus posibles permutaciones.
permutaciones [] = [[]]
permutaciones (x:xs) = concatMap (insertarEnTodasPosiciones x) (permutaciones xs)
  where
    insertarEnTodasPosiciones x [] = [[x]]
    insertarEnTodasPosiciones x (y:ys) = (x:y:ys) : Data.List.map (y:) (insertarEnTodasPosiciones x ys)

-- Por ejemplo, las permutaciones de [1, 2, 3] son:
-- ~>   [[1, 2, 3], [1, 3, 2], [2, 1, 3]
--       [2, 3, 1], [3, 1, 2], [3, 2, 1]]

(.) :: (a -> b) -> (b -> c) -> a -> c -- Compone 2 funciones. Por ejemplo: ((\x -> x * 4).(\y -> y - 3)) 10 devuelve 28.
-- (.) f g = \x -> g (f x)
(.) f g = g Prelude.. f

flip :: (a -> b -> c) -> b -> a -> c-- Invierte los argumentos de una funcion. Por ejemplo: flip (\x y -> x - y) 1 5 devuelve 4.
flip f y x = f x y

($) :: (a -> b) -> a -> b -- Aplica una funcion a un argumento. Por ejemplo: id $ 6 devuelve 6.
($) f = f

const :: a -> b -> a-- Dado un valor, retorna una funcion constante que devuelve siempre ese valor. Por ejemplo: const 5 ‘‘casa’’ devuelve 5.
const k _ = k

-- Responder:

-- ¿Que hace flip ($) 0?
-- flip ($) 0 toma una función y la aplica al valor 0. Es decir, es la funcion que f(x=0) sea cual sea la funcion f.

-- ¿Y (==0) . (flip mod 2)?
-- A simple vista se ve que es la función esPar implementada con composicion de funciones -- f . g (x) = f(g(x)).
-- Por un lado, (flip mod 2) x = mod x 2 ~> Te devuelve el resto de x divido por 2.
-- Por otro lado, (== 0) es la funcion booleana que te indica si algo es igual a 0.
-- (==0) . (flip mod 2) = (== 0) ((flip mod 2) x)
-- ~> mod x 2 == 0

-------------------------------------------------------------
-- TDAs: Structs y Enums
-------------------------------------------------------------

data Direccion = Norte | Este | Sur | Oeste deriving Show

opuesta :: Direccion -> Direccion
opuesta Norte = Sur
opuesta Sur = Norte
opuesta Este = Oeste
opuesta Oeste = Este

-- data Maybe a = Nothing | Just a

ultimoIndiceDe :: Eq a => a -> [a] -> Maybe Int
ultimoIndiceDe _ [] = Nothing
ultimoIndiceDe y xs = buscar xs 0 Nothing
  where
    buscar [] _ res = res
    buscar (z:zs) i res
      | y == z    = buscar zs (i+1) (Just i)
      | otherwise = buscar zs (i+1) res
-- En esta solucion, se recorre la lista con un indice i y se va actualizando el resultado res cada vez que se encuentra una coincidencia.
-- Al final, se devuelve el resultado res, que sera Nothing si no se encontro ninguna coincidencia, o Just i con el ultimo indice encontrado.
-- La gracia es que se recorre la lista una sola vez mientras te vas guardando el ultimo indice encontrado.

-- Usando Reverse
-- ultimoIndiceDe y xs = Just (length xs - 1 - fromMaybe (-1) (ultimoIndiceDe y (reverse xs)))


-------------------------------------------------------------
-- Generalizando funciones
-------------------------------------------------------------

mejorSegun :: (a -> a -> Bool) -> [a] -> Maybe a
mejorSegun _ [] = Nothing
mejorSegun f (x:xs) = Just (foldl (\acc y -> if f y acc then y else acc) x xs)

maximo :: Ord a => [a] -> Maybe a
maximo = mejorSegun (>)

minimo :: Ord a => [a] -> Maybe a
minimo = mejorSegun (<)

listaMasCorta :: [[a]] -> [a]
listaMasCorta xs = fromMaybe [] (mejorSegun (\a b -> length a < length b) xs)


-- Con Filter
-------------------------------------------------------------

-- filter :: (a -> Bool) -> [a] -> [a]
-- filter [] = []
-- filter p (x:xs) = if p x then x : filter p xs else filter p xs

deLongitudN :: Int -> [[a]] -> [[a]]
deLongitudN n = Data.List.filter (\x -> length x == n)

soloPuntosFijosEnN :: Int -> [Int->Int] -> [Int->Int] -- Dados un numero n y una lista de funciones, deja las funciones que al aplicarlas a n dan n.
soloPuntosFijosEnN n = Data.List.filter (\x -> x n == n)

-- Con Map
-------------------------------------------------------------

-- map :: (a -> b) -> [a] -> [b]
-- map [] = []
-- map f (x:xs) = f x : map f xs

reverseAnidado :: [[Char]] -> [[Char]] -- dada una lista de strings, devuelve una lista con cada string dado vuelta y la lista completa dada vuelta. Por ejemplo: reverseAnidado [‘‘quedate’’, ‘‘en’’, ‘‘casa’’] devuelve [‘‘asac", ‘‘ne’’, ‘‘etadeuq’’]. Ayuda: ya existe la funci´on reverse que invierte una lista.
reverseAnidado = Data.List.map reverse Prelude.. reverse

paresCuadrados :: [Int] -> [Int] -- dada una lista de enteros, devuelve una lista con los cuadrados de los numeros pares y los impares sin modificar.
paresCuadrados = Data.List.map (\x -> if esPar x then x * x else x)
  where
    esPar = (==0) Prelude.. Prelude.flip mod 2

-- Con Map y Filter
-------------------------------------------------------------

-- listaComp :: (a -> Bool) -> (a -> b) -> [a] -> [b]
-- listaComp p f xs = [f x | x <- xs, p x]

listaComp :: (a -> Bool) -> (a -> b) -> [a] -> [b]
listaComp p f xs = Prelude.map f (Prelude.filter p xs) -- Recorro 2 veces la lista, medio feo. Con un map y una condicion por cada map se resuelve.

-- Redefiniendo Map y Filter con Foldr
-------------------------------------------------------------

filter :: (a -> Bool) -> [a] -> [a]
filter p = foldr (\x r -> if p x then x:r else r) []

map :: (a -> b) -> [a] -> [b]
map f = foldr (\x r -> f x : r) []

-------------------------------------------------------------
-- Currying
-------------------------------------------------------------

curry :: ((a,b) -> c) -> (a -> b -> c) -- que devuelve la version currificada de una funcion no currificada.
curry = undefined

uncurry :: (a -> b -> c) -> ((a,b) -> c) -- que devuelve la vbersion no currificada de una funcion currificada. Los parentesis de la segunda funcion no son necesarios.
uncurry = undefined

-------------------------------------------------------------
-- Ordenar
-------------------------------------------------------------

-- ordenar :: Ord a => [a] -> [a] -- dada una lista, devuelve su permutacion ordenada.

-- ordenar [] = []
-- ordenar (x:xs)
--     | x <= valorMinimo = x : ordenar xs
--     | otherwise = valorMinimo : ordenar (x : delete valorMinimo xs)
--     where
--         valorMinimo = minimum xs

