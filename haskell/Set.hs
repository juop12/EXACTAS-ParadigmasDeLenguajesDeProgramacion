import Data.List (delete)

newtype Conj a = CConj [a]

instance Show a => Show (Conj a) where
    show :: Conj a -> String
    show (CConj [x]) = "{" ++ show x ++ "}"
    show (CConj (x:xs)) = "{" ++ show x ++ ", " ++ drop 1 (show (CConj xs)) 
    show (CConj []) = "{}"

vacio :: Conj a
vacio = CConj []

insertar :: Ord a => a -> Conj a -> Conj a
-- Esta insercion en un Set es NO ordenada
-- insertar v (CConj lista) = CConj (if v `elem` lista then lista else insertar_ordenado v lista)
insertar v (CConj lista) = CConj (insertar_en_set_ordenado v lista)
    where
        insertar_en_set_ordenado :: Ord a => a -> [a] -> [a]
        insertar_en_set_ordenado x [] = [x]
        insertar_en_set_ordenado x (y:ys)
            | x < y = x : (y : ys)
            | x > y = y : insertar_en_set_ordenado x ys
            | x == y = y : ys

pertenece :: Ord a => a -> Conj a -> Bool
pertenece v (CConj lista) = v `elem` lista

eliminar :: Ord a => a -> Conj a -> Conj a
eliminar v (CConj lista) = CConj (delete v lista)

-- CHANGELOG:
-- 1. Armo la función Vacio.
-- 2. Se insertan elementos en el Set.
-- 3. Agrego implementacion de funcion pertenece.
-- 4. Se puede eliminar del Set.
-- 5. Se inserta en el Set ordenadamente.
