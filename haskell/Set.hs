import Data.List (delete)

newtype Conj a = CConj [a] deriving Show

vacio :: Conj a
vacio = CConj []

insertar :: Eq a => a -> Conj a -> Conj a
insertar v (CConj lista) = CConj (if v `elem` lista then lista else v : lista)

pertenece :: Eq a => a -> Conj a -> Bool
pertenece v (CConj lista) = v `elem` lista

eliminar :: Eq a => a -> Conj a -> Conj a
eliminar v (CConj lista) = CConj (delete v lista)

-- Show??

-- CHANGELOG:
-- 1. Armo la función Vacio.
-- 2. Se insertan elementos en el Set.
-- 3. Agrego implementacion de funcion pertenece.
-- 4. Se puede eliminar del Set.
