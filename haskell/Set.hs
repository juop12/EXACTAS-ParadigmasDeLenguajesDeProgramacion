newtype Conj a = CConj [a]

vacio :: Conj a
vacio = CConj []

insertar :: Eq a => a -> Conj a -> Conj a
insertar v (CConj lista) = CConj (if v `elem` lista then lista else v : lista)

-- pertenece :: Eq a => a -> Conj a -> Bool

-- eliminar :: Eq a => a -> Conj a -> Conj a

-- Show??

-- CHANGELOG:
-- 1. Armo la función Vacio.
-- 2. Se insertan elementos en el Set
