
-- Defino el tipo de dato abstracto (TDA) de Arbol Binario de Busqueda
data ABB a = Nil | Bin (ABB a) a (ABB a) deriving Show

abb1 = Bin (Bin (Bin Nil 1 Nil) 3 Nil) 5 (Bin (Bin Nil 7 Nil) 8 (Bin Nil 9 Nil))
abb2 = Bin (Bin Nil -1 (Bin Nil 0 (Bin Nil 1 Nil))) 1 (Bin (Bin Nil 2 Nil) 3 Nil)

insertar :: (Ord a) => ABB a -> a -> ABB a
insertar Nil valor = Bin Nil valor Nil
insertar (Bin izq raiz der) valor 
    | valor <= raiz = Bin (insertar izq valor) raiz der
    | valor > raiz = Bin izq raiz (insertar der valor)
 -- | otherwise = Bin l y r -- x == y, no change needed

preorder :: ABB a -> [a]
preorder Nil = []
preorder (Bin izq raiz der) = raiz : (preorder izq ++ preorder der)

inorder :: ABB a -> [a]
inorder Nil = []
inorder (Bin izq raiz der) = inorder izq ++ (raiz : inorder der)

postorder :: ABB a -> [a]
postorder Nil = []
postorder (Bin izq raiz der) = postorder izq ++ (postorder der ++ [raiz])

-- CHANGELOG:
    -- Los constarints al tipo de dato dependiente de un generico van en las implementaciones de las funciones, no en las definiciones de los TDAs.
    -- El Impl de Ord ya contiene Eq por defecto.
    -- Si inserto un elemento ya existente, lo inserto como predecesor inorder.
    -- Si empiezo insertando Enteros, puedo insertar un Float luego y lo transforma.