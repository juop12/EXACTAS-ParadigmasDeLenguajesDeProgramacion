module Abb where

-- Defino el tipo de dato abstracto (TDA) de Arbol Binario de Busqueda
data ABB a = Nil | Bin (ABB a) a (ABB a) deriving Show

abb1 = Bin (Bin (Bin Nil 1 Nil) 3 Nil) 5 (Bin (Bin Nil 7 Nil) 8 (Bin Nil 9 Nil))
abb2 = Bin (Bin Nil (-1) (Bin Nil 0 (Bin Nil 1 Nil))) 1 (Bin (Bin Nil 2 Nil) 3 Nil)

insertar :: (Ord a) => ABB a -> a -> ABB a
insertar Nil valor = Bin Nil valor Nil
insertar (Bin izq raiz der) valor
    | valor <= raiz = Bin (insertar izq valor) raiz der
    | valor > raiz = Bin izq raiz (insertar der valor)
 -- | otherwise = Bin l y r -- x == y, no change needed

-- Reemplazo el elemento quitado con el predecesor inorder del ABB.
quitar :: (Ord a) => ABB a -> a -> ABB a
quitar Nil _ = Nil
quitar (Bin izq raiz der) valor
    | valor < raiz = Bin (quitar izq valor) raiz der
    | valor > raiz = Bin izq raiz (quitar der valor)
    | valor == raiz = quitarRaiz (Bin izq raiz der)

quitarRaiz :: (Ord a) => ABB a -> ABB a
quitarRaiz (Bin Nil _ der) =  der
quitarRaiz (Bin izq _ Nil) =  izq
quitarRaiz (Bin izq _ der) =  Bin nuevo_izq nuevo_valor der -- Necesito borrar el elemento que busco en el abb izq
    where
        (nuevo_izq, nuevo_valor) = extraerMaximo izq

extraerMaximo :: (Ord a) => ABB a -> (ABB a, a)
extraerMaximo (Bin izq raiz Nil) = (izq, raiz)
extraerMaximo (Bin izq raiz der) = (Bin izq raiz nueva_der, nuevo_valor)
    where
        (nueva_der, nuevo_valor) = extraerMaximo der

buscar :: (Ord a) => ABB a -> a -> Maybe a
buscar Nil _ = Nothing
buscar (Bin Nil raiz Nil) valor
    | raiz /= valor = Nothing
    | raiz == valor = Just raiz
buscar (Bin izq raiz der) valor
    | raiz > valor = buscar izq valor
    | raiz < valor = buscar der valor
    | raiz == valor = Just raiz

mapABB :: (Ord a, Ord b) => (a -> b) -> ABB a -> ABB b
mapABB _ Nil = Nil
mapABB f (Bin izq raiz der) = Bin (mapABB f izq) (f raiz) (mapABB f der)

foldABB :: (Ord a) => (a -> b -> b) -> b -> ABB a -> b
foldABB _ i Nil = i
foldABB f i (Bin izq raiz der) = f raiz (foldABB f (foldABB f i izq) der)

-- factorial 5
-- abb2 = (Bin (Bin Nil 2 (Bin Nil 1 Nil)) 3 (Bin (Bin Nil 4 Nil) 5 Nil))
-- factorial 5 ~> foldABB (*) 1 abb2 ~> 120

-- abb1 = (Bin (Bin Nil 0 (Bin Nil 1 Nil)) 3 (Bin (Bin Nil 4 Nil) 5 Nil))
-- foldABB (+) 0 abb1 ~> 13

-- (Bin (Bin Nil 0 (Bin Nil 1 Nil)) 3 (Bin (Bin Nil 4 Nil) 5 Nil))
-- foldABB (+) 0 (Bin Nil 1 Nil)) 
-- ~> (+) 1 (foldABB (+) (foldABB (+) 0 Nil) Nil)
-- ~> (+) 1 (foldABB (+) (0) Nil)
-- ~> (+) 1 (0)
-- ~> 1

arrayToABB :: (Ord a) => [a] -> ABB a
arrayToABB = foldl insertar Nil

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
    -- Agrego funcion quitar.
    -- Fue un dolor de cabeza pensar tan recursivamente con Haskell. Estoy por estallar
    -- La gracia del ejercicio de quitar es reemplazar el nodo o valor quitado y conservar los hijos actualizados. Es decir, en este caso el elemento eliminado se tiene que reemplazar con el más grande de su arbol Izq. Para esto se busca el elemento mas grande de dicho arbol y se lo reemplaza en el nodo que se va a eliminar. Además, se actualiza el arbol Izq para borrar este nodo. Si el arbol Izq es Nil, se reemplaza con el arbol Der. 
    -- Mejoro el TDA haciendo que al buscar reemplazar el nodo a eliminar con el predecesor inorder, con 1 sola busqueda se pueda actualizar el arbol Izq eliminando correctamente el precedesor inorder de este y conseguir el valor del predecesor inorder.
    -- Agrego la funcion buscar, que devuelve un Maybe a.
    -- Agrego función para convertir un array en un ABB usando foldl.
    -- Agrego funciones para hacer un map de los elementos del ABB.
    -- Agrego funciones para hacer un fold/reduce de los elementos del ABB.