
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
    | valor == raiz = quitar_raiz (Bin izq raiz der)

quitar_raiz :: (Ord a) => ABB a -> ABB a
quitar_raiz (Bin Nil _ der) =  der
quitar_raiz (Bin izq _ Nil) =  izq
quitar_raiz (Bin izq _ der) =  Bin nuevo_izq nuevo_valor der -- Necesito borrar el elemento que busco en el abb izq
    where
        (nuevo_izq, nuevo_valor) = extraer_maximo izq

extraer_maximo :: (Ord a) => ABB a -> (ABB a, a)
extraer_maximo (Bin izq raiz Nil) = (izq, raiz) 
extraer_maximo (Bin izq raiz der) = (Bin izq raiz nueva_der, nuevo_valor)
    where 
        (nueva_der, nuevo_valor) = extraer_maximo der

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