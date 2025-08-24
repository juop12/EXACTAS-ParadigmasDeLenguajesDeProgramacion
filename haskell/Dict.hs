module Dict where

import Data.Maybe (Maybe(Nothing))
import Abb (ABB(Nil), insertar, buscar)

newtype Entry k v = Entry (k, v) deriving Show

instance Eq k => Eq (Entry k v) where
    (==) :: Eq k => Entry k v -> Entry k v -> Bool
    (==) (Entry (k1, _)) (Entry (k2, _)) = k1 Prelude.== k2

instance Ord k => Ord (Entry k v) where
    compare :: Ord k => Entry k v1 -> Entry k v2 -> Ordering
    compare (Entry (k1, _)) (Entry (k2, _)) = compare k1 k2
    -- or define (<), (<=), (>) and (>=) directly

newtype Dict k v = CDict (ABB (Entry k v)) deriving Show

vacio :: Dict k v
vacio = CDict Nil

definir :: Ord k => k -> v -> Dict k v -> Dict k v
definir k v (CDict abb) = CDict (insertar abb (Entry (k, v)))

buscar :: Ord k => k -> Dict k v -> Maybe v
buscar k (CDict abb) =
    case valor_buscado of
        Nothing -> Nothing
        Just (Entry (_, valor)) -> Just valor
    where
        valor_buscado = Abb.buscar abb (Entry (k, undefined))

-- CHANGELOG:
-- 1. Agregado el tipo de dato `Entry`
-- 2. Implementada la instancia `Eq` para `Entry`
-- 3. Implementada la instancia `Ord` para `Entry`
-- 4. Definido el tipo de dato `Dict` como un nuevo tipo que encapsula un ABB de `Entry` 
-- 5. Implementada la función `vacio` para crear un diccionario vacío
-- 6. Implementada la función `definir` para agregar o actualizar entradas en el diccionario
-- 7. Intento de implementación de la función `buscar` para buscar un valor por su clave en el diccionario
-- 8. Uso de `Maybe` para manejar la posibilidad de que una clave no exista
-- 9. Uso de `Entry` para encapsular las claves y valores en el ABB
-- 10. Uso de `Ord` para permitir la ordenación de las entradas en el ABB
-- 11. Uso de `Eq` para permitir la comparación de las entradas en el ABB
-- 12. Uso de `Data.Maybe` para manejar el caso de que una clave no exista en el diccionario
-- 13. Cambio la implementación de Eq y Ord de mi nuevo datatype Entry