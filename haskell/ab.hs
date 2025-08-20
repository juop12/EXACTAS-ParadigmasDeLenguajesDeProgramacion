module Ab where

data AB a = Nil | Bin (AB a) a (AB a) deriving Show

ab1 = Bin (Bin (Bin Nil 1 Nil) 3 Nil) 5 (Bin (Bin Nil 7 Nil) 8 (Bin Nil 9 Nil))

preorder :: AB a -> [a]
preorder Nil = []
preorder (Bin izq raiz der) = raiz : (preorder izq ++ preorder der)

inorder :: AB a -> [a]
inorder Nil = []
inorder (Bin izq raiz der) = inorder izq ++ (raiz : inorder der)

postorder :: AB a -> [a]
postorder Nil = []
postorder (Bin izq raiz der) = postorder izq ++ (postorder der ++ [raiz])