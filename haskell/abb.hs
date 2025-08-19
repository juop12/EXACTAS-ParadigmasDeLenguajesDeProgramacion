{- DEF data structure -}
data (Ord a, Eq a) => ABB a = Nil | Bin (ABB a) a (ABB a) 
	deriving Show

ab1 = Bin (Bin (Bin Nil 1 Nil) 3 Nil) 5 (Bin (Bin Nil 7 Nil) 8 (Bin Nil 9 Nil))

preorder :: ABB a -> [a]
preorder Nil = []
preorder (Bin izq raiz der) = raiz : (preorder izq ++ preorder der)

inorder :: ABB a -> [a]
inorder Nil = []
inorder (Bin izq raiz der) = inorder izq ++ (raiz : inorder der)

postorder :: ABB a -> [a]
postorder Nil = []
postorder (Bin izq raiz der) = postorder izq ++ (postorder der ++ [raiz])