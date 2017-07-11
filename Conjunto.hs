import Data.List

data Conj a = Conj [a] deriving Eq
instance (Show a) => Show (Conj a) where
    showsPrec _ (Conj s) cad = showConj s cad
 
showConj []     cad = showString "{}" cad
showConj (x:xs) cad = showChar '{' (shows x (showl xs cad))
     where showl []     cad = showChar '}' cad
           showl (x:xs) cad = showChar ',' (shows x (showl xs cad))

vazio :: Conj a
vazio = Conj []

isVazio :: Conj a -> Bool
isVazio (Conj []) = True
isVazio _ = False

pertence :: Ord a => a -> Conj a -> Bool
pertence x (Conj s) = elem x (takeWhile (<= x) s)

elimina :: Ord a => a -> Conj a -> Conj a
elimina x (Conj s) = Conj (elimina_conj x s)
	where elimina_conj x [] = []
	      elimina_conj x s@(y:ys) | (x > y) = y : elimina_conj x ys
		                          | (x < y) = s
							      | otherwise = ys
							 
insere :: Ord a => a -> Conj a -> Conj a
insere x (Conj s) = Conj (insere_elem x s)
    where insere_elem x []                    = [x]                
          insere_elem x s@(y:ys) | (x > y)    = y : (insere_elem x ys)
                                 | (x < y)    = x : s
                                 | otherwise  = s
		  
 

