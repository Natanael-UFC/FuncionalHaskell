module Stack(Stack, push, pop, top, isEmpty) where 
	push :: a -> Stack a -> Stack a
	pop :: Stack a -> Maybe (Stack a)
	top :: Stack a -> a
	isEmpty :: Stack a -> Bool
	
	data Stack a = Empty | Top a (Stack a)

	push x y = Top x y

	isEmpty Empty = True
	isEmpty _ = False

	pop Empty = Nothing
	pop (Top _ p) = Just p
	
	top Empty = error "Vazia"
	top (Top x _) = x

	instance (Show a) => Show (Stack a) where
		show (Empty) = "#"
		show (Top x s) = (show x) ++ "|" ++ (show s)
		

	
	
	
	
	
	
	
	

