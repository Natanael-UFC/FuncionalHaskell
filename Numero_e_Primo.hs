main = do
	putStrLn "Digite um numero:"
	num <- getLine
	if(isPrimo(read num))
		then do
			putStrLn "sim."
		else do
			putStrLn "Não."	
	
	
n_divisores :: Int -> Int -> Int
n_divisores _ 0 = 0
n_divisores a b | mod a b == 0 = 1 + n_divisores a (b - 1)
                | otherwise = n_divisores a (b - 1)
				
isPrimo :: Int -> Bool
isPrimo 0 = False
isPrimo 1 = True
isPrimo n | n_divisores n n == 2 = True
          | otherwise = False