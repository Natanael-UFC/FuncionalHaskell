main = do 
	qtd_elefantes <- getLine
	if(read qtd_elefantes) <= 1
		then do 
			putStrLn $ qtd_elefantes ++ "elefante não incomodam."
			return ()
		else do
			imprime[2..(read qtd_elefantes)]
			
imprime :: [Int] -> IO () 
imprime [a] = return ()
imprime (x:xs) = do 
	putStrLn $ "Se " ++ show x ++ " elefantes incomodam muita gente..."
	putStrLn $ show(x + 1) ++ " incomodam muito mais."
	imprime xs
			