main = do
	putStrLn "Digite um numero"
	n <- getLine
	putStrLn $ show $ fatorial(read n)	
	
fatorial :: Int -> Int
fatorial 0 = 1
fatorial n = n * fatorial(n - 1)