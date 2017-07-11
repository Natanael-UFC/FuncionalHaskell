import System.Environment
import System.Random

main = do
	gen <- getStdGen
	args <- getArgs
	
	let n = (read(args !! 0))
	
	let (numero_ale, _) = randomR(1, n) gen :: (Int, StdGen)
	
	adivinhe numero_ale (read(args !! 1))
	
adivinhe :: Int -> Int -> IO()
adivinhe numero_al num_v = do 
	if num_v == 0 then do
		putStrLn "Voce perdeu. Suas chances acabaram !"
		return ()
	else do
		putStr "Voce tem "
		putStr $ show num_v
		putStrLn "  chances para acertar o numero."
		putStrLn "Informe um numero: "
		num_info <- getLine	
		let num_inf = read num_info
		if num_inf == numero_al then do
			putStrLn "parabens."
			return ()
		else do
			putStrLn "Errou."
			adivinhe numero_al (num_v - 1)
		
			
			
			
			
			
			
			
	
	
	