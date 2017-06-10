--Obter os N primeiros termos de uma lista
primeiros :: Int -> [Int] -> [Int]
primeiros 0 _ = []
primeiros _ [] = []
primeiros n (a:x) = a : primeiros(n - 1) x

--Verifica se um objeto pertence a lista
pertence :: Int -> [Int] -> Bool
pertence p [] = False
pertence p (a:x) | p == a = True
                 | otherwise = pertence p x
				 
--Adicionar um objeto na lista, sem repetição
insere :: Int -> [Int] -> [Int]
insere c [] = [c]
insere c (a:x) | c == a = a:x
               | otherwise = a : insere c x
			   
--Maior valor da lista
maior :: [Int] -> Int
maior [a] = a
maior (a:x) | a > (maior x) = a
            | otherwise = maior x
			
--Remove o ultimo elemento de uma lista
remove :: [Int] -> [Int]
remove [_] = []
remove (a:x)  = a : remove x

--Gerando uma lista apenas com numeros pares
par :: Int -> Bool
par x = mod x 2 == 0

constroi_lista :: [Int]
constroi_lista = [x | x <- [0..100], par x]
			   
--Função zip
zipa :: [a] -> [b] -> [(a, b)]
zipa _ [] = []
zipa [] _ = []
zipa (a:b) (x:y) = (a, x) : zipa b y

--Ordenar uma lista
menor :: [Int] -> Int
menor [a] = a
menor (a:x) | a < (menor x) = a
            | otherwise = menor x
			
remove_lista :: [Int] -> [Int]
remove_lista [a] = []
remove_lista (a:x) | a == (menor (a:x)) = x
                   | otherwise = a : remove_lista x
				   
ordena_lista :: [Int] -> [Int]
ordena_lista [] = []
ordena_lista [a] = [a]
ordena_lista (a:x) = (menor (a:x)) : (ordena_lista (remove_lista (a:x)))

--Concatenar duas listas
concatena :: [a] -> [a] -> [a]
concatena [] a = a
concatena (a:x) y = a : concatena x y
			   
--Inverter uma lista
inverte :: [a] -> [a]
inverte [] = []
inverte (a:x) = concatena (inverte x) [a]

--Função encontra o primeiro numero inteiro de uma lista
n_divisores :: Int -> Int -> Int
n_divisores a 1 = 1
n_divisores a b | (mod a b == 0) = 1 + n_divisores a (b - 1)
                | otherwise = n_divisores a (b - 1)
				
eh_primo :: Int -> Bool
eh_primo 0 = False
eh_primo 1 = True
eh_primo n | (n_divisores n n == 2) = True
           | otherwise = False
		   
primeiro_primo :: [Int] -> Int
primeiro_primo (a:x) | eh_primo a = a
                     | otherwise = primeiro_primo x
					 
--Exercicios Propostos
--Questão 1
maior_elemento :: [Int] -> Int
maior_elemento [a] = a
maior_elemento(a:x) | a > (maior_elemento x) = a
                    | otherwise = maior_elemento x
				
conta_pos :: [Int] -> Int
conta_pos (a:x) | a == maior_elemento(a:x) = 1
                | otherwise = 1 + conta_pos x

maior_pos :: [Int] -> (Int, Int)
maior_pos [a] = (a, 1)
maior_pos (a:x) = (maior_elemento(a:x), conta_pos(a:x))
			   
--Questão 4
del_pos_n :: [Int] -> Int -> [Int]
del_pos_n [a] 0 = [a]
del_pos_n [] _ = []
del_pos_n (a:x) n | n == 1 = x
                  | otherwise = a : del_pos_n x (n - 1)
			   
--Questão 5
eh_impar :: Int -> Bool
eh_impar x | mod x 2 == 0 = False
           | otherwise = True
		   
impares :: [Int] -> [Int]
impares [] = []
impares (a:x) | eh_impar(a) = ordena_lista(a : impares x)
              | otherwise = impares x
			   
--Questão
multiplos a b x | (b <= a) || (b - a == 1) = 0
                | mod(b - 1) x == 0 = 1 + multiplos a (b - 1) x
			    | mod(b - 1) x /= 0 = 0 + multiplos a (b - 1) x
				
multiplo n1 n2 n3 = [x | x <- [n1..n2], mod x n3 == 0]

fibonacci n = [fib a | a <- [0..n-1]]
	where
		fib 0 = 0
		fib 1 = 1
		fib n = fib (n-1) + fib (n-2)
			   
			   
			   
			   
			   
			   
			   
			   
			   
			   
			   
			   
			   
			   