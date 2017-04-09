								--Resolução da lista de exercicos 1, prof Ricardo -- UFC
-- Questão 3 -> Fatorial
fatorial :: Integer -> Integer
fatorial 1 = 1
fatorial 0 = 1
fatorial n = n * fatorial(n - 1)

-- Questão 4 -> Fibonacci
fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci(n - 1) + fibonacci(n - 2)

-- Questão 5 -> INPUT : Lista u e um natural n
--		OUTPUT : n-ésimo termo de u
--		EX( S ): elemento 2 [2,7,3,9] ==> 3
		
elemento n (a:x) | n == 0 = a
                 | otherwise = elemento (n - 1) x

-- Questão 6 -> INPUT : Lista u e valor x
--		OUTPUT : Verdadeiro se x ∈ u e falso do contrário
--		EX( S ): pertence 1 [3,7,4,2] ==> False

elem_pert p [] = False
elem_pert p (a:x) | p == a = True
                  | otherwise = elem_pert p x
					  
-- Questão 7 -> Tamanho de uma lista
tam [] = 0
tam (a:x) = 1 + tam x

-- Questão 8 -> Maior elemento de uma lista
maior [a] = a
maior (a:x) | a > (maior x) = a
            | otherwise = (maior x)

-- Questão 9 -> Retorna o total de ocorrencias de x em uma lista u.
freg n [] = 0
freg n (a:x) | n == a = 1 + freg n x
             | otherwise = freg n x

-- Questão 10 -> INPUT : Lista u e valor x
--		OUTPUT : Verdadeiro se x ocorre exatamente uma vez em u e falso do contrário
--		EX(S): unico 2 [1,2,3,2] ==> False
freg_aux n [] = 0
freg_aux n (a:x) | n == a = 1 + freg_aux n x
                 | otherwise = freg_aux n x

verif num (a:x) | freg_aux num (a:x) == 1 = True
                | otherwise = False

-- Questão 11 -> INPUT : Número x e uma lista u de números
--		 OUTPUT : Sublista de u cujos números sejam maiores que x
--		 EX(S): maioresQue 10 [4 6 30 3 15 3 10 7] ==> [30, 15]
--maiores n [] = []
--maiores n (a:x) | n > a = maiores n x
--                | otherwise = a : maiores n x
					  
					  
					  
					  
					  
					  
					  
					  
					  
					  
					  
					  
					  
					  
					  
