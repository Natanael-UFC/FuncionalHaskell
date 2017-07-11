data Arv a = Folha | No a (Arv a) (Arv a) deriving (Show, Eq)
--Construção da arvore
construir :: [a] -> Arv a
construir [] = Folha
construir xs = No x (construir xsm) (construir xsl)
               where n = length xs `div` 2     -- ponto médio
                     xsm = take n xs		   -- valores à esquerda
                     x:xsl = drop n xs 		   -- valores central e à direita
					 
--Algoritmo de ordenção QuickSort
quicksort :: (Ord a) => [a] -> [a]
quicksort []     = []
quicksort (x:xs) = smaller ++ equal ++ greater
    where smaller = quicksort (filter (<x) xs)
          equal = x:(filter (==x) xs)
          greater = quicksort (filter (>x) xs)

--Retorna o tamanho da arvore
tamanhoArv :: Arv a -> Int
tamanhoArv Folha = 0
tamanhoArv (No x esq dir) = 1 + tamanhoArv esq + tamanhoArv dir

--Retorna a altura da arvore
altura :: Arv a -> Int
altura Folha = 0
altura (No _ esq dir) = 1 + max (altura esq) (altura dir)

-- Soma todos o valor de todos os nós da arvore
soma :: (Num a) => Arv a -> a
soma Folha = 0
soma (No x Folha Folha) = x
soma (No x esq dir) = x + soma esq + soma dir

-- Retorna o nivel que o nó se encontra na arvore
nivel :: (Ord a) => Int -> Arv a -> [a]
nivel _ Folha = []
nivel 0 (No x esq dir) = [x]
nivel n (No x esq dir) = quicksort $ if altura(No x esq dir) == n then quicksort[x] else nivel n esq ++ nivel n dir

--Função que aplica uma função na arvore
mapArv :: (a -> b) -> Arv a -> Arv b
mapArv func Folha = Folha
mapArv func (No x esq dir) = (No (func x) (mapArv func esq) (mapArv func dir))

-- Função que imprime a arvore espelhada
reflect :: Arv a -> Arv a
reflect Folha = Folha
reflect (No x esq dir) = (No x (reflect dir) (reflect esq))

-- Inserir um nó na arvore
inserir :: (Ord a) => a -> Arv a -> Arv a
inserir x Folha = No x Folha Folha
inserir x (No y esq dir) | x == y = No y esq dir
                         | x < y = No y (inserir x esq) dir
                         | x > y = No y esq (inserir x dir)

-- Remover um nó da arvore
mais_esq :: Ord a => Arv a -> a
mais_esq (No x Folha _) = x
mais_esq (No _ esq _) = mais_esq esq

remover :: Ord a => a -> Arv a -> Arv a
remover x Folha = Folha
remover x (No y Folha dir) | x == y = dir
remover x (No y esq Folha) | x == y = esq
remover x (No y esq dir) | x < y = No y (remover x esq) dir
                         | x > y = No y esq (remover x dir)
     					 | x == y = let z = mais_esq dir
						            in No z esq (remover z dir)

--Lista todos os nós da arvore
listar :: (Ord a) => Arv a -> [a]
listar Folha = []
listar (No x esq dir) = listar esq ++ [x] ++ listar dir
