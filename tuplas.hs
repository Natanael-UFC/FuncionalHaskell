import Data.Char

soma_e_sub :: (Int, Int) -> (Int, Int)
soma_e_sub (a, b) = (a + b , a - b)

soma_e_sub_currificada :: Int -> Int -> (Int, Int)
soma_e_sub_currificada a b = (a + b, a - b)

--Tipos compostos

type Seq_Caract = String
type Nomes = (Seq_Caract, Seq_Caract, Seq_Caract, Seq_Caract)

f_nomes_est :: Nomes
f_nomes_est = ("Inverno", "Outono", "Primavera", "Verao")

selec_inv (x, _, _, _) = x
selec_outono (_, x, _, _) = x
selec_prima (_, _, x, _) = x
selec_verao (_, _, _, x) = x

-- -> Segundo exemplo de tipos

type Peso = Float 
type Idade = Int

type Pessoa = (String, Idade, Peso, String)

f_joao, f_maria :: Pessoa
f_joao = ("Joao Pedro", 19, 75.789, "Futebol")
f_maria = ("Maria Fernanda", 18, 65.432, "futsal")

selec_nome :: Pessoa -> String
selec_nome (n, i, p, e) = n

selec_idade :: Pessoa -> Idade
selec_idade (n, i, p, e) = i

selec_peso :: Pessoa -> Peso
selec_peso (n, i, p, e) = p

selec_esporte :: Pessoa -> String
selec_esporte (n, i, p, e) = e

-- Exercicio resolvido
type Meu_tipo = (String, Float, Char)

pessoa :: Float -> Meu_tipo -- Usando o tipo definido anteriormente
pessoa rg | rg == 1 = ("Joao", 12, 'm')  -- retorna uma tupla_3 a partir do rg passado
          | rg == 2 = ("Jonas", 1, 'm')
		  | rg == 3 = ("Joice", 21, 'f')
		  | rg == 4 = ("Janete", 55, 'f')
		  | rg == 5 = ("Julieta", 21, 'f')
		  | otherwise = ("Nao ha mais ninguem", 0, 'x')
		  
menor_idade :: Float -> Meu_tipo
menor_idade x | x == 1 = pessoa 1
              | otherwise = menor (pessoa x) (menor_idade (x - 1))
			  
menor :: Meu_tipo -> Meu_tipo -> Meu_tipo
menor x y | x1 <= x2 = x
          | otherwise = y
		   where
		   		x1 = idade x
				x2 = idade y

idade :: (String, Float, Char) -> Float
idade (x, y, z) = y

soma_idade :: Float -> Float
soma_idade x | x == 1 = idade(pessoa 1)
             | otherwise = idade (pessoa x) + (soma_idade (x - 1))

media_idade :: Float -> Float
media_idade x = (soma_idade x) / x

sexo :: Meu_tipo -> Char
sexo (x, y, z) = z

conta :: Float -> Float -> Float
conta x cont | x == 0 = cont
             | sexo(pessoa x) /= 'm' = conta(x - 1) cont
	         | otherwise = conta(x - 1) (cont + 1)
	  
conta_masc :: Float -> Float
conta_masc x = conta x 0

-- -> segunda alternativa da função conta_masc
sexo1 :: Meu_tipo -> Char
sexo1(_, _, x) = x

eh_masc :: Meu_tipo -> Float
eh_masc x | sexo1 x == 'm' = 1
          | otherwise = 0
		  
conta_masc1 :: Float -> Float
conta_masc1 x | x == 1 = eh_masc(pessoa 1)
              | otherwise = eh_masc(pessoa x) + conta_masc1(x - 1)

maior :: Float -> Float -> Float
maior x y | idade(pessoa x) >= idade(pessoa y) = x
          | otherwise = y
				 
maior_idade :: Float -> Float
maior_idade x | x == 1 = x
              | otherwise = maior x (maior_idade(x - 1))
			  
-- Resolução dos exercicios propostos
--Questão 1
conv_maiusc :: Char -> Char
conv_maiusc x = toUpper x

conv_num :: Char -> Int
conv_num x = ord x

converte :: Char -> (Char, Char, Int)
converte x = (x, conv_maiusc x, conv_num x)

--Questão 2
-- a)
nome :: Meu_tipo -> String
nome (x, y, z) = x

nome_menor_idade :: Float -> String
nome_menor_idade x | x == 1 = nome(pessoa 1)
                   | otherwise = nome(menor_idade x)

--b)
media_idade1 :: Float -> Float
media_idade1 x = (soma_idade x) / x

--c)
conta_masc2 :: Float -> Float
conta_masc2 x | x == 1 = eh_masc(pessoa 1)
              | otherwise = eh_masc(pessoa x) + conta_masc2(x - 1)
			  
--d)
maior_idade1 :: Float -> Float
maior_idade1 x | x == 1 = x
               | otherwise = maior x (maior_idade1(x - 1))
			   
--Questão 3
ehMaiuscula :: Char -> Bool
ehMaiuscula ch = ('A' <= ch) && (ch <= 'Z')

analisa :: Char -> (Char, Char, Int)
analisa x | ehMaiuscula x == True = (x, toLower x, conv_num x)
          | otherwise = (x, conv_maiusc x, conv_num x)
		  
--Questão 6
baskara :: (Float, Float, Float) -> Float
baskara (a, b, c) = (b^2 - 4 * a * c)

raiz1 :: (Float, Float, Float) -> Float
raiz1 (a, b, c) = (b + sqrt(baskara(a, b, c))) / (2 * a)

raiz2 :: (Float, Float, Float) -> Float
raiz2 (a, b, c) = (b - sqrt(baskara(a, b, c))) / (2 * a)

equacao :: (Float, Float, Float) -> (Float, Float)
equacao (a, b, c) | baskara(a, b, c) < 0 = (0, 0)
                  | otherwise = (raiz1(a, b, c), raiz2(a, b, c))

--Questão 7
isForma :: (Int, Int, Int) -> Bool
isForma (a, b, c) | (b - c < a && a < b + c) && (a - c < b && b < a + c) && (a - b < c && c < a + b) = True
                  | otherwise = False
					  
perimetro :: (Int, Int, Int) -> Int
perimetro (a, b, c) = (a + b + c)

tipo :: (Int, Int, Int) -> String
tipo (a, b, c) | (a == b && b == c) = "Equilatero"
               | (a == b || b == c || c == a) = "Isoceles"
			   | otherwise = "Equilatero"

triangulo :: (Int, Int, Int) -> (String, Int)
triangulo (a, b, c) | isForma(a, b, c) == True = (tipo(a, b, c), perimetro(a, b, c))
                    | otherwise = ("", 0)
					
--Questao 8
base :: Int -> (Int, String, String, Char)
base x | x == 0 = (1793, "Pedro", "Mestre", 'M')
       | x == 1 = (1797, "Joana", "Mestre", 'F')
	   | x == 2 = (1534, "Claudio", "Doutor", 'M')
	   | otherwise = (0, "", "", '0')
	   
--a)
type BaseInf = (Int, String, String, Char)

titulo :: BaseInf -> String
titulo (_, _, t, _) = t

ehDoutor :: BaseInf -> Int
ehDoutor x | titulo x == "Doutor" = 1
           | otherwise = 0
		   
contaDoutor :: Int -> Int
contaDoutor 0 = 0
contaDoutor x | x == 1 = ehDoutor(base 1)
              | otherwise = ehDoutor(base x) + contaDoutor(x - 1)
			  
--b)
sexo_b :: BaseInf -> Char
sexo_b (_, _, _, x) = x

ehMul :: BaseInf ->  Int
ehMul x | sexo_b x == 'F' = 1
        | otherwise = 0
		
contarMul :: Int -> Int
contarMul 0 = 0
contarMul x | x == 1 = ehMul(base 1)
            | otherwise = ehMul(base x) + contarMul(x - 1)















