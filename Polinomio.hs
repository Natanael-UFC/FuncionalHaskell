module Polinomio
  ( Polinomio,
    polZero,   -- Polinomio a                                         
    ehPolZero, -- Num a =>  Polinomio a -> Bool                       
    consPol,   -- (Num a) => Int -> a -> Polinomio a -> Polinomio a   
    grau,      -- Polinomio a -> Int                                  
    coefLider, -- Num t => Polinomio t -> t                           
    restoPol   -- Polinomio t -> Polinomio t                          
  ) where
 
data Polinomio a = PolZero | ConsPol Int a (Polinomio a) deriving Eq
 
instance (Show a, Num a, Eq a) => Show (Polinomio a) where
    show PolZero               = "0"
    show (ConsPol 0 b PolZero) = show b
    show (ConsPol 0 b p)       = concat [show b, " + ", show p] 
    show (ConsPol 1 b PolZero) = concat [show b, "*x"]
    show (ConsPol 1 b p)       = concat [show b, "*x + ", show p] 
    show (ConsPol n 1 PolZero) = concat ["x^", show n] 
    show (ConsPol n b PolZero) = concat [show b, "*x^", show n] 
    show (ConsPol n 1 p)       = concat ["x^", show n, " + ", show p] 
    show (ConsPol n b p)       = concat [show b, "*x^", show n, " + ", show p] 
 
-- Exemplos de polinomios com coeficientes inteiros:
exPol1, exPol2, exPol3:: Polinomio Int
exPol1 = consPol 4 3 (consPol 2 (-5) (consPol 0 3 polZero))
exPol2 = consPol 5 1 (consPol 2 5 (consPol 1 4 polZero))
exPol3 = consPol 4 6 (consPol 1 2 polZero)
 
-- Exemplos:
--    > exPol1
--    3*x^4 + -5*x^2 + 3
--    > exPol2
--    x^5 + 5*x^2 + 4*x
--    > exPol3
--    6*x^4 + 2*x
 
-- Exemplos de polinomios com coeficientes real:
exPol5, exPol6, exPol7:: Polinomio Float
exPol5 = consPol 4 3 (consPol 2 (-5) (consPol 0 3 polZero))
exPol6 = consPol 5 1 (consPol 2 5 (consPol 1 4 polZero))
exPol7 = consPol 1 2 (consPol 4 6 polZero)
 
-- Exemplos:
--    > exPol5
--    3.0*x^4 + -5.0*x^2 + 3.0
--    > exPol6
--    x^5 + 5.0*x^2 + 4.0*x
--    > exPol7
--    6.0*x^4 + 2.0*x
 
-- polZero é o polinomio zero. Por exemplo,
--    > polZero
--    0
polZero :: Polinomio a
polZero = PolZero
 
-- (ehPolZero p) verifica se p é o polinomio zero. Por exemplo,
--    ehPolZero polZero  ==  True
--    ehPolZero exPol1   ==  False
ehPolZero :: Polinomio a -> Bool
ehPolZero PolZero = True
ehPolZero _       = False
 
-- (consPol n b p) é o polinomio bx^n+p. Por exemplo,
--    exPol2               ==  x^5 + 5*x^2 + 4*x
--    consPol 3 0 exPol2   ==  x^5 + 5*x^2 + 4*x
--    consPol 3 2 polZero  ==  2*x^3
--    consPol 6 7 exPol2   ==  7*x^6 + x^5 + 5*x^2 + 4*x
--    consPol 4 7 exPol2   ==  x^5 + 7*x^4 + 5*x^2 + 4*x
--    consPol 5 7 exPol2   ==  8*x^5 + 5*x^2 + 4*x
consPol :: (Num a, Eq a) => Int -> a -> Polinomio a -> Polinomio a  
consPol _ 0 p = p
consPol n b PolZero = ConsPol n b PolZero
consPol n b (ConsPol m c p) 
    | n > m      = ConsPol n b (ConsPol m c p)
    | n < m      = ConsPol m c (consPol n b p)
    | b + c == 0 = p
    | otherwise  = ConsPol n (b + c) p
 
-- (grau p) é o grau do polinomio p. Por exemplo,
--    exPol3        ==  6*x^4 + 2*x
--    grau exPol3   ==  4
grau:: Polinomio a -> Int
grau PolZero         = 0
grau (ConsPol n _ _) = n
 
-- (coefLider p) é o coeficiente líder do polinomio p. Por exemplo,
--    exPol3            ==  6*x^4 + 2*x
--    coefLider exPol3  ==  6
coefLider:: Num t => Polinomio t -> t
coefLider PolZero         = 0
coefLider (ConsPol _ b _) = b
 
-- (restoPol p) retorna o resto do polinomio p. Por exemplo,
--    exPol3           ==  6*x^4 + 2*x
--    restoPol exPol3  ==  2*x
--    exPol2           ==  x^5 + 5*x^2 + 4*x
--    restoPol exPol2  ==  5*x^2 + 4*x
restoPol :: Polinomio t -> Polinomio t
restoPol PolZero         = PolZero
restoPol (ConsPol _ _ p) = p











