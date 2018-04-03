{-
Module      : Exercicios Funcoes
Description : lista do site http://folivetti.github.io/courses/Haskell/Exercicios2
Author  : drii.cavalcanti@gmail.com
-}

-- Exercicios Funcoes: -------------------------------

-- #01 -----------------------------------------------
ehTriangulo :: Integer -> Integer -> Integer -> [Char]
ehTriangulo x y z 
  | validaLado x y z && validaLado y x z && validaLado z x y = "Eh triangulo"
  | otherwise = "Nao eh triangulo"

validaLado :: Integer -> Integer -> Integer -> Bool
validaLado a b c 
  | (abs (a-c) < b) && (b < (a+c)) = True
  | otherwise = False

-- #02 -----------------------------------------------
classTriangulo :: Integer -> Integer -> Integer -> [Char]
classTriangulo x y z
  | (ehTriangulo x y z == "Eh triangulo") &&
    (x == y) && (y == z) = "Triangulo Equilatero"
  | (ehTriangulo x y z == "Eh triangulo") &&
    (x == y) || (y == z) || (x == z) = "Triangulo Isosceles"
  | (ehTriangulo x y z == "Eh triangulo") &&
    (x /= y) && (y /= z) = "Triangulo Escaleno"
  |otherwise = "Nao eh triangulo"
  
-- #03 -----------------------------------------------

-- #04 -----------------------------------------------
divNumero :: Integer -> Integer -> Bool
divNumero _ 0 = True
divNumero _ 1 = True
divNumero a b 
  | (mod a b == 0) = False
  | otherwise = divNumero a (b-1)

ehPrimo :: Integer -> Bool
ehPrimo x = divNumero x ((div x 2)-1)

-- #05 -----------------------------------------------
somaDig :: Integer -> Integer
somaDig 0 = 0
somaDig x = (mod x 10) + somaDig (div x 10)

-- #06 -----------------------------------------------
{--
unicoDig :: Integer -> Integer
unicoDig 0 = 0
unicoDig x | tamanho x > 9 = 0
           | otherwise = 1

contador :: Int -> Int
contador = x + 1 --}

-- #07 -----------------------------------------------
fatorial :: Int -> Int
fatorial 0 = 1
fatorial n = fatorial(n-1) * n

binomial :: Int -> Int -> Float 
binomial m n = (fromIntegral $ fatorial m) 
                / 
                ((fromIntegral $ fatorial n) * (fromIntegral $ fatorial (m-n)))

-- #08 -----------------------------------------------



  
  
  