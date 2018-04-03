{-
Module      : Exercicios Listas
Description : lista do site http://folivetti.github.io/courses/Haskell/Exercicios3
Author  : drii.cavalcanti@gmail.com
-}

-- Exercicios Listas: -------------------------------

-- #01 -----------------------------------------------
lista :: [Int]
lista = [1..20]

divisivel20 :: Int -> Bool
divisivel20 x = divisao lista x
  
divisao :: [Int] -> Int -> Bool
divisao [] _ = True
divisao (a:b) x | mod x a == 1 = False
                | otherwise = divisao b x

-- #02 -----------------------------------------------
{--projectEuler5 | --}

-- #03 -----------------------------------------------
fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci(n-1) + fibonacci(n-2)

lstFibonacci :: Integer -> [Integer]
lstFibonacci n = 0 : prox [x|x <- [1, 2..n]]
  where
    prox :: [Integer] -> [Integer]
    prox [] = []
    prox (a:b) = (fibonacci a) : prox b

-- #04 -----------------------------------------------
{--projectEuler2 :: Integer -> Integer
projectEuler2 n 
  | n < 4000000 = somaLista [x | x <- lstFibonacci n, mod x 2 == 0]
  | otherwise = 0
  where
    somaLista :: [Integer] -> Integer
    somaLista [] = []
    somaLista (a:b) = a + somaLista b --}

-- #05 -----------------------------------------------
produtoEscalar :: [Integer] -> [Integer] -> Integer
produtoEscalar [] _ = 0
produtoEscalar _ [] = 0
produtoEscalar (a:b) (x:xs) = (a*x) + produtoEscalar b xs

-- #06 -----------------------------------------------    
collatz_x :: Int -> Int
collatz_x 0 = 0
collatz_x x
  | (mod x 2 == 0) = (div x 2)
  | otherwise = ((3*x) + 1)
  
-- #07 -----------------------------------------------    
listaCollatz :: Int -> [Int]
listaCollatz = valida . iterate collatz_x
  where 
    valida (1:_) = [1]
    valida (a:b) = a:valida b

collatzLen_x :: Int -> Int
collatzLen_x n = length $ listaCollatz n

-- #08 -----------------------------------------------        
projectEuler_14 = maximum $ map collatzLen_x [1..1000000]

  
  

    
    





