{-
Module      : Exercicios Basicos
Description : lista do site http://folivetti.github.io/courses/Haskell/Exercicios1
Author  : drii.cavalcanti@gmail.com
-}

-- Exercicios Basicos: -------------------------------

-- #01 -----------------------------------------------
-- 2 * (3 + 5) = 16
-- (2 + 2) * (3 + 1) = 16
-- (3^4 + 5) * (2^5 + 1) = 2838

-- #02 -----------------------------------------------
mult3 :: Int -> Bool
mult3 x | (mod x 3 == 0) = True
        | otherwise = False

-- #03 -----------------------------------------------
mult5 :: Int -> Bool
mult5 x | (mod x 5 == 0) = True
        | otherwise = False

-- #04 -----------------------------------------------
mult35 :: Int -> Bool
mult35 x = (mult3 x) && (mult5 x)
         
-- #05 -----------------------------------------------
item5 :: Int -> Bool
item5 x = (x < -1) || ((x > 1) && ((mod x 2) == 0))

-- #06 -----------------------------------------------
{--div2d :: [Integer] -> [Double]
div2d [] = []
div2d (a:b) = [a/2.0] ++ div2d [b]--}

-- #07 -----------------------------------------------
seno :: Float -> (Float, Float)
seno 0 = (0, 0)
seno a = (sqrt((1-cos(a/2))/2), (-1)*sqrt((1-cos(a/2))/2))

-- #08 -----------------------------------------------
verifica_bi :: Int -> Bool
verifica_bi ano | mod ano 4 == 0 && (mod ano 100 /= 0 || mod ano 400 == 0) = True
                | otherwise = False
anos_bi = [x | x <- [1..2018], verifica_bi x]

-- #09 -----------------------------------------------
prim_10_anos_bi = take 10 anos_bi

-- #10 -----------------------------------------------
split_anos_bi = splitAt 245 anos_bi

-- #11 -----------------------------------------------
concatenar :: [Char] -> [Char]
concatenar texto = [c | c <- texto, c /= ' ']

-- #12 -----------------------------------------------
numeros = "0123456789"
split_numeros :: [Char] -> Integer
split_numeros (a:b) = [read [a] :: Integer] ++ split_numeros [b]








