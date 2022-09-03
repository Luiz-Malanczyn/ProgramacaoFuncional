-- Luiz Eduardo Malanczyn de Oliveira

import Text.Printf

-- 1)
fibonacci :: Integer -> Integer
fibonacci a
  | a == 1 = 1
  | a == 2 = 1
  | otherwise = fibonacci (a - 1) + fibonacci (a - 2)


-- 2)
mdc :: Integer -> Integer -> Integer
mdc a b
  | b == 0 = abs a
  | otherwise = mdc b (a `mod` b)

-- 3)
somaInteiros :: Integer -> Integer
somaInteiros a
  | div a 10 == 0 = a
  | otherwise = (somaInteiros(div a 10) + (a `mod` 10))

-- 4)
somaMenorMil :: Integer -> Integer
somaMenorMil a
  | a == 0 = 0
  | a == 10000 = (somaMenorMil 9999)
  | a `mod` 5 == 0 = a + (somaMenorMil (a - 1))
  | a `mod` 3 == 0 = a + (somaMenorMil (a - 1))
  | otherwise = (somaMenorMil (a - 1))

-- 5)


-- 6)


-- 7)
sequencia :: Integer -> [Integer]
sequencia a = sequenciaAux 1 (-1) a

sequenciaAux b c a =
  if a==0 then [2]
  else if a==1 then [2] ++ [b]
  else (sequenciaAux b c (a-1)) ++[(b* (last(sequenciaAux b c (a-1)))-c*(last(sequenciaAux b c (a-2))))]

-- 8)
aoContrario :: [arr] -> [arr]
aoContrario [] = []
aoContrario (x:xs) = aoContrario xs ++ [x]

-- 9)
somaRecursiva :: Integer -> Integer -> Integer
somaRecursiva x 1 = x
somaRecursiva x y = x + somaRecursiva x (y-1)

-- 10)
comprimento :: [Integer] -> Integer
comprimento [] = 0
comprimento (x:xs) = 1 + comprimento xs

main = do
 printf "Função fibonacci: Valor:%d; Resultado:%d\n\n" (10 :: Integer) (fibonacci 10)
 printf "Função mdc: Valores a e b:%d %d; Resultado:%d\n\n" (3 :: Integer) (9 :: Integer) (mdc 3 9)
 printf "Função Soma Inteiros: Valor a:%d; Resultado:%d\n\n" (12345 :: Integer) (somaInteiros 12345)
 printf "Função Soma Menor Mil: Valor 10000:%d; resultado:%d\n\n" (10000 :: Integer) (somaMenorMil 10000)
 putStrLn $ "Sequencia de Lucas: Valor 7 Resultado: " ++ show(sequencia 7) ++ "\n"
 printf "Função ao Contrario: Valor:%s; Resultado:%s\n\n" (show ([4, 2, 1])) (show (aoContrario [4, 2, 1]))
 printf "Função soma recursiva: Valor:%d %d; Resultado:%d\n\n" (12 :: Integer) (2 :: Integer) (somaRecursiva 12 2)
 printf "Função Comprimento: Valor:%s; Resultado:%d\n\n" (show ([3, 5, 7, 9, 12, 55, 40])) (comprimento [3, 5, 7, 9, 12, 55, 40] )
