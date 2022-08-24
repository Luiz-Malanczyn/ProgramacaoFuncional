-- Luiz Eduardo Malanczyn de Oliveira

import Text.Printf

-- 1)
soma1 :: Integer -> Integer
soma1 a = a + 1

-- 2)
sempre :: a -> Integer
sempre a = 0

-- 3)
treco :: Double -> Double -> Double -> Double
treco a b c = (a + b) * c 

-- 4)
resto :: Integer -> Integer -> Integer
resto a b = a `mod` b

-- 5)
precoMaior :: Double -> Double -> Double -> Double -> Double
precoMaior a b c d = maximum[a, b, c, d]

-- 6)
impar :: Integer -> Integer -> Bool
impar a b = if  a * b `mod` 2 /= 0 then True else False

-- 7)
somaPar :: (Integer, Integer) -> Integer
somaPar a = uncurry (+) a

-- 8)
equacao :: Double -> Double -> Double -> Double
equacao a b c = a**2 + ((b/2) + c)
-- 9)
diagnostico :: Double -> String
diagnostico peso 
  |peso/1.79^2<17 = "Muito Abaixo do peso"
  |peso/1.79^2<=18.49 = "Abaixo do peso"
  |peso/1.79^2<=24.99 = "Peso normal"
  |peso/1.79^2<=29.99 = "Sobrepeso"
  |peso/1.79^2<=34.99 = "Obesidade leve"
  |peso/1.79^2<=39.99 = "Obesidade severa"
  |otherwise ="Obesidade mórbida"

-- 10)
bissexto :: Integer -> String
bissexto ano =  if (ano `mod` 4) == 0 && ((ano `mod` 100) /= 0) || ((ano `mod` 100) == 0 && (ano `mod` 400) == 0) 
then
    "True"
else
    "False"

main = do
 printf "Função soma1: Valor:%d; Resultado da soma:%d\n\n" (9::Integer) (soma1 1)
 printf "Função sempre: Valor:%d; Resultado:%d\n\n" (3::Integer) (sempre 3)
 printf "Função treco: Valor 1, 2 e 3:%f %f %f; Soma dos primeiros multiplcado pelo terceiro:%f\n\n" (2::Double) (3::Double) (4::Double) (treco 2.0 3.0 4.0)
 printf "Função resto: Valor 1 e 2:%d %d; Resto da divisão:%d\n\n" (7::Integer) (2::Integer) (resto 7 2)
 printf "Função precoMaior: Valor 1, 2, 3 e 4:%f %f %f %f; Maior Valor:%f\n\n" (12::Double) (1::Double) (3::Double) (6::Double)(precoMaior 12 1 3 6)
 printf "Função impar: Valor 1 e 2:%d %d; O resultado é impar: %s\n\n" (4::Integer) (2::Integer) (show (impar 4 2))
 printf "Função somaPar: Valor (1, 2):(%d,%d); Resultado da Soma:%d\n\n" (1::Integer) (2::Integer) (somaPar (1,2))
 printf "Função equacao: Valor 1, 2 e 3:%f %f %f; Resultado da equação:%f\n\n" (7::Double) (2::Double) (9::Double) (equacao 7 2 9)
 printf "Função diagnostico: Peso:%f; Resultado:%s\n\n" (70::Double) (diagnostico 70)
 printf "Função bissexto: Ano:%d; Resultado:%s\n\n" (2000::Integer) (bissexto 2000)
