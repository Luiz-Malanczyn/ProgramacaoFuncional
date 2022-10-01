--Luiz Eduardo Malanczyn de Oliveira

import Data.Char

--1)
divisoresDeN :: Int -> [Int]
divisoresDeN a = [x | x <- [1..a], (a `mod` x) == 0]

--2)
contaCaractere :: String -> Char -> Int
contaCaractere str char = length [a | a <- str, toLower a == toLower char]

--3)
dobroNaoNegativo :: [Int] -> [Int]
dobroNaoNegativo lista = [a*2 | a <- lista, a >= 0]

--4)
pitagoras :: Int -> [(Int, Int, Int)]
pitagoras n = [(a,b,c) | a <-[1..n], b <-[1..n], c <- [1..n], ((a^2) + (b^2)) == (c^2), c > a, c > b, b > a]

--5)
numerosPerfeitos :: Int -> [Int]
numerosPerfeitos a = [b | b <- [1..a], sum (init (divisoresDeN b)) == b]

--6)
produtoEscalar :: [Int] -> [Int] -> Int
produtoEscalar a1 a2 =  sum [b * c | (b, c) <- zip a1 a2]

--7)
primeirosPrimos :: Int -> [Int]
primeirosPrimos a = takeWhile (\b -> length [c | c <- [2..(b-1)], length(divisoresDeN c) == 2] < a ) [c | c <- [2..], length(divisoresDeN c) == 2]

--8)
paresOrdenados :: Int -> [(Double, Double)]
paresOrdenados a = [(2^x, 3^x) | x <- [0..a]]

main = do
  putStrLn $ "divisoresDeN - Valor: 21; Resultado: " ++ show(divisoresDeN 21)
  putStrLn $ "contaCaractere - Valor: (Macaco, o) Resultado:" ++ show(contaCaractere "Macaco" 'o')
  putStrLn $ "dobroNaoNegativo - Valor: -20; Resultado:" ++ show(dobroNaoNegativo [(-20)..20])
  putStrLn $ "pitagoras - Valor: 15; Valor:" ++ show(pitagoras 15)
  putStrLn $ "numerosPerfeitos - Valor: 30; Resultado:" ++ show(numerosPerfeitos 30)
  putStrLn $ "produtoEscalar - Valor: [1,2,3]; Resultado:" ++ show(produtoEscalar [1,2,3] [1,2,3])
  putStrLn $ "primeirosPrimos - Valor: 45; Resultado:" ++ show(primeirosPrimos 45)
  putStrLn $ "paresOrdenados - Valor: 10; Resultado:" ++ show(paresOrdenados 10)
