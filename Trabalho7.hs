--Luiz Eduardo Malanczyn de Oliveira

import Data.Char

--1)

data Triangulo = Triangulo {
  a :: Double, 
  b :: Double,
  c :: Double
} deriving (Show) 

classificarTriangulo :: Triangulo -> String
classificarTriangulo triangulo
  | a triangulo  < 0 || b triangulo < 0 || c triangulo < 0 = "Nao e um triangulo, um dos lados"
  | a triangulo == b triangulo && a triangulo == c triangulo = "Triangulo Equilatero"
  | a triangulo == b triangulo || a triangulo == c triangulo || b triangulo == c triangulo = "Triangulo isosceles"
  | a triangulo /= b triangulo && a triangulo /= c triangulo && b triangulo /= c triangulo = "Triangulo escaleno" 
  | a triangulo  == 0 && b triangulo == 0 && c triangulo == 0 = "Triangulo Degenerado" 
  | otherwise = "Nao e um triangulo"


--2)

pedaco :: [x] -> Int -> Int -> [x]
pedaco a b c = map (a !!) [b .. c -1]

lsitaFatias :: Int -> Int -> String -> [Int]
lsitaFatias x y str = map digitToInt (pedaco str y (y + x))

lista :: Int -> Int -> String -> [[Int]]
lista x y str
  | y + (x - 1) < length str = [lsitaFatias x y str] ++ lista x (y + 1) str
  | otherwise = []

fatias :: Int -> String -> [[Int]]
fatias x str = lista x 0 str


--3)

numerosDecimais :: [Int]
numerosDecimais = [1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1]

numeraisRomanos :: [String]
numeraisRomanos = ["M", "CM", "D", "CD", "C", "XC", "L", "XL", "X", "IX", "V", "IV", "I"]

decimaisParaRomanos :: Int -> Int -> String
decimaisParaRomanos _ 0 = ""
decimaisParaRomanos a b
  | b >= (numerosDecimais !! a) = (numeraisRomanos !! a) ++ (decimaisParaRomanos a (b - (numerosDecimais !! a)))
  | otherwise = decimaisParaRomanos (a + 1) b

romanos :: Int -> String
romanos b
  | b <= 3000 = decimaisParaRomanos 0 b
  | otherwise = ""


--4)

palindro :: Int -> [Int] -> Bool
palindro a b
  | a >= ((length b) -1) - a = True
  | b!!a /= b!!(((length b) -1) - a) = False
  | otherwise = palindro (a+1) b

palindro2 :: [Int] -> Bool
palindro2 b = palindro 0 b

divisores :: Int -> [Int]
divisores c = [d | d <- [1..c], (c `mod` d) == 0]

primo :: Int -> Bool
primo d = (length (divisores d)) == 2

listaInt :: [Int] -> Int -> Int
listaInt [] acc = acc
listaInt (d:b) acc = listaInt b (acc*10+d)

palindromoPrimo :: [[Int]] -> [[Int]]
palindromoPrimo b = filter (\d -> (primo (listaInt d 0)) && (palindro2 d) ) b


main = do
  putStrLn $ "classificarTriangulo - Valor: 4, 5, 3  Resultado:\n" ++ show(classificarTriangulo(Triangulo 4 5 3))
  putStrLn $ "Fatias - Valor: [3, 123456789] Resultado:\n" ++ show(fatias 4 "123456789")
  putStrLn $ "Romanos - Valor: 32  Resultado:\n" ++ show(romanos 32)
  putStrLn $ "palindrometroPrimo - Valor:  Resultado:\n" ++ show(palindromoPrimo [[1,1,1],  [1,2,3],  [5,2,5]])
