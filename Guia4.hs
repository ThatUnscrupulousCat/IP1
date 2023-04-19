fibonacci :: Integer -> Integer
fibonacci n | n == 0 = 0
            | n == 1 = 1
            | otherwise = fibonacci (n - 1) + fibonacci (n-2)

parteEntera :: Float -> Integer
parteEntera n = toInteger (round  n)

esDivisibleSinMod :: Integer -> Integer -> Bool
esDivisibleSinMod a b | a < b = False
                      | a == b = True
                      | otherwise = esDivisibleSinMod (a-b) b

sumaImpares :: Integer -> Integer
sumaImpares n | n == 1 = 1
              | otherwise = (2*n - 1) + sumaImpares (n-1)

medioFact :: Integer -> Integer
medioFact n | n == 0 = 1
            | n == 1 = 1
            | otherwise = n*medioFact (n-2)

sumaDigitos :: Integer -> Integer
sumaDigitos n | n <= 0 = 0
              | otherwise = mod n 10 + sumaDigitos (div n 10)

cantDigitos :: Integer -> Integer
cantDigitos n | n < 10 = 1
              | otherwise = 1 + cantDigitos (sacarUltimo n)

iesimoDigito :: Integer -> Integer -> Integer
iesimoDigito n i | i == cantDigitos n = ultimoDigito n
                 | otherwise = iesimoDigito (sacarUltimo n) i

cantDigitos :: Integer -> Integer
cantDigitos n | n < 10 = 1
              | otherwise = 1 + cantDigitos (sacarUltimo n)
              