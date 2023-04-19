prodInt :: (Integer, Integer) -> (Integer, Integer) -> Integer
prodInt (a1, a2) (b1, b2) = b1*b2 + a1*a2

todoMenor :: (Integer, Integer) -> (Integer, Integer) -> Bool
todoMenor (a1, a2) (b1, b2) | a1 > a2 && b1 > b2 = True
                            | otherwise = False

distanciaPuntos ::  (Float, Float) -> (Float, Float) -> Float
distanciaPuntos (x1, y1) (x2, y2) = sqrt((x2 - x1)^2+(y2 - y1)^2)

sumaTerna :: (Integer, Integer, Integer) -> Integer
sumaTerna (x, y, z) = x + y + z

sumarSoloMultiplos :: (Integer, Integer, Integer) -> Integer -> Integer
sumarSoloMultiplos (x, y, z) n = sum [i | i <- [x,y,z], i `mod` n == 0]

posPrimerPar :: (Integer, Integer, Integer) -> Integer
posPrimerPar (x,y,z) | even x = 1
                     | even y = 2
                     | even z = 3
                     | otherwise = 4

crearPar :: a -> b -> (a, b)
crearPar a b = (a, b)

invertir :: a -> b -> (b, a)
invertir a b = (b, a)