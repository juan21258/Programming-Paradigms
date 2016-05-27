todosIguales :: Int -> Int -> Int -> Int -> Bool
todosIguales w x y z = if w == x && x== y && y== z
                          then True
                          else False
todosDiferentes :: Int -> Int -> Int -> Int -> Bool
todosDiferentes w x y z = if w /= x && w /= y && w /= z && y /= w && y /= x &&
                             y /= z && z /= w && z /= x && z /= y
                            then True
                            else False
resta :: (Int,Int) -> Int
resta (a,b) = a - b
myDiv :: Int -> Int -> Int 
myDiv a b = 
            let x = fromIntegral a
                y = fromIntegral b
            in truncate (x / y)
prom :: Int -> [Int] -> Int
prom first []   = 0
prom first (x:xs) = if x < first 
                       then 1 + prom first xs
                       else prom first xs
cuantosDebajoPromedio :: Int -> Int -> Int -> Int -> Int
cuantosDebajoPromedio a b c d = prom ((a + b + c + d) `div` 4) [a,b,c,d]
negativos :: [Int] -> Int 
negativos [] = 0
negativos lista = length [a | a <- lista, a < 0] 
fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
absoluto :: Integer -> Integer
absoluto x = if x > 0 then x else negate x
intercambio :: ((x,g) -> f) -> ((g,x) -> f)
intercambio f (g,x) = f(x,g) 
secuenciaMaxLarga :: Eq a => [a] -> Int
secuenciaMaxLarga [] = 0
secuenciaMaxLarga (a:as) = comp 1 1 a as where
    comp larga actual _ [] = max larga actual
    comp larga actual prev (a:as)
        | a == prev = comp larga (actual + 1) prev as
        | otherwise = comp (max larga actual) 1 a as
