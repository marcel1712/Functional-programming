--string nao eh primeira classe em c, vamos aprender usar funcao
main = do 
    putStrLn $ show $ somaTeste ehPositivo a
    putStrLn $ show $ somaTeste ehNegativo a
    putStrLn $ show $ somaTeste ehPar a
    putStrLn $ show $ somaTeste ehImpar a
    putStrLn $ show $ operaTEste ehPositivo adiciona 0 a
    putStrLn $ show $ operaTEste ehImpar multiplica 1
    putStrLn $ show $ somaTeste (\x -> True) a

--sistemas de tipo
k :: Integer
k = 5
m :: int
m = 5

adiciona :: Integer -> (Integer -> Integer)
adiciona x y =  x + y
adiciona' x = \y -> x + y
multiplica x y = x * y

ehPositivo :: Integer ->bool
ehPositivo' = \x -> x > 0
ehNegativo x = x < 0
ehPar x = mod x 2 == 0
ehImpar x = mod x 2 == 1

a = [3,4,5,-7,10,-8,12]

soma [] = 0
soma (x:xs) = x + soma xs

somaPos [] = 0
somaPos (x:xs)
    | x > 0 = x + somaPos xs
    | otherwise = somaPos xs

somaNeg [] = 0
somaNeg (x:xs)
    | x < 0 = x + somaNeg xs
    | otherwise = somaNeg xs

somaPar [] = 0
somaPar (x:xs)
    | mod x 2 == 0 = x + somaPar xs
    | otherwise = somaPar xs

somaSe cond [] = 0
somaSe cond (x:xs)
    | cond == "Pos" && x > 0 = x+ somaSe cond xs
    | cond == 

somaTeste teste [] = 0
somaTeste teste (x:xs)
    | teste x = x + somaTeste teste xs
    | otherwise = somaTeste teste xs

prodTeste teste [] = 1
prodTeste teste (x:xs)
    | teste x = x * prodTeste teste xs
    | otherwise = prodTeste teste xs

operaTEste teste op neutro [] = neutro
operaTEste teste op neutro (x:xs)
    | teste x = op x $ operaTEste teste op neutro xs
    | otherwise = operaTEste teste op neutro xs

filtra _ [] = []
filtra teste (x:xs)
    | teste x = x:r
    | otherwise = r
        where 
         r = filtra teste xs


-- Curry
