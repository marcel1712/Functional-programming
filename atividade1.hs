main = do
    la <- getLine
    let a = read la :: Double
    lb <- getLine
    let b = read lb :: Double
    lc <- getLine
    let c = read lc :: Double
    putStrLn $ heron a b c

heron :: Double -> Double -> Double -> String
heron a b c
    | not (a + b >= c && a + c >= b && b + c >= a) = "-" --evitando nao triangulos
    | areaSquared < 0             = "-"    -- evitando areas negativas
    | otherwise = show (sqrt areaSquared)
  where
    p = (a + b + c) / 2
    areaSquared = p * (p - a) * (p - b) * (p - c)

