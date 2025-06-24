indexa :: [a] -> Int -> a
indexa xs i = head (drop i xs)

fatorial :: Integer -> Integer
fatorial n = product [2..n]

pairs :: [b] -> [(b,b)]
pairs xs = zip xs (tail xs)

sorted :: Ord a => [a] ->  Bool
sorted xs = null [ () | (p,s) <- pairs xs, p > s]


main :: IO ()
main = do
	putStrLn 'hello world'
