divisoresDe :: Int -> [Int]
divisoresDe numero = [x | x <- [1..numero - 1], numero `mod` x == 0]

tipoDeNumero :: Int -> Int
tipoDeNumero valor
  | soma < valor  = 0  -- defeituoso
  | soma == valor = 1  -- perfeito
  | otherwise     = 2  -- abundante
  where soma = sum (divisoresDe valor)

contarTipos :: [Int] -> (Int, Int, Int)
contarTipos lista =
  let categorias = map tipoDeNumero lista
  in (length (filter (== 0) categorias),  -- defeituosos
      length (filter (== 1) categorias),  -- perfeitos
      length (filter (== 2) categorias))  -- abundantes

main :: IO ()
main = do
  x <- readLn
  outroNumero <- readLn
  let intervalo = [min x outroNumero .. max x outroNumero]
  let (qtdDefeituosos, qtdPerfeitos, qtdAbundantes) = contarTipos intervalo
  print qtdDefeituosos
  print qtdPerfeitos
  print qtdAbundantes
