-- Monad
main = do
    putStrLn "Hello World!!"
    putStrLn (show x);//show transforma x em string
x = 5 //x = 5 algebricamente
y = x + 1//y = 6
p = show y

--noob
sinal(x) = if x < 0
            then -1
            else if x == 0
                    then 0
                    else 1

--guardas retorna valor booleano
sinal(x)
    |x < 0 = -1
    |x == 0 = 0
    |otherwise = 1 --true - 1

--em caso que voce nao colocar o otherwise a funcao se torna incompleta e o programa aborta
sinal(x)
    |x < 0 = -1
    |x == 0 = 0


resultado(nota)
    |nota  > 3 = "rec"
    |nota > 5 = "passou"
    |otherwise = reprovou


absoluto(x)
    |x < 0 = -x
    |otherwise = x

--aqui criamos uma lista com [] e dps meio que (x:xs) cria uma struct com um head == x e prox = xs e soma = x + soma xs
soma [] = 0
soma (x:xs) = x + soma xs

--funcao condicional para verificar para somar só os valores positivos
somaPos [] = 0
somaPos (x:xs)
    |x > 0 = x + somaPos xs
    |otherwise = somaPos xs

--exemplo do codigo a cima só em C
itn somaPosC(int a[], int n){
    int s = 0;
    for(int i = 0; i < n; i++){
        if(a[i]>0){
            s += a[i];
        }
    }
    return s;
}

--baskhara
baskhara a b C
    |delta < 0 = []
    |delta == 0 = [x]
    |otherwise = [x', x'']
     where
       delta = b^2 - 4*a*C
       x = -b/(2*a)
       x' = (-b + sqrt delta) / (2 * a)
       x'' = (-b - sqrt delta) / (2 * a)

--como pegar variaveis
main = do
    la <- getLine
    let a = read la --ele sabe automaticamente em oq converter
    putStrLn $ show $ baskhara a b c