module Root.Src.PF where

{-- 1. 
Apresente uma definição recursiva da função (pré-definida) myEnumFromTo ::
Int -> Int -> [Int] que constrói a lista dos números inteiros compreendidos
entre dois limites. Por exemplo, myEnumFromTo 1 5 corresponde à lista [1,2,3,4,5] --}
myEnumFromTo :: Int -> Int -> [Int]
myEnumFromTo a b 
     |a <= b    = a:(myEnumFromTo (a + 1) b)
     |otherwise = []

{-- 2. 
Apresente uma definição recursiva da função (pré-definida) myEnumFromThenTo
:: Int -> Int -> Int -> [Int] que constrói a lista dos números inteiros
compreendidos entre dois limites e espaçados de um valor constante. Por exemplo,
myEnumFromThenTo 1 3 10 corresponde à lista [1,3,5,7,9]. --}
myEnumFromThenTo :: Int -> Int -> Int -> [Int]

myEnumFromThenTo 0 0 0 = [0]
myEnumFromThenTo a b c
 |a <= c         = a:(myEnumFromThenTo b (2*b-a) c)
 |otherwise      = []
 
{-- 3. 
Apresente uma definição recursiva da função (pré-definida) (+++) :: [a] ->
[a] -> [a] que concatena duas listas. Por exemplo, (+++) [1,2,3] [10,20,30]
corresponde à lista [1,2,3,10,20,30]. --}
infixl +++
(+++) :: [a] -> [a] -> [a]
l +++ [] = l
l +++ (h:t) = l ++ [h] +++ t

{-- 4. 
Apresente uma definição recursiva da função (pré-definida) myLast :: [a] ->
a que calcula o último elemento de uma lista não vazia. Por exemplo, last
[10,20,30] corresponde a 30. --}
myLast :: [a] -> a
myLast [a] = a 
myLast (h:t) = myLast t

{-- 5. 
Apresente uma definição recursiva da função (pré-definida) myInit :: [a] ->
[a] que dada uma lista não vazia calcula uma lista igual a essa mas sem o
último elemento. Por exemplo, myInit [10,20,30] corresponde a [10,20]. --}
myInit :: [a] -> [a]
myInit [a] = []
myInit (h:t) = h:(myInit t)

{-- 6. 
Apresente uma definição recursiva da função (pré-definida) (.!!) :: [a] ->
Int -> a que dada uma lista e um inteiro, calcula o elemento da lista que se
encontra nessa posição (assumese que o primeiro elemento se encontra na posição
0. Por exemplo, (.!!) [10,20,30] 1 corresponde a 20. Ignore os casos em que a
função não se encontra definida (i.e., em que a posição fornecida não
corresponde a nenhuma posição válida da lista). --}
infixl .!!
(.!!) :: [a] -> Int -> a
(h:t) .!! 0 = h
(h:t) .!! n = t .!! (n-1)

{-- 7. 
Apresente uma definição recursiva da função (pré-definida) myReverse :: [a]
-> [a] que dada uma lista calcula uma lista com os elementos dessa lista pela
ordem inversa. Por exemplo, myReverse [10,20,30] corresponde a [30,20,10]. --}
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (h:t) = (myReverse t) ++ [h]



{-- 8. 
Apresente uma definição recursiva da função (pré-definida) myTake :: Int ->
[a] -> [a] que dado um inteiro n e uma lista l calcula a lista com os (no
máximo) n primeiros elementos de l.
A lista resultado só terá menos de que n elementos se a lista l tiver menos do
que n elementos. Nesse caso a lista calculada é igual à lista fornecida. Por
exemplo, myTake 2 [10,20,30] corresponde a [10,20]. --}
myTake :: Int -> [a] -> [a]
myTake _ [] = []
myTake 0 _ = []
myTake n (h:t) 
     |n < 0      = []
     |otherwise  = h:(myTake (n-1) t)

{-- 9. 
Apresente uma definição recursiva da função (pré-definida) myDrop :: Int ->
[a] -> [a] que dado um inteiro n e uma lista l calcula a lista sem os (no
máximo) n primeiros elementos de l. Se a lista fornecida tiver n elementos ou
menos, a lista resultante será vazia. Por exemplo, myDrop 2 [10,20,30] 
corresponde a [30]. --}
myDrop :: Int -> [a] -> [a]
myDrop _ [] = []
myDrop 0 l = l
myDrop n (h:t) 
    |n < 0      = (h:t)
    |otherwise  = myDrop (n-1) t

{-- 10. 
Apresente uma definição recursiva da função (pré-definida) myZip :: [a] ->
[b] -> [(a,b)] constói uma lista de pares a partir de duas listas. Por exemplo,
myZip [1,2,3] [10,20,30,40] corresponde a [(1,10),(2,20),(3,30)]. --}
myZip :: [a] -> [b] -> [(a,b)] 
myZip _ [] = []
myZip [] _ = []
myZip (h:t) (x:s) = (h,x):(myZip t s)

{-- 11. 
Apresente uma definição recursiva da função (pré-definida) myElem :: Eq a
=> a -> [a] -> Bool que testa se um elemento ocorre numa lista. Por exemplo,
elem 20 [10,20,30] corresponde a True enquanto que myElem 2 [10,20,30] 
corresponde a False. --}
myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem a (h:t) = if a==h then True else myElem a t

{-- 12. 
Apresente uma definição recursiva da função (pré-definida) myReplicate ::
Int -> a -> [a] que dado um inteiro n e um elemento x constói uma lista com n
elementos, todos iguais a x. Por exemplo, myReplicate 3 10 corresponde a
[10,10,10]. --}
myReplicate :: Int -> a -> [a] 
myReplicate 0 a = []
myReplicate n a 
    |n < 0        = []
    |otherwise    = a:(myReplicate (n-1) a)

{-- 13. 
Apresente uma definição recursiva da função (pré-definida) myIntersperse
:: a -> [a] -> [a] que dado um elemento e uma lista, constrói uma lista em que o
elemento fornecido é intercalado entre os elementos da lista fornecida. Por
exemplo, myIntersperse 1 [10,20,30] corresponde a [10,1,20,1,30]. --}
myIntersperse :: a -> [a] -> [a]
myIntersperse a [] = []
myIntersperse a [x] = [x]
myIntersperse a (h:t) = h:a:(myIntersperse a t)
