module Root.Src.PF where

{-- 34. 
Apresente uma definição recursiva da função (pré-definida) myUnwords ::
[String] -> String que junta todas as strings da lista numa só, separando-as por
um espaço. Por exemplo, myUnwords ["Programacao", "Funcional"] corresponde a
"Programacao Funcional". --}
myUnwords :: [String] -> String
myUnwords [] = "" 
myUnwords [a] = a
myUnwords (h:t) = h ++ " " ++ myUnwords t

{-- 35. 
Apresente uma definição recursiva da função (pré-definida) myUnlines ::
[String] -> String que junta todas as strings da lista numa só, separando-as
pelo caracter ’\n’. Por exemplo, myUnlines ["Prog", "Func"] corresponde a
"Prog\nFunc". --}
myUnlines :: [String] -> String
myUnlines [] = ""
myUnlines [a] = a ++ "\n"
myUnlines (h:t) = h ++ "\n" ++ myUnlines t

{-- 36. 
Apresente uma definição recursiva da função pMaior :: Ord a => [a] ->
Int que dada uma lista não vazia, retorna a posição onde se encontra o maior
elemento da lista. As posições da lista começam em 0, i.e., a função deverá
retornar 0 se o primeiro elemento da lista for o maior. --}
pMaior :: Ord a => [a] -> Int                              
pMaior (h:t) = if h == max' (h:t) then 0 else 1 + pMaior t   
                                                           
max' :: (Ord a) => [a] -> a                                           
max' [a] = a                                               
max' (h:r:t) = if h > r then max' (h:t) else max' (r:t)    



{-- 37. 
Apresente uma definição recursiva da função temRepetidos :: Eq a => [a]
-> Bool que testa se uma lista tem elementos repetidos. Por exemplo,
temRepetidos [11,21,31,21] corresponde a True enquanto que temRepetidos
[11,2,31,4] corresponde a False. --}
temRepetidos :: Eq a => [a] -> Bool
temRepetidos [] = False
temRepetidos [a] = False
temRepetidos (h:t) = if elem h t then True else temRepetidos t

{-- 38. 
Apresente uma definição recursiva da função algarismos :: [Char] ->
[Char] que determina a lista dos algarismos de uma dada lista de caracteres. Por
exemplo, algarismos "123xp5" corresponde a "1235". --}
algarismos :: [Char] -> [Char]
algarismos [] = []
algarismos (h:t) = if '0' <= h && h <= '9' then h:(algarismos t) else algarismos t


{-- 39. 
Apresente uma definição recursiva da função posImpares :: [a] -> [a] que
determina os elementos de uma lista que ocorrem em posições ímpares. Considere
que o primeiro elemento da lista ocorre na posição 0 e por isso par. Por
exemplo, posImpares [10,11,7,5] corresponde a [11,5]. --}
posImpares :: [a] -> [a]
posImpares [] = []
posImpares [a] = []
posImpares (h:r:t) = r:(posImpares t)

{-- 40. 
Apresente uma definição recursiva da função posPares :: [a] -> [a] que
determina os elementos de uma lista que ocorrem em posições ímpares. Considere
que o primeiro elemento da lista ocorre na posição 0 e por isso par. Por
exemplo, posPares [10,11,7,5] corresponde a [10,7]. --} --}
posPares :: [a] -> [a]
posPares [] = []
posPares [a] = [a]
posPares (h:r:t) = h:(posPares t)

{-- 41. 
Apresente uma definição recursiva da função isSorted :: Ord a => [a] ->
Bool que testa se uma lista está ordenada por ordem crescente. Por exemplo,
isSorted [1,2,2,3,4,5] corresponde a True, enquanto que isSorted [1,2,4,3,4,5]
corresponde a False. --}
isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [a] = True
isSorted (h:r:t) = if h <= r then isSorted (r:t) else False

{-- 42. 
Apresente uma definição recursiva da função iSort :: Ord a => [a] -> [a]
que calcula o resultado de ordenar uma lista. Assuma, se precisar, que existe
definida a função insert
:: Ord a => a -> [a] -> [a] que dado um elemento e uma lista ordenada retorna a
:: lista
resultante de inserir ordenadamente esse elemento na lista. --}
iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort (h:t) = insert h (iSort t)

insert :: (Ord a) => a -> [a] -> [a]
insert a [] = [a]
insert a l 
    |a >= last l      = l ++ [a]
    |otherwise   = (insert a (init l)) ++ [last l]

{-- 43. 
Apresente uma definição recursiva da função menor :: String -> String ->
Bool que dadas duas strings, retorna True se e só se a primeira for menor do que
a segunda, segundo a ordem lexicográfica (i.e., do dicionário) Por exemplo,
menor "sai" "saiu" corresponde a True enquanto que menor "programacao"
"funcional" corresponde a False. --}
menor :: String -> String -> Bool
menor "" "" = False
menor "" _ = True
menor _ "" = False
menor (h:t) (x:xs) 
     |h < x        = True
     |h == x       = menor t xs
     |otherwise    = False
    
