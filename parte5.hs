module Root.Src.PF where

{-- 44. 
Considere que se usa o tipo [(a,Int)] para representar multi-conjuntos
de elementos de a. Considere ainda que nestas listas não há pares cuja primeira
componente coincida, nem cuja segunda componente seja menor ou igual a zero.
Defina a função elemMSet :: Eq a => a -> [(a,Int)] -> Bool que testa se um
elemento pertence a um multi-conjunto. Por exemplo, elemMSet ’a’ [(’b’,2),
(’a’,4), (’c’,1)] corresponde a True enquanto que elemMSet ’d’ [(’b’,2),
(’a’,4), (’c’,1)] corresponde a False. --}
elemMSet :: Eq a => a -> [(a,Int)] -> Bool
elemMSet _ [] = False
elemMSet a (h:t) = if a == fst h then True else elemMSet a t

{-- 45. 
Considere que se usa o tipo [(a,Int)] para representar multi-conjuntos
de elementos de a. Considere ainda que nestas listas não há pares cuja primeira
componente coincida, nem cuja segunda componente seja menor ou igual a zero.
Defina a função lengthMSet :: [(a,Int)] -> Int que calcula o tamanho de um
multiconjunto. Por exemplo, lengthMSet [(’b’,2), (’a’,4), (’c’,1)] corresponde a
7. --}
lengthMSet :: [(a,Int)] -> Int
lengthMSet [] = 0
lengthMSet (h:t) = snd h + lengthMSet t

{-- 46. 
Considere que se usa o tipo [(a,Int)] para representar multi-conjuntos
de elementos de a. Considere ainda que nestas listas não há pares cuja primeira
componente coincida, nem cuja segunda componente seja menor ou igual a zero.
Defina a função converteMSet :: [(a,Int)] -> [a] que converte um multi-conjuto
na lista dos seus elementos
Por exemplo, converteMSet [(’b’,2), (’a’,4), (’c’,1)] corresponde a "bbaaaac". --}
converteMSet :: [(a,Int)] -> [a] 
converteMSet [] = []
converteMSet ((a,0):t) = converteMSet t
converteMSet ((a,n):t) = a:(converteMSet ((a,n-1):t))

{-- 47. 
Considere que se usa o tipo [(a,Int)] para representar multi-conjuntos
de elementos de a. Considere ainda que nestas listas não há pares cuja primeira
componente coincida, nem cuja segunda componente seja menor ou igual a zero.
Defina a função insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)] que acrescenta
um elemento a um multi-conjunto. Por exemplo, insereMSet ’c’ [(’b’,2), (’a’,4),
(’c’,1)] corresponde a [(’b’,2), (’a’,4), (’c’,2)]. --}
insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet x [] = [(x,1)]
insereMSet x ((a,n):t) = if x == a  then (a,n+1):t else (a,n):(insereMSet x t)


{-- 48. 
Considere que se usa o tipo [(a,Int)] para representar multi-conjuntos
de elementos de a. Considere ainda que nestas listas não há pares cuja primeira
componente coincida, nem cuja segunda componente seja menor ou igual a zero.
Defina a função removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)] que remove um
elemento a um multi-conjunto. Se o elemento não existir, deve ser retornado o
multi-conjunto recebido. Por exemplo, removeMSet ’c’ [(’b’,2), (’a’,4), (’c’,1)]
corresponde a [(’b’,2), (’a’,4)]. --}
removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet x [] = []
removeMSet x ((a,n):t)
     |x == a    = if n == 1 then t else (a,n-1):t
     |otherwise = (a,n):(removeMSet x t)

{-- 49. 
Considere que se usa o tipo [(a,Int)] para representar multi-conjuntos
de elementos de a. Considere ainda que nestas listas não há pares cuja primeira
componente coincida, nem cuja segunda componente seja menor ou igual a zero.
Defina a função constroiMSet :: Ord a => [a] -> [(a,Int)] dada uma lista
ordenada por ordem crescente, calcula o multi-conjunto dos seus elementos. Por
exemplo, constroiMSet "aaabccc" corresponde a [(’a’,3), (’b’,1), (’c’,3)]. --}
constroiMSet :: Ord a => [a] -> [(a,Int)]
constroiMSet [] = []
constroiMSet (h:t) = insereMSet h (constroiMSet t)
--nesta função, em vez de definir uma auxiliar de novo, utilizei a da pergunta #47

{-- 50. 
Apresente uma definição recursiva da função somaPares :: [Int] -> Int
que soma os números pares de uma lista de inteiros. Por exemplo, somaPares
[2,4,5,3,4,2] corresponde a 12. --} 
somaPares :: [Int] -> Int
somaPares [] = 0
somaPares (h:t) = if mod h 2 == 0 then h + somaPares t else somaPares t
