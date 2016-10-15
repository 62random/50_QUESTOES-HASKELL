module Root.Src.PF where

{-- 14. 
Apresente uma definição recursiva da função (pré-definida) myGroup :: Eq a
=> [a] -> [[a]] que agrupa elementos iguais e consecutivos de uma lista. Por
exemplo, myGroup [1,2,2,3,4,4,4,5,4] corresponde a
[[1],[2,2],[3],[4,4,4],[5],[4]]. --}
myGroup :: Eq a => [a] -> [[a]]
myGroup [] = []
myGroup (h:t) = t1:(myGroup t2) where (t1,t2) = span (==h) (h:t)

{-- 15. 
Apresente uma definição recursiva da função (pré-definida) myConcat ::
[[a]] -> [a] que concatena as listas de uma lista. Por exemplo, myConcat
[[1],[2,2],[3],[4,4,4],[5],[4]] corresponde a [1,2,2,3,4,4,4,5,4]. --}
myConcat :: [[a]] -> [a]
myConcat [] = []
myConcat (h:t) = h ++ (myConcat t)

{-- 16. 
Apresente uma definição recursiva da função (pré-definida) myInits :: [a]
-> [[a]] que calcula a lista dos prefixos de uma lista. Por exemplo, myInits
[11,21,13] corresponde a [[],[11],[11,21],[11,21,13]]. --}
myInits :: [a] -> [[a]]
myInits [] = [[]]
myInits ls = (myInits (init ls)) ++ [ls]
{-- 17. 
Apresente uma definição recursiva da função (pré-definida) myTails :: [a]
-> [[a]] que calcula a lista dos sufixos de uma lista. Por exemplo, myTails
[1,2,3] corresponde a [[1,2,3],[2,3],[3],[]]. --}
myTails :: [a] -> [[a]]
myTails [] = [[]]
myTails ls = [ls] ++ (myTails (tail ls))

{-- 18. 
Apresente uma definição recursiva da função (pré-definida) myIsPrefixOf ::
Eq a => [a] -> [a] -> Bool que testa se uma lista é prefixo de outra. Por
exemplo, isPrefixOf [10,20] [10,20,30] corresponde a True enquanto que
myIsPrefixOf [10,30] [10,20,30] corresponde a False. --}
myIsPrefixOf :: Eq a => [a] -> [a] -> Bool 
myIsPrefixOf [] l = True
myIsPrefixOf l [] = False
myIsPrefixOf (h:t) (x:s) 
    |h==x       = myIsPrefixOf t s
    |otherwise  = False

{-- 19. 
Apresente uma definição recursiva da função (pré-definida) myIsSuffixOf ::
Eq a => [a] -> [a] -> Bool que testa se uma lista é sufixo de outra. Por
exemplo, isSuffixOf [20,30] [10,20,30] corresponde a True enquanto que
myIsSuffixOf [10,30] [10,20,30] corresponde a False. --}
myIsSuffixOf :: Eq a => [a] -> [a] -> Bool 
myIsSuffixOf [] _ = True
myIsSuffixOf _ [] = False
myIsSuffixOf l ls 
    |last l == last ls  = myIsSuffixOf (init l) (init ls)
    |otherwise          = False


{-- 20. 
Apresente uma definição recursiva da função (pré-definida)
myIsSubsequenceOf :: Eq a => [a] -> [a] -> Bool que testa se os elementos de uma
lista ocorrem noutra pela mesma ordem relativa. Por exemplo, myIsSubsequenceOf
[20,40] [10,20,30,40] corresponde a True enquanto que myIsSubsequenceOf [40,20]
[10,20,30,40] corresponde a False. --}
myIsSubsequenceOf :: Eq a => [a] -> [a] -> Bool 
myIsSubsequenceOf [] l = True
myIsSubsequenceOf l [] = False
myIsSubsequenceOf (h:t) (x:y)
     |h==x   = myIsSubsequenceOf t y
     |otherwise = myIsSubsequenceOf (h:t) y

{-- 21. 
Apresente uma definição recursiva da função (pré-definida) myElemIndices
:: Eq a => a -> [a] -> [Int] que calcula a lista de posições em que um dado
elemento ocorre numa lista. Por exemplo, myElemIndices 3 [1,2,3,4,3,2,3,4,5]
corresponde a [2,4,6]. --}
myElemIndices :: Eq a => a -> [a] -> [Int]
myElemIndices x [] = []
myElemIndices x l
    |(last l)==x    = (myElemIndices x (init l)) ++ [(length l) - 1]
    |otherwise      = myElemIndices x (init l)

{-- 22. 
Apresente uma definição recursiva da função (pré-definida) myNub :: Eq a
=> [a] -> [a] que calcula uma lista com os mesmos elementos da recebida, sem
repetições. Por exemplo, myNub [1,2,1,2,3,1,2] corresponde a [1,2,3]. --}
myNub :: Eq a => [a] -> [a]
myNub [] = []
myNub l
    |elem (last l) (init l)   = myNub (init l)
    |otherwise                = (myNub (init l)) ++ [last l]

{-- 23. 
Apresente uma definição recursiva da função (pré-definida) myDelete :: Eq
a => a -> [a] -> [a] que retorna a lista resultante de remover (a primeira
ocorrência de) um dado elemento de uma lista. Por exemplo, myDelete 2
[1,2,1,2,3,1,2] corresponde a [1,1,2,3,1,2]. Se não existir nenhuma ocorrência
a função deverá retornar a lista recebida. --}
myDelete :: Eq a => a -> [a] -> [a]
myDelete _ [] = []
myDelete x l 
    |elem x (init l)    = myDelete x (init l) ++ [last l]
    |otherwise          = if last l == x  then init l else l

{-- 24. 
Apresente uma definição
recursiva da função (pré-definida) (\\\):: Eq a => [a] -> [a] -> [a] que retorna
a lista resultante de remover (as primeiras ocorrências) dos elementos da
segunda lista da primeira. Por exemplo, (\\)[1,2,3,4,5,1] [1,5] corresponde a
[2,3,4,1]. --}
infixl \\\
(\\\) :: Eq a => [a] -> [a] -> [a]
[] \\\ _  = []
l \\\  [] = l
l \\\ (h:t) = (retirar1a h l) \\\ t
     
     
retirar1a :: (Eq a) => a -> [a] -> [a]  ---FUNÇAO AUXILIAR PARA A PERGUNTA 24
retirar1a _ [] = []
retirar1a a l 
    |a /= last l    = retirar1a a (init l) ++ [last l]
    |otherwise      = if elem a (init l) then retirar1a a (init l) ++ [last l] else init l

{-- 25. 
Apresente uma definição recursiva da função (pré-definida) myUnion :: Eq a
=> [a] -> [a] -> [a] que retorna a lista resultante de acrescentar à primeira
lista os elementos da segunda que não ocorrem na primeira. Por exemplo, myUnion
[1,1,2,3,4] [1,5] corresponde a [1,1,2,3,4,5]. --}
myUnion :: Eq a => [a] -> [a] -> [a] 
myUnion l [] = l
myUnion l (h:t) = if elem h l then (myUnion l t) else (myUnion (l ++ [h]) t) 

{-- 26. 
Apresente uma definição recursiva da função (pré-definida) myIntersect ::
Eq a => [a] -> [a] -> [a] que retorna a lista resultante de remover da primeira
lista os elementos que não pertencem à segunda. Por exemplo, myIntersect
[1,1,2,3,4] [1,3,5] corresponde a [1,1,3]. --}
myIntersect :: Eq a => [a] -> [a] -> [a] 
myIntersect [] _ = []
myIntersect l [] = []
myIntersect (h:t) l = if elem h l then h:myIntersect t l else myIntersect t l

