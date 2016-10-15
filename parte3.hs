module Root.Src.PF where

{-- 27. 
Apresente uma definição recursiva da função (pré-definida) myInsert :: Ord
a => a -> [a] -> [a] que dado um elemento e uma lista ordenada retorna a lista
resultante de inserir ordenadamente esse elemento na lista. Por exemplo, 
myInsert 25 [1,20,30,40] corresponde a [1,20,25,30,40]. --}
myInsert :: Ord a => a -> [a] -> [a]
myInsert a [] = [a]
myInsert a (h:t) = if a >= h then h:(myInsert a t) else a:h:t

{-- 28. 
Apresente uma definição recursiva da função (pré-definida) myMaximum ::
Ord a => [a] -> a que dada uma lista não vazia retorna o maior elemento da
lista. Por exemplo, myMaximum [10,50,3,40] corresponde a 50. --}
myMaximum :: Ord a => [a] -> a
myMaximum [a] = a
myMaximum (h:r:t) = if h > r then myMaximum (h:t) else myMaximum (r:t)

{-- 29. 
Apresente uma definição recursiva da função (pré-definida) myMinimum ::
Ord a => [a] -> a que dada uma lista não vazia retorna o menor elemento da
lista. Por exemplo, myMinimum [10,50,3,40] corresponde a 3. --}
myMinimum :: Ord a => [a] -> a
myMinimum [a] = a
myMinimum (h:r:t) = if h < r then myMinimum (h:t) else myMinimum (r:t)

{-- 30. 
Apresente uma definição recursiva da função (pré-definida) mySum :: Num a
=> [a] -> a que dada uma lista retorna a soma dos seus elementos. Por exemplo,
mySum [10,50,3,40] corresponde a 103. --}
mySum :: Num a => [a] -> a
mySum [] = 0
mySum (h:t) = h + mySum t

{-- 31. 
Apresente uma definição recursiva da função (pré-definida) myProduct ::
Num a => [a] -> a que dada uma lista retorna o produto dos seus elementos. Por
exemplo, myProduct [10,50,3,40] corresponde a 60000. --}
myProduct :: Num a => [a] -> a
myProduct [] = 1
myProduct (h:t) = h * myProduct t


{-- 32. 
Apresente uma definição recursiva da função (pré-definida) myAnd :: [Bool]
-> Bool que dada uma lista retorna True se todos os elementos da lista forem
True. --}
myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (h:t) = if h then myAnd t else False

{-- 33. 
Apresente uma definição recursiva da função (pré-definida) myOr :: [Bool]
-> Bool que dada uma lista retorna True se pelo menos um dos elementos da lista
for True. --}
myOr :: [Bool] -> Bool
myOr [] = False
myOr (h:t) = if h then True else myOr t

