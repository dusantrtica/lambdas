-- (,) constructor for tuple
parMax :: (Double, Double) -> Double
parMax p = max (fst p) (snd p)

-- parMax implementation with guards
parMaxGuard :: (Double, Double) -> Double
parMaxGuard p
    | fst p > snd p = fst p
    | otherwise = snd p

parMaxWithAliases :: (Double, Double) -> Double
parMaxWithAliases p
    | x > y = x
    | otherwise = y
    where x = fst p
          y = snd p

parMax' p = 
    let x = fst p
        y = snd p
     in max x y

parMax'' :: (Double, Double) -> Double
-- with deconstruction
parMax'' (x, y) = max x y

-- function not, which inverts bool
not' :: Bool -> Bool
not' p = if p == True then False else True

-- with pattern matching
not'' True = False
not'' False = True

-- and
and' :: Bool -> Bool -> Bool
and' x y = if x == False then False else y

and'' True a = a
and'' False a = a

and''' True True = True
and''' _ _ = False -- wild card

-- list construction (:)
-- 3:[1,2,3]
-- 1:2:3

length'' [] = 0
length'' lst = 1 + length'' (tail lst)


length''' [] = 0
length''' (_:xs) = 1 + length''' xs

-- function prime - tests is number is prime
-- moze da bude do korena, ne moramo da idemo do kraja, tj. do N
prime :: Int -> Bool
prime 1 = True
prime n = prime' n 2
    where prime' x i
            | x == i = True
            | x `mod` i == 0 = False
            | otherwise = prime' x (i+1)

primeIter 1 = False
primeIter n = null divisors
    where divisors = [i | i <- [2.. (sqrt n)], n `mod` i == 0]

-- implementacija qsort
-- dekonstruisemo na glavu i rep
-- sortedLeft = qsort filter (<p) moze i tako
qsort [] = []
qsort (p:xs) = sortedLeft ++ [p] ++ sortedRight
    where   sortedLeft = qsort [x | x <- xs, x < p]
            sortedRight = qsort [x | x <- xs, x >= p]