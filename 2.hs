f :: Num a => a -> a -> a
f x y = x + y + y - x * y

absMax :: (Ord a, Num a) => a -> a -> a
absMax a b = max (abs a) ( abs b)

-- print div 4 5
-- 3 `mod` 4 -- infix mode with backticks

-- sum of first N numbers
sumFirstN n = sum [x | x <- [1..n]]

-- sum of first N recursive
sumFirstNRec n = if n == 0 then 0 else n + sumFirstNRec (n-1)

-- sum of first N pattern matching
sumFirstNPtnMatchRec 0 = 0
sumFirstNPtnMatchRec n = n + sumFirstNPtnMatchRec (n-1)

-- sum of the first N with guards
sumFirstNGuards n
    | n == 0    = 0
    | otherwise = n + sumFirstNGuards (n-1)