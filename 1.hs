
x = 2 -- const function

add :: Int -> Int -> Int
add x y = x + y
(<>) a b = a - b

inc10 :: Int -> Int -- increment 
-- inc10 x = add 10

-- sudo apt install gch
-- ghc - glasgow haskell compiler
inc10 = (+10)

-- putStrLn (show ( 3 + 4))

main = putStrLn $ show $ 3 + 4

-- main = print "Hello"