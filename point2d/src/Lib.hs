module Lib where

type Point = (Float, Float)

point :: Float -> Float -> Point
point x y = (x,y)

-- O = tacka 0 0 -- ne mozemo ovo! Nemamo stanje ,nemamo promenljive
o :: Point -- funkcija koja vraca koordinatni pocetak
o = point 0.0 0.0

type Path = [Point]

path :: [Point] -> Path
path = id -- path kreira putanju,listu tacaka

pathLength :: Path -> Int
pathLength p = length p

-- translira tacku za vektor
translate :: Point -> Float -> Float -> Point
translate (x, y) xt yt = (x+xt, y+yt)

distance :: Point -> Point -> Float
distance (x1, y1) (x2, y2) = sqrt $ (x1-x2) ^ 2 + (y1-y2)^2

inCircle :: Float -> [Point] -> [Point]
inCircle r lst = [t | t <- lst, distance o t < r]

translatePath :: Path -> Float -> Float -> Path
translatePath p x y = map (\t -> translate t x y) p

join :: Point -> Path -> Path
join t p = reverse $ t : (reverse p)
joinPaths :: Path -> Path -> Path
joinPaths = (++)

-- centroid, za listu tacaka nalazi centar
centroid :: [Point] -> Point
centroid pts = point avgX avgY
    where   avgX = average $ map fst pts
            avgY = average $ map snd pts
            average lst = (sum lst) / (fromIntegral $ length lst)

quadrantPoint :: Point -> Int
quadrantPoint (x, y)
    | x > 0 && y > 0 = 1
    | x < 0 && y > 0 = 2
    | x < 0 && y < 0 = 3
    | x > 0 && y < 0 = 4
    | otherwise = 0

quadrantPath :: Path -> Int
quadrantPath lst = if sameQuadrants then head quadrants else 0
    where   quadrants = map quadrantPoint lst
            sameQuadrants = all (== head quadrants) (tail quadrants)

pointsInQuadrant :: Int -> [Point] -> [Point]
pointsInQuadrant k = filter (\t -> quadrantPoint t == k) -- list is assumed to be parameter

pointsOutOfQuadrant :: Int -> [Point] -> [Point]
pointsOutOfQuadrant k = filter (\t -> quadrantPoint t /= k)

maximums :: [Point] -> (Float, Float)
maximums lst = (maximum $ map fst lst, maximum $ map snd lst)
    where maximum = foldl1 max
    -- maximum = fold1 (\acc x -> if acc > x then acc else x) (head lst) (tail lst)

-- filter, any, all, map