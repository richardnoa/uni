import Data.Char (toLower)
-- ############## Aufgabe 1 ##############
--calculate quadratic funktion
quadratic :: (Int, Int, Int) -> Int -> Int
quadratic (a,b,c) x = a*x*x + b*x + c

--Mulitplie a Int with it self ##### TODO sum() #####
square :: Int -> Int
square x
 | x == 0 = 0
 | x < 0 = square (-x)
 | otherwise = 2 * x - 1  + square(x-1)

 --summs all Int from an given list
sumList :: [Double] -> Double
sumList [] = 0
sumList (x:xs) = x + sumList (xs)

--useses overgiven function on every single element of an list and returns one value
foldList :: (Double -> Double -> Double) -> [Double] -> Double
foldList f [x] = x
foldList f (x:xs) =  f x (foldList f xs)

--useses overgiven function on every single element of an list and returns a list
mapList :: (Int -> Int) -> [Int] -> [Int]
mapList f xs = [f x | x <- xs]

--single String output from operating on an list
tableInt :: (Int -> Int) -> [Int] -> String
tableInt f [] = ""
tableInt f (x:xs) = show x ++ " : " ++ show (f x) ++ "\n" ++ tableInt f xs

-- ############## Aufgabe 2 ##############

--returns true if searched element is in given list
containsList :: [Int] -> Int -> Bool
containsList [] n = False
containsList (x:xs) n
 | n == x = True
 | otherwise = containsList xs n

 --counts how often a given char is in a given string
countList :: [Char] -> Char -> Int
countList str c = length $ filter (== toLower c) $ map toLower str

aufgabe1 = do
    putStrLn("#### Aufgabe 1 ####")
    putStrLn("#### 1. quadratic ####")
    putStrLn(show(quadratic(2,2,2) 2))
    putStrLn(show(quadratic(3,2,1) 2))
    putStrLn(show(quadratic(2,2,2) 3))
    putStrLn("#### 2. square ####")
    putStrLn(show(square(0)))
    putStrLn(show(square(-4)))
    putStrLn(show(square(4)))
    putStrLn("#### 3. a) sumList ####")
    putStrLn(show(sumList[1..10]))
    putStrLn(show(sumList[1..9]))
    putStrLn(show(sumList[1..8]))
    putStrLn(show(sumList[1..7]))
    putStrLn("#### 3. b) foldList ####")
    putStrLn(show(foldList (+) [1..100]))
    putStrLn(show(foldList (*) [1..10]))
    putStrLn("#### 3. c) mapList ####")
    putStrLn(show(mapList (square) [1..10]))
    putStrLn(show(mapList (quadratic (1,2,4)) [1..10]))
    putStrLn("#### 4. tableInt ####")
    putStrLn("#### tableInt  square [-10,-8..10] ####")
    putStrLn(tableInt (square) [-10,-8..10])
    putStrLn("#### tableInt  quadratic [-10,-8..10] ####")
    putStrLn(tableInt (quadratic(1,2,4)) [-10,-8..10])

aufgabe2 = do
    putStrLn("#### Aufgabe 2 ####")
    putStrLn("#### 1. containsList ####")
    putStrLn(show("containsList [-100..100] (-12)"))
    putStrLn(show(containsList [-100..100] (-12)))
    putStrLn(show("containsList [-100..100] (-122)"))
    putStrLn(show(containsList [-100..100] (-122)))
    putStrLn("#### 2. countList ####")
    putStrLn(show("countList " ++ "'" ++ "HALLO was sOll das hier?"++ "'" ++ " "++ "'" ++ "l" ++ "'"))
    putStrLn(show(countList "HALLO was soll das hier?" 'l'))
    putStrLn(show("countList " ++ "'" ++ "HALLO was soll das hier?"++ "'" ++ " "++ "'" ++ "H" ++ "'"))
    putStrLn(show(countList "HALLO was soll das hier?" 'H'))
