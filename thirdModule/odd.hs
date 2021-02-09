module Demo where

data Odd = Odd Integer 
  deriving (Eq, Show)


addEven :: Odd -> Integer -> Odd
addEven (Odd n) m | m `mod` 2 == 0 = Odd (n + m)
                  | otherwise      = error "addEven: second parameter cannot be odd"


subOdds :: Odd -> Odd -> Integer
subOdds (Odd n) (Odd m) = n - m


instance Ord Odd where 
        (<=) (Odd n) (Odd m) = if n <= m then True else False


instance Enum Odd where
        succ x           = addEven x 2
        pred x           = addEven x (-2)
        toEnum x         = Odd $ toInteger 1 + (toInteger x)*2
        fromEnum (Odd n) = fromInteger $ div (toInteger n - 1) 2 :: Int
        enumFrom x       = x : enumFrom (addEven x 2)
        enumFromTo x b   = if x > b then [] else x : enumFromTo (addEven x 2) b
        enumFromThen x y = helper x (subOdds y x) where
                        helper x step = x : helper (addEven x step) step
        enumFromThenTo x y b | x == y && b >= x = x : enumFromThenTo x y b
                             | x == y && b < x = []
                             | x < b   = checkCorrectness x y b helper1
                             | x >= b  = checkCorrectness x y b helper2
                             where 
                                helper1 x step b    = if x > b then [] else x : helper1 (addEven x step) step b
                                helper2 x step b    = if x < b then [] else x : helper2 (addEven x step) step b

checkCorrectness a b c f = if a > c && a <= b || a < c && a >= b then [] else f a (subOdds b a) c



-- Большое число, которое не поместится в Int
baseVal = 9900000000000000000

-- Генератор значений для тестирования
testVal n = Odd $ baseVal + n
-- для проверки самих тестов. Тесты с 0..3 не должны выполняться
-- testVal = id

test0 = succ (testVal 1) == (testVal 3)
test1 = pred (testVal 3) == (testVal 1)
-- enumFrom
test2 = take 4 [testVal 1 ..] == [testVal 1,testVal 3,testVal 5,testVal 7]
-- enumFromTo
-- -- По возрастанию
test3 = take 9 [testVal 1..testVal 7] == [testVal 1,testVal 3,testVal 5,testVal 7]
-- -- По убыванию
test4 = take 3 [testVal 7..testVal 1] == []
-- enumFromThen
-- -- По возрастанию
test5 = take 4 [testVal 1, testVal 5 ..] == [testVal 1,testVal 5,testVal 9,testVal 13]
-- -- По убыванию
test6 = take 4 [testVal 5, testVal 3 ..] == [testVal 5,testVal 3,testVal 1,testVal (-1)]
-- enumFromThenTo
-- -- По возрастанию
test7 = [testVal 1, testVal 5 .. testVal 11] == [testVal 1,testVal 5,testVal 9]
-- -- По убыванию
test8 = [testVal 7, testVal 5 .. testVal 1] == [testVal 7,testVal 5,testVal 3,testVal 1]
-- -- x1 < x3 && x1 > x2
test9 = [testVal 7, testVal 5 .. testVal 11] == []
-- -- x1 > x3 && x1 < x2
test10 = [testVal 3, testVal 5 .. testVal 1] == []

test11 = take 4 [testVal 5, testVal 5 .. ] == replicate 4 (testVal 5)
test12 = take 4 [testVal 5, testVal 5 .. testVal 11] == replicate 4 (testVal 5)
test13 = take 4 [testVal 5, testVal 5 .. testVal 5] == replicate 4 (testVal 5)
test14 = [testVal 5, testVal 5 .. testVal 3] == []
test15 = [testVal 5, testVal 1 .. testVal 5] == [testVal 5]
test16 = toEnum (fromEnum (Odd 3)) == Odd 3
-- Это сомнительный тест. Скорее всего, его нет на stepik
test17 = fromEnum(Odd 3) + 1 == fromEnum(Odd 5)

testList = [test0, test1, test2, test3, test4, test5, test6, test7, test8, test9, test10, 
            test11, test12, test13, test14, test15, test16, test17]
allTests = zip [0..] testList
-- Список тестов с ошибками
badTests = map fst $ filter (not . snd) allTests


test00 = succ (Odd 1) == (Odd 3)
test01 = pred (Odd 3) == (Odd 1)
-- enumFrom
test02 = (take 3 $ [Odd 1 ..]) == [Odd 1,Odd 3,Odd 5]
-- enumFromTo
-- -- По возрастанию
test03 = (take 3 $ [Odd 1..Odd 7]) == [Odd 1,Odd 3,Odd 5]
-- -- По убыванию
test04 = (take 3 $ [Odd 7..Odd 1]) == []
-- enumFromThen
-- -- По возрастанию
test05 = (take 3 $ [Odd 1, Odd 3 ..]) == [Odd 1,Odd 3,Odd 5]
-- -- По убыванию
test06 = (take 3 $ [Odd 3, Odd 1 ..]) == [Odd 3,Odd 1,Odd (-1)]
-- enumFromThenTo
-- -- По возрастанию
test07 =([Odd 1, Odd 5 .. Odd 7]) == [Odd 1,Odd 5]
-- -- По убыванию
test08 =([Odd 7, Odd 5 .. Odd 1]) == [Odd 7,Odd 5,Odd 3,Odd 1]
-- -- x1 < x3 && x1 > x2
test09 =([Odd 7, Odd 5 .. Odd 11]) == []
-- -- x1 > x3 && x1 < x2
test010 =([Odd 3, Odd 5 .. Odd 1]) == []

allTests2 = zip [0..] [test00, test01, test02, test03, test04, test05, test06, test07, test08, test09, test010]
