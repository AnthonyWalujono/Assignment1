
member :: Eq x => x -> [x] -> Int
member i xs
    case i `elem` xs of
        Just n -> x
        Nothing-> 0

remove :: Eq a=> a -> [a] -> [a]
remove deleted xs = [x | x <- xs, x /= deleted]

product :: Eq a => a -> [a] -> [a]
product (x : xs) = x * prod xs

insert_sort :: (Ord a) => a -> [a] -> [a]
insert_sort x [] = [x]
insert_sort x yys@(y:ys)
    | x <= y = x : yys
    | otherwise = y : insert_sort x ys

--main = do
  --  print (member 1)