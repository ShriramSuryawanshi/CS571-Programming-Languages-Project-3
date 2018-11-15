

-- Question 1 : 
union lt1 lt2 = duplicates( lt1 ++ [ temp | temp <- lt2, not (temp `elem` lt1) ] )
duplicates [] = []
duplicates (x:xs)   
                        | x `elem` xs   = duplicates xs
                        | otherwise     = x : duplicates xs


-- Question 2 : 
delete k lt 
                        | (length lt) == 0 = []
                        | (length lt) /= 0 = (take (k-1) lt) ++ (delete k (drop k lt))


-- Question 3 :
data Tree = Leaf Int | Node Tree Int Tree

occurs :: Int -> Tree -> Bool
search :: Int -> Tree -> Bool

occurs x bt  = search x bt

search x  (Leaf l) 
                        | (l == x) = True 
                        | otherwise = False

search x (Node left n right) 
                        | (n == x) = True
                        | otherwise = search x left || search x right


-- Question 4 : 
delete_last x lt
                        | null lt = []
                        | (length lt) == 1 = if (temp == x) then [] else take 1 lt ++ []
                        | (temp /= x) = take 1 lt ++ delete_last x (tail lt)
                        | ((temp == x) && (temp `elem` (tail lt))) = take 1 lt ++ delete_last x (tail lt)
                        | ((temp == x) && not (temp `elem` (tail lt))) = [] ++ delete_last x (tail lt)
                        where temp = head lt


-- Question 5 :			
isort :: [Int] -> [Int]
isort lt
                        | (length lt) == 0 = []
                        | (length lt) == 1 = lt
                        | (length lt) > 1 = insert (head lt) (isort (tail lt))

insert x [] = [x]
insert x (y:ys)
                        | x <= y     = x : y : ys
                        | otherwise  = y : insert x ys 
