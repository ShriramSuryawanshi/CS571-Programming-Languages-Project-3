
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