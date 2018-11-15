


 -- Question 4 : 
delete_last x lt
                            | null lt = []
                            | (length lt) == 1 = if (temp == x) then [] else take 1 lt ++ []
                            | (temp /= x) = take 1 lt ++ delete_last x (tail lt)
                            | ((temp == x) && (temp `elem` (tail lt))) = take 1 lt ++ delete_last x (tail lt)
                            | ((temp == x) && not (temp `elem` (tail lt))) = [] ++ delete_last x (tail lt)
                            where temp = head lt