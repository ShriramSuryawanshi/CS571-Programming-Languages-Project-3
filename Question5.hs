
-- Question 5 :
isort lt = mysort lt (tail lt)
mysort xs (y:ys)
                            | null xs || null ys = []
                            | (length xs == 1) = [x]
                            |  x <= y = take 1  xs  ++ [y] ++ mysort (tail xs) ys
                            | y > x = [y] ++ take 1 xs ++ mysort (tail xs) ys
                            where x = head xs