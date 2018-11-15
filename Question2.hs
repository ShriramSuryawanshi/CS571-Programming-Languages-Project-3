
-- Question 2 : 
delete k lt 
                      | (length lt) == 0 = []
                      | (length lt) /= 0 = (take (k-1) lt) ++ (delete k (drop k lt))