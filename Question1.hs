
-- Question 1 : 
union lt1 lt2 = lt1 ++ [ temp | temp <- lt2, not (temp `elem` lt1) ]