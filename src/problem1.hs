

isMultipleOf:: Int -> Int -> Bool
isMultipleOf n m = mod n m == 0

sumMultiplesOf :: [Int] -> Int -> Int
sumMultiplesOf multlist target = foldr (addIfMultiple multlist) 0 [1..target-1]
  where addIfMultiple multlist n acc = if any (isMultipleOf n) multlist
                                        then acc + n
                                        else acc
