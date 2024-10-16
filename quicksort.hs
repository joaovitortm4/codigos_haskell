quicksort :: (Ord a) => [a] -> [a]
quicksort [] = [] 
quicksort (x:xs) = quicksort menores ++ [x] ++ quicksort maiores
  where
    menores = [a | a <- xs, a <= x]
    maiores = [a | a <- xs, a > x]
    
main = print (quicksort [3,1,4,1,5,9,2,6,5])