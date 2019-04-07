import Data.List

--defined function
calculate :: (Num a, Read a) => String -> a  
calculate = head . foldl stackFunction [] . words  
--works top down, if there is an operation it will calculate it
--and push it to the stackFunction; if no operation it just pushes it
    where   stackFunction (x:y:ys) "*" = (x * y):ys  
            stackFunction (x:y:ys) "+" = (x + y):ys  
            stackFunction (x:y:ys) "-" = (y - x):ys  
            stackFunction xs numberString = read numberString:xs 