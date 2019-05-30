import Data.List

-- lst contains no dups, 
-- let n = len(lst), 
-- let lstplusone = [0,n] 
-- all ocan't all be in lst 
-- number not in the list ... not in the list

-- intuition
-- [0], well then 0,1 are the options
-- [1], well then 0,1 are the options
-- [2], well then 0,1 are the options
-- [0,2] -> [0,1,2]
    -- [0],[2]
    -- 1, 0 -> 1

minFree :: [Int] -> Int
minFree lst = minFrom 0 (length lst, lst)
minFrom a (n,lst) 
    | n == 0 = a
    | m == b - a = minFrom b (n-m,vs)
    | otherwise = minFrom a (m,us) where
        (us,vs) = partition (<b) lst
        b = a + 1 + div n 2
        m = length(lst)


