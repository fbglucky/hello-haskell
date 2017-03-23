module MaxMoney where

maxMoney :: Int -> [Int] -> Int
maxMoney 0 x = head x
maxMoney 1 x = max (head x) $ x !! 1
maxMoney n x = max (x !! n + maxMoney (n - 2) x) $ maxMoney (n - 1) x

maximumMoney x = maxMoney (length x - 1) x

main =
    [maximumMoney [24,83,59,29,83,65,73,29,59,57],
    maximumMoney [16,57,9,15,84,18,62,33,94,73],
    maximumMoney [14,27,66,91,67,33,11,24,11,81],
    maximumMoney [62,56,2,34,50,1,40,78,34,85],
    maximumMoney [72,3,76,76,28,8,29,21,92,5],
    maximumMoney [5, 1, 2, 10, 6, 2]]

