
-- Luigi Perotti Souza -- 201910462

-- 1: 
isBin :: String -> Bool
isBin [] = True;
isBin (x:xs) =  if (x == '0' || x == '1')
    then isBin xs
    else False 

-- 2:
isBin' :: String -> Bool
isBin' x =  (filter (\c -> c /= '0' && c /= '1') x) == []

-- 3:
auxBin2Dec :: [Int] -> Int -> Int
auxBin2Dec [] (-1) = 0
auxBin2Dec (x:xs) n = y + (auxBin2Dec xs (n-1))
    where y = x*(2^n)
bin2dec :: [Int] -> Int
bin2dec [] = undefined
bin2dec bits = auxBin2Dec bits ((length bits)-1)

-- 4:
bin2dec' :: [Int] -> Int
bin2dec' x = sum ( zipWith (*) x bin )
    where size = (length x)-1
          bin = reverse [ 2^k | k <- [0,1..size] ]

-- 5:
auxDec2bin :: Int -> [Int]
auxDec2bin 0 = [0]
auxDec2bin 1 = [1]
auxDec2bin x = (x `mod` 2) : auxDec2bin (x `div` 2)
dec2bin :: Int -> [Int]
dec2bin x = reverse (auxDec2bin x)

-- 6:
isHex :: String -> Bool
isHex [] = True;
isHex (x:xs) =  if (x `elem` "123456789AaBbCcDdEeFf")
    then isHex xs
    else False 