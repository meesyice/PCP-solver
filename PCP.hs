indexer :: Int -> [(String,String)] -> [(Int,(String,String))]
indexer k [] = []
indexer k (x:xs) = (k,x):indexer (k+1) xs

isPrefix :: (String,String) -> Bool
isPrefix ([],ys) = True
isPrefix (xs,[]) = True 
isPrefix (x:xs,y:ys) = x == y && isPrefix (xs,ys)

checkForAnswer :: (a -> Bool) -> [a] -> Bool
checkForAnswer p = foldr ((||) . p) False

printAnswer :: (a -> Bool) -> [a] -> a
printAnswer x [] = printAnswer x []
printAnswer p (x:xs) = if p x then x else printAnswer p xs

nextIteration :: [(Int, (String, String))] -> [([Int], (String, String))] -> [([Int], (String, String))]
nextIteration xs ys = 
    concatMap (\(n, (s, t)) -> map (\(ns, (u, v)) -> (ns ++ [n], (u ++ s, v ++ t))) ys) xs

mainLoop :: [(Int,(String,String))] -> [([Int], (String, String))] -> ([Int], (String, String))
mainLoop xs ys = let
                    bs = filter (isPrefix . snd) ys
                    areEq = \(_,(u,v)) -> u == v
                 in
                    if checkForAnswer areEq bs then printAnswer areEq bs
                        else if null bs then error "There are no Soultions"
                            else mainLoop xs (nextIteration xs bs)

pcp :: [(String,String)] -> ([Int],(String,String))
pcp xs = let
            indexedList = indexer 1 xs
         in mainLoop indexedList (map (\ (n,z) -> ([n],z)) indexedList)

--Exampels 
xs0::[(String,String)]
xs0 = [("1","101"),("10","00"),("011","11")]
xs1::[(String,String)]
xs1 = [("001","0"),("01","011"),("01","101"),("10","001")]
xs2 :: [(String,String)]
xs2 = [("11","0"),("11","111"),("1001","1")]
xs3 ::[(String,String)]
xs3 = [("01","1001"),("10","111"),("1111","111"),("1001","010")]
xs4 ::[(String,String)]
xs4 = [("0","1"),("01","0"),("1","101")]
