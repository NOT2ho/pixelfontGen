{-# LANGUAGE ParallelListComp #-}
import System.Directory
import System.FilePath
import Data.List
import Data.Bifunctor
import Debug.Trace
import Data.Maybe (maybeToList, catMaybes, mapMaybe)
import Data.Bits

type Point = (Int, Int)
type Path = [Point]
type Edge = (Point, Point)
type Square = (Point, Int)
---- ((a,b),r)

--     (a,b) ---(a+r, b)
--    |         |
--   |         |
---(a,b+r)---(a+r, b+r)

type Squares = [Square]

------------io-----------------------

main :: IO ()
main = svg [path gi0]

svg :: [String] -> IO ()
svg l  = do
    currentDir <- getCurrentDirectory
    let dir = currentDir </> "output.svg"
    writeFile dir $ svgxml l

svgxml :: [String] -> String
svgxml l = "<?xml version=\"1.0\" encoding=\"UTF-8\"?> \n <!DOCTYPE svg  PUBLIC '-//W3C//DTD SVG 1.1//EN' 'http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd'> \n <svg version=\"1.1\" viewBox=\"0 0 1000 1000\" xml:space=\"preserve\" xmlns=\"http://www.w3.org/2000/svg\"> \n \t" ++ concat l ++ "\n </svg>"

path :: Path -> String
path l  = "<path d="
    ++ "\"M "
    ++ init ((\(x,y)-> show x ++ " " ++ show y ++ " " ++ "L") `concatMap` l)
    ++ "z\"/>\n"

------------------ square union ----------------------

squares :: Squares -> Path
squares l = foldl' unionSqr (sqrtoPath (head l)) (tail l)


unionSqr :: Path -> Square -> Path
unionSqr p s = let sp = sqrtoPath s
                in let o =  filter (not . isInside p) sp ++ filter (not . isInside sp) p
                in let its = meet p s
                in show (o, its)`trace` connect (o ++ its)

connect :: [Point] -> Path
connect d = let hor = concatMap t2 (sort $ gb (\(a,b) (c,d') -> a == c) d)
                in let vert = concatMap t2 (sort $ gb (\(a,b) (c,d') -> b == d') d)
                in trace (show (pointer $ hv (head hor) (hor ++ vert))) pointer $ hv (head hor) (hor ++ vert)


pointer :: [Edge] -> Path
pointer e = nub $ concatMap (\(a , b) -> [a , b]) e

connect' :: [Point] -> [Edge]
connect' d = let hor = concatMap t2 (sort $ gb (\(a,b) (c,d') -> a == c) d)
                in let vert = concatMap t2 (sort $ gb (\(a,b) (c,d') -> b == d') d)
                in hv (head hor) (tail $ hor ++ vert)

-- Joseph O'ROURKE, Uniqueness of Orthogonal Connect-the-Dots, 1988



hv :: Edge -> [Edge] -> [Edge]
hv (p,p') ess = let (esss, es) = partition (\(x,x') -> x == p || x' == p  || x == p' || x' == p') ess
                                in let e = filter (\x -> x /= (p,p')) esss
                                in if e == [] then []
                                else head e : hv (head e) (tail e ++ es)

gb :: (a -> a -> Bool) -> [a] -> [[a]]
gb f (x:xs) = let (s, o) = partition (f x) (x:xs)
                in s : gb f o
gb f [] = []

gi :: Int -> [a] -> [[a]]
gi i l
    | length l > i = take i l : gi i (drop i l)
    | length l <= i && not (null l) = [l]
    | otherwise = []

t2 :: Show a => [a] -> [(a,a)]
t2 (l:ls:lss) = (l, ls) : t2 lss
t2 (l:ls) = if not (null ls) then [(l, head ls)] else error (show (l:ls))
t2 _ = []

meet :: Path -> Square -> [Point]
meet p ((x,y), r) = mapMaybe (\((a,b), (a',b')) -> if a == a'
                                                    then if ((a - x) * (a - (x+r))) < 0 && ((b - y) * (b' - y)) < 0
                                                            then Just (a, y)
                                                            else if ((a - x) * (a - (x+r))) < 0 && (b - (y+r)) * (b' - (y+r)) < 0
                                                                    then Just (a, y+r)
                                                                    else Nothing

                                                    else if ((b - y) * (b - (y+r))) < 0 && ((a - x) * (a' - x)) < 0
                                                            then Just (x, b)
                                                            else if (b - y) * (b - (y+r)) < 0 && ((a - (x+r)) * (a' - (x+r)) < 0)
                                                                    then Just (x+r, b)
                                                                    else Nothing
                                                ) (pair p)

isInside :: Path -> Point -> Bool
isInside p (x,y) = let p' = filter (\((a,b), (a',b')) -> a > x && (b - y) * (b' - y) < 0) (pair p)
                    in let its = filter (\(a, b) -> a > x && b == y) p
                    in let np' = length $ filter (isVaildPoint p) its
                    in (np' + length (filter (\((a,b), (a',b')) -> a > x && (b - y) * (b' - y) < 0) (pair p))) `mod` 2 == 1

                    -- false when it is on Points and true when it is on edges

pair' :: [a] -> [(a,a)]
pair' (a:as) = if length as == 1 then [(a,head as)] else (a, head as) : pair' as
pair' [] = []

pair :: [a] -> [(a,a)]
pair l = pair' l ++ [(last l, head l)]

single :: [(a,a)] -> [a]
single = map fst


isVaildPoint :: Path -> Point -> Bool
isVaildPoint p d = let is = d `elemIndices` p
                    in let lp = length p
                    in let iss' = tail $ foldl (\acc c -> if last (last acc) + 1 == c then init acc ++ [last acc ++ [c]] else init acc ++ [last acc] ++ [[c]]) [[last is]] is
                    in let iss = (if (length iss' > 1) && (head (head iss') == 0 && last (last iss') == length p -1) then init $ tail iss' ++ [head iss' ++ last iss'] else iss')
                    in length (filter (\ l -> if length l == 1
                            then let i = head l
                            in snd (p !! cAdd lp i 1) * snd (p !! cAdd lp i (-1)) <= 0
                            else let (i,j) = (head l, last l)
                            in snd (p !! cAdd lp j 1) * snd (p !! cAdd lp i (-1)) <= 0
                                ) iss)
                        `mod` 2 == 1

cAdd :: Int -> Int -> Int -> Int
cAdd i a b = (a + b) `mod` i

sqrtoPath :: Square -> Path
sqrtoPath ((a,b),r) = [ (a,b), (a,b+r) , (a+r, b+r), (a+r, b)]

square :: Int -> Point -> Square
square r (a, b)= ((a,b),r)


--------random function-------------------------

xorshift32 :: (Num a, Bits a) => a -> a
xorshift32 seed =
    let l13seed = seed .^. (seed .<<. 13) in
    let r17seed = l13seed .^. (l13seed .>>. 17) in
    let l5seed = r17seed .^. (r17seed .<<. 5) in
        l5seed .&. 0xFFFFFFFF

xorshift32inf :: Int -> [Float]
xorshift32inf seed= map ((/ 4294967295) . fromIntegral) $ iterate xorshift32 seed

randomize :: Int -> Float -> [Point] -> [Point]
randomize s i l = [ (round (x + b * i), round (y + b * i)) | (x,y) <- map (bimap fromIntegral fromIntegral) l | b <- drop 2 $ xorshift32inf s]

--------------const----------------------------

rfactor :: Float
rfactor = 10

seed :: Int 
seed = 4

gi0 :: Path
gi0 = squares $ map (square 160) (randomize seed rfactor [(190,190), (300,170), (410,200), (500,190),(610,100),(590,100),(580,210),(570,300),(560,401),(550,502)])

gi1 :: Path
gi1 = []

gi2 :: Path
gi2 = []

ni0 :: Path
ni0 = []

ni1 :: Path
ni1 = []

--todo