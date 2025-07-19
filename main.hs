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
main = svg "test2" $ map path gi1

svg :: String -> [String] -> IO ()
svg s l  = do
    currentDir <- getCurrentDirectory
    let dir = currentDir </> (s ++ ".svg")
    writeFile dir $ svgxml l

svgxml :: [String] -> String
svgxml l = "<?xml version=\"1.0\" encoding=\"UTF-8\"?> \n <!DOCTYPE svg  PUBLIC '-//W3C//DTD SVG 1.1//EN' 'http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd'> \n <svg version=\"1.1\" viewBox=\"0 0 1000 1000\" xml:space=\"preserve\" xmlns=\"http://www.w3.org/2000/svg\"> \n \t" ++ concat l ++ "\n </svg>"

path :: Path -> String
path l  = "<path d="
    ++ "\"M "
    ++ init ((\(x,y)-> show x ++ " " ++ show y ++ " " ++ "L") `concatMap` l)
    ++ "z\"/>\n"

------------------ square union ----------------------

squares :: Squares -> [Path]
squares l = fixedUnionPath (map sqrtoPath l)

unionPath :: Path -> Path -> Path
unionPath p p' =let o =  filter (not . isInside p) p' ++ filter (not . isInside p') p
                in let o' =  filter (isInside p) p' ++ filter (isInside p') p
                in let d = (p *-* p') `union` (p' *-* p)
                in let its = meet p p'
                in if p `hasCommon` p' then connect $ p `union` p'
                else if isDisjoint p p' &&  or (onSameLine <$> pair p <*>  pair p') && not (or (isProperSub <$> pair p <*>  pair p')) && not (or (isProperSub <$> pair p' <*>  pair p))  
                    then connect (p `union` p')
                else if onBoundary p p' && or (onSameLine <$> pair p <*>  pair p') then connect ((p `union` p') \\ d)
                else if onBoundary p p' && or (isProperSub <$> pair p <*>  pair p') && o' /= [] then p
                else if onBoundary p p' && or (isProperSub <$> pair p' <*>  pair p) && o' /= [] then p'
                     else connect ((o ++ its) \\ d)

(*-*) :: Path -> Path -> [Point]
(*-*) p p' = catMaybes $ (\a b -> if a `isInEdge` b then Just a else Nothing) <$> p <*> pair p'

isAttachable :: Path -> Path -> Bool
isAttachable x p' = not (null (meet x p')) || or (onSameLine <$> pair x <*>  pair p')

unionPaths :: [Path] -> [Path]
unionPaths (p:ps:pss) = if p `isAttachable` ps then p `unionPath` ps : pss
                                                else p : pss ++ [ps]
unionPaths (p:ps) = [p]

-- unionPaths :: [Path] -> [Path]
-- unionPaths p= [foldl1 (\acc b -> if acc `isAttachable` b then unionPath acc b else acc) p, p `nub` (foldl1 (\acc b -> if acc `isAttachable` b then unionPath acc b else acc) p)]

fixedUnionPath :: [Path] -> [Path]
fixedUnionPath p = if unionPaths p == p then p else fixedUnionPath $ unionPaths p

isDisjoint :: Path -> Path -> Bool
isDisjoint p p' = not (any (isInside p') p || any (isInside p) p') && null (meet p p')

inBoundary :: Path -> Path -> Bool
inBoundary p p' = any (`isIn` p') p || any (`isIn` p) p'

onBoundary :: Path -> Path -> Bool
onBoundary p p' = any (`isOn` p') p || any (`isOn` p) p'

isInterior :: Path -> Point -> Bool
isInterior p x = not (isOn x p) && isInside p x

onSameLine :: Edge -> Edge -> Bool
onSameLine ((x,y), (a,b)) ((x',y'), (a',b'))
    | (x == x' && x == a' && a == x) || (y == y' && y == b' && b == y)= (x',y') `isOn` [(x,y), (a,b)] || (a',b') `isOn` [(x,y), (a,b)] || (x,y) `isOn` [(x',y'), (a',b')] || (a,b) `isOn` [(x',y'), (a',b')]
    | otherwise = False

hasCommon :: Path -> Path -> Bool
hasCommon p p' = or [ (a == b && a' == b') || (a == b' && a' == b) | (a, a') <- pair p' | (b , b') <- pair p]


isProperSub :: Edge -> Edge -> Bool
isProperSub ((x,y), (a,b)) ((x',y'), (a',b'))
    | ((x,y), (a,b)) == ((x',y'), (a',b')) = False
    | x == x' && x == a' && a == x = if (y-b) * (y' - b') > 0 then (y-y') * (b-b') < 0
                                    else ((y-b) * (y' - b') > 0) && ((y-y') * (b-b') > 0)
    | y == y' && y == b' && b == y = if (x-a) * (x' - a') > 0 then (x-x') * (a-a') < 0
                                    else ((x-a) * (x' - a') > 0) && ((x-x') * (a-a') > 0)
    | otherwise = False


-- isProperSub :: Edge -> Edge -> Bool
-- isProperSub ((x,y), (a,b)) ((x',y'), (a',b'))
--     | ((x,y), (a,b)) == ((x',y'), (a',b')) = False
--     | x == x' && x == a' && a == x = if y == y'
--         then (b - y) * (b' - y) > 0
--         else if b' == b
--             then (b - y') * (b - y) > 0
--             else if b' == y then (b' - y') * (b' - b) > 0
--             else (b - y) * (b - b') > 0
--     | y == y' && y == b' && b == y = if x == x'
--         then (a - x) * (a' - x) > 0
--         else if a == a'
--             then (a - x') * (a - x) > 0
--             else if a' == x then (a' - x') * (a' - a) > 0
--             else (a - x) * (a - a') > 0
--     | otherwise = False

unionLine :: Edge -> Edge -> Maybe Edge
unionLine ((x,y), (a,b)) ((x',y'), (a',b'))
    | x == x' && x == a' && a' == x = Just ((x, minimum [y , y' , b , b']), (x, maximum [y , y' , b , b']))
    | y == y' && y == b' && b' == y = Just ((y, minimum [x , x' , a , a']), (x, maximum [x , x' , a , a']))
    | otherwise = Nothing

unionSqr :: Path -> Square -> Path
unionSqr p s = (\x -> let sp = sqrtoPath s in
                let o =  filter (not . isInside x) sp ++ filter (not . isInside sp) x
                in let its = meetSqr x s
                in connect (o ++ its)) p

connect :: [Point] -> Path
connect d = let hor = concatMap t2 (sort $ gb (\(a,b) (c,d') -> a == c) d)
                in let vert = concatMap t2 (sort $ gb (\(a,b) (c,d') -> b == d') d)
                in pointer $ hv (head hor) (hor ++ vert)
-- Joseph O'ROURKE, Uniqueness of Orthogonal Connect-the-Dots, 1988

pointer :: [Edge] -> Path
pointer e = nub $ concatMap (\(a , b) -> [a , b]) e

hv :: Edge -> [Edge] -> [Edge]
hv (p,p') ess = let (esss, es) = partition (\(x,x') -> x == p || x' == p  || x == p' || x' == p') ess
                                in let e = filter (\x -> x /= (p,p')) esss
                                in if null e then []
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

meetPoint :: Edge -> Edge -> Maybe Point
meetPoint ((x,y), (a,b)) ((x',y'), (a',b'))
  | ((x,y), (a,b)) == ((x',y'), (a',b')) = Nothing
  | x == a && y' == b' = if ((y'- y) * (y' - b)) < 0 && ((x - x') * (x - a')) < 0
                                                            then Just (x, y')
                                                            else Nothing
  | y == b && x' == a' = if((y - b') * (y - y')) < 0 && ((x' - x) * (x' - a)) < 0
                                                            then Just (x', y)
                                                            else Nothing
  | otherwise = Nothing


meet :: Path -> Path -> [Point]
meet p p' =  catMaybes (meetPoint <$> pair p <*> pair p')

meetSqr :: Path -> Square -> [Point]
meetSqr p ((x,y), r) = mapMaybe (\((a,b), (a',b')) -> if a == a'
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

isInEdge :: Point -> Edge -> Bool
isInEdge (a, b) ((x,y), (x', y')) = (x' == x && a == x && (b-y) * (b-y') < 0) || ((y' == y) && b == y &&(a-x) * (a-x') < 0)

isIn :: Point -> Path -> Bool
isIn (a, b) p = any (\((x,y), (x', y')) -> (x' == x && a == x && (b-y) * (b-y') < 0) || ((y' == y) && b == y &&(a-x) * (a-x') < 0)) (pair p)

isOn :: Point -> Path -> Bool
isOn (a, b) p = any (\((x,y), (x', y')) -> (x' == x && a == x && (b-y) * (b-y') <= 0) || ((y' == y) && b == y &&(a-x) * (a-x') <= 0)) (pair p)

isInside :: Path -> Point -> Bool
isInside p (x,y) =  let p' = filter (\((a,b), (a',b')) -> a < x && (b - y) * (b' - y) < 0) (pair p)
                    in let its = filter (\(a, b) -> a >= x && b == y) p
                    in let np' = length $ vaildPoints p its
                    in ((x,y) `isOn` p) || ((np' + length p') `mod` 2 == 1)

pair' :: [a] -> [(a,a)]
pair' (a:as) = if length as == 1 then [(a,head as)] else (a, head as) : pair' as
pair' [] = []

pair :: [a] -> [(a,a)]
pair l = pair' l ++ [(last l, head l)]

single :: [(a,a)] -> [a]
single = map fst


vaildPoints :: Path -> [Point] -> [Point]
vaildPoints p d = let is = concatMap (`elemIndices` p) d
                  in mapMaybe (\x -> let lp = length p
                    in let (x', y') = x
                    in let iss' = tail $ foldl (\acc c -> if last (last acc) + 1 == c then init acc ++ [last acc ++ [c]] else init acc ++ [last acc] ++ [[c]]) [[last is]] is
                    in let iss = (if (length iss' > 1) && (head (head iss') == 0 && last (last iss') == length p -1) then drop 1 $ tail iss' ++ [last iss' ++ head iss'] else iss')
                    in if length (filter (\ l -> if length l == 1
                            then let i = head l
                            in  (y' - snd (p !! cAdd lp i 1)) * (y' - snd (p !! cAdd lp i (-1))) < 0
                            else let (i,j) = (head l, last l)
                            in (y' - snd (p !! cAdd lp i 1)) * (y' - snd (p !! cAdd lp j (-1))) < 0
                                ) iss) `mod` 2 == 1 then Just x else Nothing) d

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

gi0 :: [Path]
gi0 = squares $ map (square 160) (randomize seed rfactor [(190,190), (300,170), (410,200), (500,190),(610,100),(590,100),(580,210),(570,300),(560,401),(550,502)])

gi1 :: [Path]
gi1 = squares $ map (square 149) (randomize seed rfactor [(100, 100), (200,100), (300,100), (400,100), (500,100), (500,200), (500,300),(500,400),(500,500)])

gi2 :: Path
gi2 = []

ni0 :: Path
ni0 = []

ni1 :: Path
ni1 = []

--todo