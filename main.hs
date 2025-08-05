{-# LANGUAGE ParallelListComp #-}
import System.Directory
import System.FilePath
import Data.List
import Data.Bifunctor
import Debug.Trace
import Data.Maybe (maybeToList, catMaybes, mapMaybe)
import Data.Bits
import Numeric (showHex)
import Data.Set (fromList)

type Point = (Float, Float)
type Path = [Point]
type Edge = (Point, Point)
type Square = (Point, Float)
---- ((a,b),r)

--     (a,b) ---(a+r, b)
--    |         |
--   |         |
---(a,b+r)---(a+r, b+r)

type Squares = [Square]

------------io-----------------------

main :: IO ()
main = sequence_ hangul

svg :: String -> [String] -> IO ()
svg s l  = do
    currentDir <- getCurrentDirectory
    let dir = currentDir </> "output" </> (s ++ ".svg")
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
unionPaths [] = []

-- unionPaths :: [Path] -> [Path]
-- unionPaths p= [foldl1 (\acc b -> if acc `isAttachable` b then unionPath acc b else acc) p, p `nub` (foldl1 (\acc b -> if acc `isAttachable` b then unionPath acc b else acc) p)]

fixedUnionPath :: [Path] -> [Path]
fixedUnionPath p = if fromList (unionPaths p) == fromList p then p else fixedUnionPath $ unionPaths p

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

square :: Float -> Point -> Square
square r (a, b)= ((a,b),r)

line :: Edge -> (Float -> Float)
line ((px, py), (qx, qy)) = if   (px-qx) /= 0
                            then
                                let a =   (py-qy) /   (px-qx) :: Float
                                in (\x -> a *  x - a *  px +  py)
                            else error $ show px ++ " = " ++ show qx

linerect :: Float -> Float -> Float -> Edge -> ([Square], Float)
linerect t r i ((px, py), (qx, qy)) =
                                    if sqrt ( (px - qx) ^ 2 +  (py - qy) ^ 2) /= 0 then
                                        let f = line ((px, py), (qx, qy))
                                        in let dx = ( i *  (qx - px) / sqrt ( (px - qx) ^ 2 +  (py - qy) ^ 2) ::Float)
                                        in let at = abs $ ( t *  (qx - px) / sqrt ( (px - qx) ^ 2 +  (py - qy) ^ 2) ::Float)
                                        in if dx == 0 then error "dense" else
                                            let n = round (abs ((qx - px) / dx) + 1)
                                                in let rest = (qx - px) / dx
                                                in (take n [ square r (a, f a) | a <- [px+at, px+at+dx..]], rest)
                                    else error $ show ( (px - qx) ^ 2) ++ " = " ++ show ( (py - qy) ^ 2)

linesrect0 :: Float -> Float -> [Edge] -> [Square]
linesrect0 = linesrect 0

linesrect :: Float -> Float -> Float -> [Edge] -> [Square]
linesrect t r i (e:es) = let (s, rest) = linerect t r i e
                        in s ++ linesrect rest r i es
linesrect _ _ _ [] = []

nbr :: [a] -> [(a,a)]
nbr a = zip a (drop 1 a)

pathrect :: Float -> Float -> Path -> [Square]
pathrect r i p = linesrect0 r i (nbr p)

translation :: Float -> Float -> Path -> Path
translation a b = map (bimap (+a) (+b))

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
randomize s i l = [ (x + b * i, y + b * i) | (x,y) <- l | b <- drop 2 $ xorshift32inf s]

randomizeSqr :: Int -> Float -> [Square] -> [Square]
randomizeSqr s i l = let p = map fst l
                    in let r = map snd l
                    in zip (randomize s i p) r

randomizeSqrr :: Int -> Float -> Float -> [Square] -> [Square]
randomizeSqrr s i j l = let p = map fst l
                    in let r = map snd l
                    in zip (randomize s i p) [ a * b * j | a <- r | b <- drop 2 $ xorshift32inf s]

--------------unicode---------------------

unicode :: Int -> String
unicode i = showHex (0xAC00 + i) ""


hangul :: [IO ()]
hangul = [svg ("uni"++ unicode s) $ map path x | s <- [0, 1..] | x <- comp]

comp :: [[Path]]
comp = (\a b c -> map (translation 50 100) a ++ map (translation 500 0) b ++ map (translation 300 500) c)
    <$>
        [
        g_0
        , gg_0
        , n_0
        , d_0
        , dd_0
        , r_0
        , m_0
        , b_0
        , bb_0
        , s_0
        , ss_0
        , _0
        , j_0
        , jj_0
        , c_0
        , k_0
        , t_0
        , p_0
        , h_0
        ]
        <*> [
        a_1
        , ae_1
        , ya_1
        , yae_1
        , eo_1
        , e_1
        , yeo_1
        , ye_1
        , o_1
        , wa_1
        , wae_1
        , oe_1
        , yo_1
        , u_1
        , weo_1
        , we_1
        , wi_1
        , yu_1
        , eu_1
        , yi_1
        , i_1
      ]
        <*> [
        []
        , g_2
        , gg_2
        , gs_2
        , n_2
        , nj_2
        , nh_2
        , d_2
        , l_2
        , lg_2
        , lm_2
        , lb_2
        , ls_2
        , lt_2
        , lp_2
        , lh_2
        , m_2
        , b_2
        , bs_2
        , s_2
        , ss_2
        , ng_2
        , j_2
        , c_2
        , k_2
        , t_2
        , p_2
        , h_2
        ]

--------------const----------------------------

-- test :: IO ()
-- test = svg "dddd" (map path $ map sqrtoPath [((10,10),55),((57,13),55),((104,15),55),((150,17),55),((194,16),55),((241,19),55),((287,21),55),((314,27),55),((322,17),55),((319,58),55),((315,98),55),((314,140),55),((310,179),55),((307,220),55),((305,262),55)])

rfactor :: Float
rfactor = 5

seed :: Int
seed = 6

radius :: Float
radius = 54

interval :: Float
interval = 44

g_0 :: [Path]
g_0 = squares $ randomizeSqr seed rfactor (pathrect radius interval [(10,10), (300,20), (320, 30), (300,300)])

gg_0 :: [Path]
gg_0 = squares $ randomizeSqr seed rfactor (pathrect radius interval [(10, 10), (200, 20), (160, 235)] ++ pathrect radius interval [(230, 30), (310, 20), (294, 235)])

n_0 :: [Path]
n_0 = squares $ randomizeSqr seed rfactor (pathrect radius interval [(40,10), (10,200), (20, 240), (40, 300) , (140, 250), (300,300)])

d_0 :: [Path]
d_0 = squares $ randomizeSqr seed rfactor (pathrect radius interval [(320, 40), (230, 50), (100, 5), (40,10), (10,150), (40, 240), (20, 300) , (120, 290), (300,300)])

dd_0 :: [Path]
dd_0 = squares (randomizeSqr seed rfactor (pathrect radius interval [(320, 40), (230, 50), (100, 5), (40,10), (10,150), (40, 240), (20, 300) , (120, 290), (300,300)]))
        ++ squares (randomizeSqr seed rfactor (pathrect radius interval [(150, 60), (145, 270)]))

r_0 :: [Path]
r_0 = squares $ randomizeSqr seed rfactor (pathrect radius interval [(10, 10), (300, 5), (289, 163), (1, 144), (25, 319), (300, 279)])

m_0 :: [Path]
m_0 = squares $ randomizeSqr seed rfactor (pathrect radius interval [])

b_0 :: [Path]
b_0 = squares $ randomizeSqr seed rfactor (pathrect radius interval [])

bb_0 :: [Path]
bb_0 = squares $ randomizeSqr seed rfactor (pathrect radius interval [])

s_0 :: [Path]
s_0 = squares $ randomizeSqr seed rfactor (pathrect radius interval [])

ss_0 :: [Path]
ss_0 = squares $ randomizeSqr seed rfactor (pathrect radius interval [])

_0 :: [Path]
_0 = squares $ randomizeSqr seed rfactor (pathrect radius interval [])

j_0 :: [Path]
j_0 = squares $ randomizeSqr seed rfactor (pathrect radius interval [])

jj_0 :: [Path]
jj_0 = squares $ randomizeSqr seed rfactor (pathrect radius interval [])

c_0 :: [Path]
c_0 = squares $ randomizeSqr seed rfactor (pathrect radius interval [])

k_0 :: [Path]
k_0 = squares $ randomizeSqr seed rfactor (pathrect radius interval [])

t_0 :: [Path]
t_0 = squares $ randomizeSqr seed rfactor (pathrect radius interval [])

p_0 :: [Path]
p_0 = squares $ randomizeSqr seed rfactor (pathrect radius interval [])

h_0 :: [Path]
h_0 = squares $ randomizeSqr seed rfactor (pathrect radius interval [])


a_1 :: [Path]
a_1 = squares (randomizeSqr seed rfactor (pathrect radius interval [(20, 30), (30, 25), (59, 300), (25, 500)])) ++ squares (randomizeSqr seed rfactor (pathrect radius interval [(30, 220), (70, 219), (90, 230)]))

ae_1 :: [Path]
ae_1 = squares (randomizeSqr seed rfactor (pathrect radius interval [(30,90),(275,100)]))

ya_1 :: [Path]
ya_1 = squares (randomizeSqr seed rfactor (pathrect radius interval [(30,90),(275,100)]))

yae_1 :: [Path]
yae_1 = squares (randomizeSqr seed rfactor (pathrect radius interval [(30,90),(275,100)]))

eo_1 :: [Path]
eo_1 = squares (randomizeSqr seed rfactor (pathrect radius interval [(30,90),(275,100)]))

e_1 :: [Path]
e_1 = squares (randomizeSqr seed rfactor (pathrect radius interval [(30,90),(275,100)]))

yeo_1 :: [Path]
yeo_1 = squares (randomizeSqr seed rfactor (pathrect radius interval [(30,90),(275,100)]))

ye_1 :: [Path]
ye_1 = squares (randomizeSqr seed rfactor (pathrect radius interval [(30,90),(275,100)]))

o_1 :: [Path]
o_1 = squares (randomizeSqr seed rfactor (pathrect radius interval [(30,90),(275,100)]))

wa_1 :: [Path]
wa_1 = squares (randomizeSqr seed rfactor (pathrect radius interval [(30,90),(275,100)]))

wae_1 :: [Path]
wae_1 = squares (randomizeSqr seed rfactor (pathrect radius interval [(30,90),(275,100)]))

oe_1 :: [Path]
oe_1 = squares (randomizeSqr seed rfactor (pathrect radius interval [(30,90),(275,100)]))

yo_1 :: [Path]
yo_1 = squares (randomizeSqr seed rfactor (pathrect radius interval [(30,90),(275,100)]))

u_1 :: [Path]
u_1 = squares (randomizeSqr seed rfactor (pathrect radius interval [(30,90),(275,100)]))

weo_1 :: [Path]
weo_1 = squares (randomizeSqr seed rfactor (pathrect radius interval [(30,90),(275,100)]))

we_1 :: [Path]
we_1 = squares (randomizeSqr seed rfactor (pathrect radius interval [(30,90),(275,100)]))

wi_1 :: [Path]
wi_1 = squares (randomizeSqr seed rfactor (pathrect radius interval [(30,90),(275,100)]))

yu_1 :: [Path]
yu_1 = squares (randomizeSqr seed rfactor (pathrect radius interval [(30,90),(275,100)]))

eu_1 :: [Path]
eu_1 = squares (randomizeSqr seed rfactor (pathrect radius interval [(30,90),(275,100)]))

yi_1 :: [Path]
yi_1 = squares (randomizeSqr seed rfactor (pathrect radius interval [(30,90),(275,100)]))

i_1 :: [Path]
i_1 = squares (randomizeSqr seed rfactor (pathrect radius interval [(30,90),(275,100)]))


g_2 :: [Path]
g_2 = squares (randomizeSqr seed rfactor (pathrect radius interval [(30,90),(275,100)]))

gg_2 :: [Path]
gg_2 = squares (randomizeSqr seed rfactor (pathrect radius interval [(30,90),(275,100)]))

gs_2 :: [Path]
gs_2 = squares (randomizeSqr seed rfactor (pathrect radius interval [(30,90),(275,100)]))

n_2 :: [Path]
n_2 = squares (randomizeSqr seed rfactor (pathrect radius interval [(30,90),(275,100)]))

nj_2 :: [Path]
nj_2 = squares (randomizeSqr seed rfactor (pathrect radius interval [(30,90),(275,100)]))

nh_2 :: [Path]
nh_2 = squares (randomizeSqr seed rfactor (pathrect radius interval [(30,90),(275,100)]))

d_2 :: [Path]
d_2 = squares (randomizeSqr seed rfactor (pathrect radius interval [(30,90),(275,100)]))

l_2 :: [Path]
l_2 = squares (randomizeSqr seed rfactor (pathrect radius interval [(30,90),(275,100)]))

lg_2 :: [Path]
lg_2 = squares (randomizeSqr seed rfactor (pathrect radius interval [(30,90),(275,100)]))

lm_2 :: [Path]
lm_2 = squares (randomizeSqr seed rfactor (pathrect radius interval [(30,90),(275,100)]))

lb_2 :: [Path]
lb_2 = squares (randomizeSqr seed rfactor (pathrect radius interval [(30,90),(275,100)]))

ls_2 :: [Path]
ls_2 = squares (randomizeSqr seed rfactor (pathrect radius interval [(30,90),(275,100)]))

lt_2 :: [Path]
lt_2 = squares (randomizeSqr seed rfactor (pathrect radius interval [(30,90),(275,100)]))

lp_2 :: [Path]
lp_2 = squares (randomizeSqr seed rfactor (pathrect radius interval [(30,90),(275,100)]))

lh_2 :: [Path]
lh_2 = squares (randomizeSqr seed rfactor (pathrect radius interval [(30,90),(275,100)]))

m_2 :: [Path]
m_2 = squares (randomizeSqr seed rfactor (pathrect radius interval [(30,90),(275,100)]))

b_2 :: [Path]
b_2 = squares (randomizeSqr seed rfactor (pathrect radius interval [(30,90),(275,100)]))

bs_2 :: [Path]
bs_2 = squares (randomizeSqr seed rfactor (pathrect radius interval [(30,90),(275,100)]))

s_2 :: [Path]
s_2 = squares (randomizeSqr seed rfactor (pathrect radius interval [(30,90),(275,100)]))

ss_2 :: [Path]
ss_2 = squares (randomizeSqr seed rfactor (pathrect radius interval [(30,90),(275,100)]))

ng_2 :: [Path]
ng_2 = squares (randomizeSqr seed rfactor (pathrect radius interval [(30,90),(275,100)]))

j_2 :: [Path]
j_2 = squares (randomizeSqr seed rfactor (pathrect radius interval [(30,90),(275,100)]))

c_2 :: [Path]
c_2 = squares (randomizeSqr seed rfactor (pathrect radius interval [(30,90),(275,100)]))

k_2 :: [Path]
k_2 = squares (randomizeSqr seed rfactor (pathrect radius interval [(30,90),(275,100)]))

t_2 :: [Path]
t_2 = squares (randomizeSqr seed rfactor (pathrect radius interval [(30,90),(275,100)]))

p_2 :: [Path]
p_2 = squares (randomizeSqr seed rfactor (pathrect radius interval [(30,90),(275,100)]))

h_2 :: [Path]
h_2 = squares (randomizeSqr seed rfactor (pathrect radius interval [(30,90),(275,100)]))
