import Data.Bits
import Data.Tree
import Data.List (nub, nubBy, sort, unfoldr, transpose)
import qualified Data.Bifunctor
import Data.Tuple (swap)
import Control.Monad (join, forever)
import Control.Arrow ((***))
import Text.Read (readMaybe)
import GHC.IO.Handle (hSetEncoding)
import System.IO (stdout, utf8, stdin)
import GHC.IO.Encoding (utf8)
import System.Process (system)

main :: IO ()
main = do
    system "chcp 65001"
    hSetEncoding stdout utf8
    forever io

io :: IO ()
io = do
    putStr "enter seed: "
    seed <- readint
    putStr "enter rate: "
    d <- readint
    putStrLn (generateMatrix seed $ generateGraph  (0,0) (buildTree d seed)) >> io

readint :: IO Int
readint = do
    str <- getLine
    case readMaybe str ::Maybe Int of
        Just i -> return i
        nothing -> putStrLn "int. : " >> readint


type Graph a = [Edge a]
type Edge a = ((Point, Point), a)
type Point = (Int, Int)

xorshift32 :: (Num a, Bits a) => a -> a
xorshift32 seed =
    let l13seed = seed .^. (seed .<<. 13) in
    let r17seed = l13seed .^. (l13seed .>>. 17) in
    let l5seed = r17seed .^. (r17seed .<<. 5) in
        l5seed .&. 0xFFFFFFFF

xorshift32inf :: Int -> [Int]
xorshift32inf = iterate xorshift32

generateGraph' :: Point -> Tree Int -> Graph Int
generateGraph' p (Node r s) =
        let edges = generate r p
        in edges ++ concat (generateGraph' <$> map (snd .fst) edges <*> s)
    where
        generate i (a,b)
            | i == 0 = [(((a,b), (a+1,b)), 2)]
            | i == 1 = [(((a,b), (a,b+1)), 1)]
            | i == 2 = [(((a,b), (a+1,b+1)), 3)]
            | i == 3 = [(((a,b), (a+1,b-1)), 4)]
            | i == 4 = [(((a,b), (a+1,b+1)), 5), (((a,b+1), (a+1,b)), 5)]


buildNode :: Int -> Int -> Int -> (Int, [Int])
buildNode d seed depth =
    let rand = drop 30 $ xorshift32inf seed
    in let rand4 = map (`mod` 5) rand
    in if (rand !! depth) `mod` d < depth
        then (rand4 !! (depth * 5 + 1), [])
        else (rand4 !! (depth * 5 + 2), [
            depth * 5 + 1, depth* 5  +2 , depth* 5  +3, depth* 5 +4
        ])

buildTree :: Int ->  Int -> Tree Int
buildTree d seed = unfoldTree (buildNode d seed) 0

generateGraph :: Point -> Tree Int -> Graph Int
generateGraph p (Node r s) =
    let graph = nubBy (\x y -> fst x == fst y || swap (fst x) == fst y) (generateGraph' p (Node r s))
    in
        let mini = foldl (\e ((a,b), (c,d))-> minimum [b,c,d,e]) 0 (map fst graph)
    in map (Data.Bifunctor.first $ join (***) (Data.Bifunctor.second (\x -> x - mini))) graph

setArrow :: Int -> Int -> String
setArrow seed x =
            let r =  xorshift32inf seed !! 12
            in let nodes = nodeRepresentation x
            in nodes !! (r `mod` length nodes)

setSymbol :: Int ->String
setSymbol seed =
            let r =  xorshift32inf seed !! 7
            in let vert = vertexRepresentation
            in vert !! (r `mod` length vert)


nodeRepresentation :: Int -> [String]
nodeRepresentation i  = map (map (\x -> " " ++ [x] ++ " ")) ["→⇉⇢⭇⭋⬷←⇇⇄⇠↪↩", "↑⇈⇡⇣↟↡↓⇊⇵", "↘⤡⤣⤥", "↙⤢⤤⤦", "⤧⤨⤩⤪⤭⤮⤯⤰⤱⤲", "⥀⥁"] !! i
  -- ⤧⤨⤩⤪⤭⤮⤯⤰⤱⤲

vertexRepresentation :: [String]
vertexRepresentation = map (\x -> " " ++ [x] ++ " ") "01ABCEFGHIJKRSTℂℤℚℕℝ*"


generateMatrix :: Int -> Graph Int -> String
generateMatrix seed graph = let g = map (Data.Bifunctor.first $ join (***) (join (***) (*2))) graph
                in let maxx = foldl (\e ((a,b), (c,d))-> maximum [a,c,e]) 0 (map fst g) +1
                in let maxy = foldl (\e ((a,b), (c,d))-> maximum [b,d,e]) 0 (map fst g) +1
                in let entries0 = concat [[((a,b), 0) | a <- take maxx [0,1..]] | b <-  take maxy [0,1..]]
                in let entries1 = concatMap (\((p1, p2), i) -> [(p1, 1), (p2, 1), (((fst p1 +fst p2) `div` 2, (snd p1 + snd p2) `div` 2), i)]) g
                in let entries = map snd . sort $ nubBy (\x y-> fst x == fst y) (entries1 ++ entries0)
                in let symbols = zipWith3 (\i a s -> if a == 0
                                                    then "   "
                                                    else if i
                                                        then setArrow s (a-1)
                                                        else setSymbol s) (concat [[ odd x || odd y  | x <- take maxy [0,1..]] | y <- take maxx [0,1..]]) entries (drop 30 $ xorshift32inf seed)
                in unlines $ map concat ((takeWhile (not . null) . unfoldr (Just . splitAt maxy)) symbols)
