import System.IO  
import Data.Maybe
import Data.List
import Text.Printf

-- parse into a tuple of tuples.
-- can be seen as ( (start,end) , ( start , end) )
parsePairs :: [Char] -> ( (Int,Int), (Int,Int) )
parsePairs x = (pair1,pair2)
  where strPairs = splitStringOn ',' x
        pair1Str = splitStringOn '-' (fst strPairs)
        pair2Str = splitStringOn '-' (snd strPairs)
        pair1 = ( (read (fst pair1Str) :: Int) , (read (snd pair1Str) :: Int))
        pair2 = ( (read (fst pair2Str) :: Int),(read (snd pair2Str) :: Int) )
-- test if x is contained in y or vice versa
isFullyContained :: (Int,Int) -> (Int,Int) -> Bool
isFullyContained (a,b) (c,d)
  | a >= c && b <= d = True
  | c >= a && d <= b = True
  | otherwise = False

-- Split string on character. exclude character from result
splitStringOn :: Char -> [Char] -> ([Char],[Char])
splitStringOn c xs = ( fst splitted, part2)
  where indexFound = fromJust (elemIndex c xs )
        splitted = splitAt indexFound xs
        part2 = tail (snd splitted)

overlap :: (Int,Int) -> (Int,Int) -> Bool
overlap (a,b) (c,d) = length (intersect [a..b] [c..d]) /= 0

main = do
    contents <- readFile "input.txt" 
    --let content = [splitAt 1 x | x <- (lines contents)]
    let content =  (lines contents)
    let parsedPairs = [parsePairs x | x <-  content ]
    let containedStatus =  [ isFullyContained (fst x) (snd x) | x <-parsedPairs ]
    let containedNum = length ((filter (True==) containedStatus))
    printf "Fully contained pairs count %d\n" containedNum

    printf "Overlapping pairs %d\n"  (length (filter (True==) [ overlap (fst x) (snd x) | x <- parsedPairs ]))
