import System.IO  
import Data.List
import Data.Maybe
import Text.Printf

pockets :: [Char] -> ([Char],[Char])
pockets xs = splitAt  splitPoint  xs
  where splitPoint = (length xs) `div` 2

pocketA :: [Char] -> [Char]
pocketA t = fst (pockets t)
pocketB :: [Char] -> [Char]
pocketB t = snd (pockets t)


scoreForLetter :: Char -> Int
scoreForLetter n = ( fromJust (elemIndex n letters) ) + 1
  where letters = ['a'..'z'] ++ ['A'..'Z']


main = do
    contents <- readFile "input.txt" 
    --let content = [splitAt 1 x | x <- (lines contents)]
    let content =  (lines contents)
    let intersections =  [ nub (intersect (pocketA x) (pocketB x)) |x  <- content ]
    let prioritySum = sum [ scoreForLetter l | x <- intersections , l <- x] 
    printf "Priority sum is %d\n" prioritySum
