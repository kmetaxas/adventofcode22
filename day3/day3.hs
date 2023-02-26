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

threewayintersect :: [Char] -> [Char] -> [Char] -> [Char]
threewayintersect as bs cs = nub (intersect ( nub (intersect as bs) ) cs )

-- TODO This is ugly. I'm sure there must be a haskell/functional way
iterBadgeGroups [] = []
iterBadgeGroups xs = (threewayintersect str1 str2 str3): iterBadgeGroups rest
  where workGroup = take 3 xs
        str1 = workGroup!!0
        str2 = workGroup!!1
        str3 = workGroup!!2
        rest = snd ( splitAt 3 xs )

main = do
    contents <- readFile "input.txt" 
    --let content = [splitAt 1 x | x <- (lines contents)]
    let content =  (lines contents)
    let intersections =  [ nub (intersect (pocketA x) (pocketB x)) |x  <- content ]
    let prioritySum = sum [ scoreForLetter l | x <- intersections , l <- x] 
    printf "Priority sum is %d\n" prioritySum

    let badgePriorities = [ scoreForLetter l |x <- (iterBadgeGroups content) , l<-x]
    printf "Badge priority sum is %d\n" (sum badgePriorities)
