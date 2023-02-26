import System.IO  
import Text.Printf

-- 1 = win  , 0 = draw , -1 = loss
gameResult :: Char ->  Char  -> Int
gameResult a b
  | x == y = 0
  | y == 'R' && x == 'S' = 1
  | y == 'S' && x == 'P' = 1
  | y == 'P' && x == 'R' = 1
  | otherwise = -1
  where x = normalizeToken a
        y = normalizeToken b

normalizeToken :: Char -> Char
normalizeToken x
  | x == 'A' || x == 'X' = 'R'
  | x == 'B' || x == 'Y' = 'P'
  | x == 'C' || x == 'Z' = 'S'
  | x == 'R' || x == 'P' || x == 'S' = x -- already normalized...
  | otherwise = error (printf"Not a known token for Rockpapersiscorrs=%s"
                      x) 
-- Calculate the game score based on the following scoring rules:
-- Score of shape selected (1 for Rock, 2 for Paper, and 3 for Scissors)
-- + outcome of round: outcome of the round (0 if you lost, 3 if the round
-- was a draw, and 6 if you won)
gameScore :: Char -> Char -> Int
gameScore x y = cardScore + outcomeScore
  where cardScore = choiceScore (normalizeToken y)
        outcomeScore = scoreForResult( gameResult x y )

-- Convert game result (-1,0,1) to scoring rules (0,3,6)
scoreForResult :: Int -> Int
scoreForResult x
  | x == -1 = 0
  | x == 0 = 3
  | x == 1 = 6
  | otherwise = error "WTF"

choiceScore :: Char -> Int
choiceScore x
  | x == 'R' = 1
  | x == 'P' = 2
  | x == 'S' = 3
  | otherwise = error (printf "Not known choice %c" x)

-- For a pair of Chars, return the Char to play to get the desired outcome.
-- X = Loose, Y = draw, Z = win
getPart2Hand :: Char -> Char -> Char
getPart2Hand x y
  | y == 'X' = getLosingForHand x
  | y == 'Y' = normalizeToken x
  | y == 'Z' = getWinningForHand x
  | otherwise = error "Trololo"

getLosingForHand x
  | n == 'R' = 'S'
  | n == 'S' = 'P'
  | n == 'P' = 'R'
  where n = normalizeToken x

getWinningForHand x
  | n == 'S' = 'R'
  | n == 'P' = 'S'
  | n == 'R' = 'P'
  where n = normalizeToken x

main = do
    contents <- readFile "input.txt" 
    --let content = [splitAt 1 x | x <- (lines contents)]
    let content = [(x!!0, x!!2) | x <- (lines contents)]
    let winlose = [ gameResult (fst x) (snd x) | x <- content ]
    let scores =[ gameScore (fst x) (snd x) | x <- content ]
    let totalScore = sum scores
    printf "Total score = %d\n" totalScore

    let part2hands = [ ( (fst x) , (getPart2Hand (fst x) (snd x))) | x <- content ]
    let scoresPart2 =[ gameScore (fst x) (snd x) | x <- part2hands ] 
    printf "Part2 score = %d\n" (sum scoresPart2)
