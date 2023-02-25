import System.IO  
import Data.List

splitPerElf [] = []
splitPerElf xs = elfSum: (splitPerElf rest)
  where sub = takeWhile (""/=) xs 
        offset = length sub +1
        elfSum = sum [ read x :: Integer | x <- sub ]
        rest = snd ( splitAt offset xs )

richestElf xs = maximum xs

main = do
    contents <- readFile "input.txt" 
    let food = (lines contents)
    print (richestElf (splitPerElf food))

    
