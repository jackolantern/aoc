import Data.List
import System.IO
import Control.Monad

windows n xs = Data.List.transpose (take n (tails xs))
calc (previous, total) current = if (sum previous) < (sum current) then (current, total + 1) else (current, total)

readInput :: String -> [[Int]]
readInput contents = windows 3 (map readInt (lines contents))
    where readInt = read :: String -> Int

main :: IO ()
main = do
    contents <- readFile "input-1"
    print $ snd (foldl calc ([-1, -1, -1], -1) (readInput contents))
