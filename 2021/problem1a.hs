import System.IO
import Control.Monad

calc (previous, total) current = if previous < current then (current, total + 1) else (current, total)

readInput :: String -> [Int]
readInput contents = map readInt (lines contents)
    where readInt = read :: String -> Int

main :: IO ()
main = do
    contents <- readFile "input-1"
    print $ snd (foldl calc (-1, -1) (readInput contents))
