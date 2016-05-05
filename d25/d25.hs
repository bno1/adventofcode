import Data.List
import Data.Maybe
import Data.Char

startVal = 20151125
base = 252533
divisor = 33554393

indexOf :: (Integer, Integer) -> Integer
indexOf (r, c) = (diag * (diag - 1)) `div` 2 + c - 1
  where diag = r + c - 1

mexp :: Integer -> Integer -> Integer -> Integer -> Integer
mexp a 0 m acc = acc
mexp a b m acc = if b `mod` 2 == 1
                 then mexp (a * a `mod` m) (b `div` 2) m (acc * a `mod` m)
                 else mexp (a * a `mod` m) (b `div` 2) m acc

main = do
  input <- readFile "d25_input.txt"
  let numbers = words . filter (\c -> isDigit c || isSpace c) $ input
  let pos = (read $ numbers !! 0, read $ numbers !! 1)
  print (mexp base (indexOf pos) divisor startVal)
