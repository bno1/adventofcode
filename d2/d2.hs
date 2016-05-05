import Data.List.Split

area :: (Num a, Ord a) => [a] -> a
area [l, h, w] = sum lst * 2 + minimum lst where lst = [l*w, l*h, w*h]
area _ = 0

ribbon :: (Num a, Ord a) => [a] -> a
ribbon lst = (sum lst - maximum lst) * 2 + product lst

looper :: (Num a, Ord a, Read a) => [[a] -> a] -> [a] -> String -> [a]
looper funcs vals line =
  if length dim == 3 then
    zipWith (\func n -> n + func dim) funcs vals
  else
    vals
  where dim = map read $ splitOn "x" line

main :: IO ()
main = do
  input <- readFile "d2_input.txt"
  print $ foldl (looper [area, ribbon]) [0, 0] $ splitOn "\n" input
