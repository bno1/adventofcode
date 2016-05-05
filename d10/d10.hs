import Data.List

process :: String -> String
process = concatMap (\l -> show (length l) ++ [head l]) . group

main :: IO ()
main = do
  input <- readFile "d10_input.txt"
  print (length $ iterate process input !! 40, length $ iterate process input !! 50)
