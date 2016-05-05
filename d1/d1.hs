import Data.Maybe

toInt :: Char -> Int
toInt '(' = 1
toInt ')' = -1
toInt _ = 0

step :: (Int, Int, Maybe Int) -> Char -> (Int, Int, Maybe Int)
step (level, index, basementIndex) c = (
    nextLevel,
    nextIndex,
    if nextLevel == -1 then Just $ fromMaybe nextIndex basementIndex else basementIndex
  )
  where nextLevel = level + toInt c
        nextIndex = index + 1

main :: IO ()
main = do
  input <- readFile "d1_input.txt"
  let (level, _, bi) = foldl step (0, 0, Nothing) input
  print (level, bi)
