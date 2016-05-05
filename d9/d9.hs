import Data.Matrix
import Data.List
import Data.Maybe

parseLines :: [String] -> ([String], [(Int, Int, Int)])
parseLines = foldl (\(cities, distances) x ->
  case words x of
    [city1, "to", city2, "=", dist] -> let
        cities' = cities `union` [city1, city2]
        ind1 = fromJust $ elemIndex city1 cities'
        ind2 = fromJust $ elemIndex city2 cities'
        dist' = read dist
      in (cities', (ind1, ind2, dist'):(ind2, ind1, dist'):distances)
    _ -> (cities, distances)
  ) ([], [])

toMatrix :: Int -> [(Int, Int, Int)] -> Matrix Int
toMatrix size = foldl (\m (l, c, d) -> setElem d (l+1, c+1) m) (zero size size)

computeDistance :: Matrix Int -> [Int] -> Int
computeDistance m (c1:c2:c) = getElem c1 c2 m + computeDistance m (c2:c)
computeDistance _ _ = 0

findMinPath :: Matrix Int -> Int
findMinPath m = foldl (\d -> min d . computeDistance m) (maxBound :: Int) $ permutations [1..(nrows m)]

findMaxPath :: Matrix Int -> Int
findMaxPath m = foldl (\d -> max d . computeDistance m) 0 $ permutations [1..(nrows m)]

main :: IO ()
main = do
  input <- readFile "d9_input.txt" >>= return . lines
  let (cities, distances) = parseLines input
  let m = toMatrix (length cities) distances
  print m
  print (findMinPath m, findMaxPath m)
