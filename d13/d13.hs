import Data.Matrix
import Data.List
import Data.Maybe

parseLines :: [String] -> ([String], [((Int, Int), Int)])
parseLines = foldl (\(people, values) line ->
    case words line of
      [p1, "would", sign', h, "happiness", "units", "by", "sitting", "next", "to", p2'] ->
        let p2 = init p2' --discard '.'
            people' = union people [p1, p2]
            p1i = fromJust $ elemIndex p1 people'
            p2i = fromJust $ elemIndex p2 people'
            sign = if sign' == "lose" then -1 else 1
            delta = sign * read h
        in
          (people', ((p1i + 1, p2i + 1), delta):values)
      _ -> (people, values)
    ) ([], [])

toMatrix :: ([String], [((Int, Int), Int)]) -> Matrix Int
toMatrix (people, values) = foldl (\mat (pos, delta) ->
    setElem delta pos mat) (zero size size) values
  where size = length people

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs lst = (zip <*> tail) $ last lst : lst

happiness :: Matrix Int -> [Int] -> Int
happiness mat = foldl (\h (a, b) -> h + getElem a b mat + getElem b a mat)
  0 . pairs

findMin :: Matrix Int -> [Int] -> Int
findMin mat = foldl (\h (a, b) -> min h $ getElem a b mat + getElem b a mat)
  maxBound . pairs

findBest :: [String] -> Matrix Int -> (Int, [Int])
findBest people mat = maximumBy (\(h1, _) (h2, _) -> compare h1 h2) .
  map ((,) <$> happiness mat <*> id) $ permutations [1..(length people)]

main :: IO ()
main = do
  input <- readFile "d13_input.txt" >>= return . lines
  let t@(people, _) = parseLines input
  let mat = toMatrix t
  let (happ, order) = findBest people mat
  let t2@(people2, _) = ("Me":people, snd t)
  let mat2 = toMatrix t2
  let (happ2, order2) = findBest people2 mat2
  print (happ, happ2, happ - findMin mat order)
