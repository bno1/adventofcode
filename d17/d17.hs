compute :: Int -> [Int] -> [[Int]]
compute space containers
  | space == 0 = [[]]
  | space < 0 = []
  | null containers = []
  | otherwise = foldl (\t (i, c) ->
      if space < c then t
      else t ++ map (c:) (compute (space - c) (drop i containers)))
    [] . zip [1..] $ containers

main :: IO ()
main = do
  containers <- readFile "d17_input.txt" >>= return . map read . lines
  let combs = compute 150 containers
  let min_len = minimum $ map length combs
  let min_combs = filter ((==min_len) . length) combs
  print (length combs, length min_combs)
