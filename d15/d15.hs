import qualified Data.Matrix as M

data Ingredient = Ingredient {capacity :: Int,
                              durability :: Int,
                              flavor :: Int,
                              texture :: Int,
                              calories :: Int} deriving (Show)

parseLine :: String -> Ingredient
parseLine str = case words . filter (/=',') $ str of
    [_, _, cap, _, dur, _, flv, _, tex, _, cal] ->
      Ingredient (read cap) (read dur) (read flv) (read tex) (read cal)
    _ -> Ingredient 0 0 0 0 0

toMatrix :: [Ingredient] -> M.Matrix Int
toMatrix = let
    toList :: Ingredient -> [Int]
    toList i = map ($ i) [capacity, durability, flavor, texture, calories]
  in M.fromLists . map toList

combinations :: Int -> Int -> [[Int]]
combinations 0 k = [replicate k 0]
combinations n 1 = [[n]]
combinations n k = [ c:comb | c <- [1..n], comb <- combinations (n-c) (k-1)]

popLastCol :: M.Matrix Int -> M.Matrix Int
popLastCol mat = M.submatrix 1 (M.nrows mat) 1 (M.ncols mat - 1) mat

findMax :: M.Matrix Int -> [[Int]] -> Int
findMax mat = maximum . map (foldl ((.max 0) . (*)) 1 . flip M.multStd mat' . M.fromList 1 (M.nrows mat'))
  where mat' = popLastCol mat

findMax2 :: M.Matrix Int -> [[Int]] -> Int
findMax2 mat = maximum . map (test . flip M.multStd mat . M.fromList 1 (M.nrows mat))
  where test rez = if (rez M.! (1, 5)) == 500 then foldl ((.max 0) . (*)) 1 (popLastCol rez) else 0

main :: IO ()
main = do
  input <- readFile "d15_input.txt" >>= return . lines
  let matrix = toMatrix (map parseLine input)
  let comb = combinations 100 (M.nrows matrix)
  print (findMax matrix comb, findMax2 matrix comb)
