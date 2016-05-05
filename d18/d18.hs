import Data.Matrix

instance (Num a1, Num a2) => Num (a1, a2) where
  (+) (a11, a12) (a21, a22) = (a11+a21, a12+a22)

choose :: a -> a -> Bool -> a
choose v _ True = v
choose _ v False = v

parseLines :: [String] -> Matrix Bool
parseLines lst = fromLists lst'
  where rep = [replicate (length $ head lst) '.']
        lst' = map (\l -> map (=='#') ("." ++ l ++ ".")) (rep ++ lst ++ rep)

activateCorners :: Matrix Bool -> Matrix Bool
activateCorners m = foldl (flip $ setElem True) m [ (x,y) | x <- [2,r-1], y <- [2,c-1] ]
  where (r, c) = (nrows m, ncols m)

step :: ((Int, Int) -> Int -> Bool -> Bool) -> Matrix Bool -> Matrix Bool
step func mat = let
    coords = [ (x, y) | x <- [-1,0,1], y <- [-1,0,1], x/=0 || y/=0 ]
    (r, c) = (nrows mat, ncols mat)
    ngb :: (Int, Int) -> Int
    ngb pos@(x, y)
      | x == 1 || x == r || y == 1 || y == c = 0
      | otherwise = foldl (\t d -> choose (t+1) t $ mat ! (pos+d)) 0 coords
    func' :: (Int, Int) -> Bool
    func' pos = func pos (ngb pos) (mat ! pos)
  in matrix r c func'

countLights :: Matrix Bool -> Int
countLights = foldl (\c -> choose (c+1) c) 0

main :: IO ()
main = do
  input <- readFile "d18_input.txt" >>= return . lines
  let mat = parseLines input
  let mat2 = activateCorners mat
  let isCorner pos = pos `elem` [ (x, y) | x <- [2, nrows mat - 1], y <- [2, ncols mat - 1]]
  let gameOfLife1 = iterate (step (\_ n v -> n==3 || (v && n==2))) mat
  let gameOfLife2 = iterate (step (\pos n v -> isCorner pos || n==3 || (v && n==2))) mat2

  print (countLights $ gameOfLife1 !! 100, countLights $ gameOfLife2 !! 100)
