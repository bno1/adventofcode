import Data.Array.Unboxed
import Data.Array.ST
import Control.Monad
import Data.List

looper :: (Monad m) => [b] -> (b -> m Bool) -> m ()
looper [] _ = return ()
looper (x:xs) f  = do
  v <- f x
  when v $ looper xs f

primeFactors :: Int -> [Int]
primeFactors n = [ i | i <- [2..n], sieve ! i ]
  where sieve =
          runSTUArray $ do
            array <- newArray (2, n) True
            forM_ [2..n] (\i -> forM_ [2*i,3*i..n] (\j -> writeArray array j False))
            return array

pfSieve1 :: Int -> UArray Int Int
pfSieve1 ns = let
    n = round . sqrt . fromIntegral $ ns
    pf = primeFactors n
    ppow p = takeWhile (\(pk, s1) -> pk <= ns) . scanl (\(pk, s1) _ -> (pk*p, s1+pk*p)) (p, p+1) $ [1..]
    helper :: Int -> (Int, Int) -> (Int, Int)
    helper p (a, b)
      | a `mod` p == 0 = helper p (a `div` p, b*p)
      | otherwise = (a, b)

    getCop m = case find (\pi -> m `mod` pi == 0) pf of
      Nothing -> (-1, -1)
      Just p -> helper p (m, 1)

  in runSTUArray $ do
    array <- newArray (1, ns) (-1)

    writeArray array 1 1
    forM_ pf (\p -> forM_ (ppow p) $ \(pk, s1) -> writeArray array pk s1)

    looper [1..ns] $ \i -> do
      v <- readArray array i
      if v == -1 then
        case getCop i of
          (-1, -1) -> writeArray array i (i+1) >> return True
          (a, b) -> do
            va <- readArray array a
            vb <- readArray array b
            writeArray array i (va * vb)
            return ((va * vb) < ns)
      else return True
    return array

pfSieve2 :: Int -> UArray Int Int
pfSieve2 n = runSTUArray $ do
  array <- newArray (1, n) 1

  forM_ [2..n] $ \i -> let k = min (i*50) n in
    forM_ [i, 2*i ..k] (\j -> do
      v <- readArray array j
      writeArray array j (v + i))

  return array

main :: IO ()
main = do
  input <- readFile "d20_input.txt" >>= return . read
  let n1 = input `div` 10
  let n2 = input `div` 11
  let sieve1 = pfSieve1 n1
  let sieve2 = pfSieve2 n2
  print . find fst . map (\i -> (sieve1 ! i >= n1, i)) $ [1..n1]
  print . find fst . map (\i -> (sieve2 ! i >= n2, i)) $ [1..n2]
