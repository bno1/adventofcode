import Data.List

type Arrangement = [[Int]]

groups :: [Int] -> Int -> [Arrangement]
groups lst k = if sum lst `mod` k /= 0 then [] else
  let
    gs = sum lst `div` k
    subseq = filter ((==gs) . sum) . subsequences

    helper :: [Int] -> Int -> [Arrangement]
    helper lst 0 = [[]]
    helper lst step = [ p2 ++ [p1] | p1 <- subseq lst, p2 <- helper (lst \\ p1) (step - 1) ]
  in helper lst k

qed :: Arrangement -> Int
qed = product . head

getBest :: [Int] -> Int -> Arrangement
getBest lst k = minimumBy (\a1 a2 -> compare (qed a1) (qed a2)) mins
  where arrangements = groups lst k
        mingifts = length . head . head $ arrangements
        mins = takeWhile ((==mingifts) . length . head) arrangements

main :: IO ()
main = do
  input <- readFile "d24_input.txt" >>= return . sort . map read . lines
  print (qed $ getBest input 3, qed $ getBest input 4)
