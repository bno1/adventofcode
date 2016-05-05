import Data.List
import Data.Char (ord, chr)

letterSet :: Char -> String
letterSet c = [c..'z'] \\ "iol"

nextChar :: Char -> Char
nextChar = chr . (+1) . ord

combinationsBase :: Int -> [String]
combinationsBase 0 = [""]
combinationsBase n = [ c:comb | c <- letterSet 'a', comb <- combinationsBase (n-1) ]

combinations :: String -> [String]
combinations "" = [""]
combinations ('a':xs) = [ c:comb | c <- letterSet 'a', comb <- combinations xs]
combinations (x:xs) = [ x:comb | comb <- combinations xs ] ++
                      [ c:comb | c <- letterSet (nextChar x), comb <- combinationsBase (length xs) ]

repair :: String -> String
repair [] = []
repair (x:xs)
  | x `elem` "iol" = nextChar x : replicate (length xs) 'a'
  | otherwise = x : repair xs

check :: String -> Bool
check str = let
    helper :: (Int, Int, (Int, Bool)) -> String -> Bool
    helper (_, 2, (2, _)) _ = True
    helper _ [] = False
    helper (prev, got3, (paircnt, b)) (x:xs) = let ci = ord x in
      helper (ci,
              if got3 /= 2 then (if ci-1 == prev then got3+1 else 0) else 2,
              if paircnt /= 2 && ci == prev && not b then (paircnt+1, True) else (paircnt, False)
        ) xs
  in helper (0, 0, (0, False)) str

passwords :: String -> [String]
passwords = filter check . combinations . repair

main :: IO ()
main = do
  input <- readFile "d11_input.txt"
  print . take 2 $ passwords input
