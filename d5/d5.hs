import System.IO
import Data.List

bannedSequences :: [String]
isVowel :: Char -> Bool
bannedSequences = ["ab", "cd", "pq", "xy"]
isVowel c = c `elem` "aeiou"

niceString :: String -> Bool
niceString str = let
  helper :: (Int, Char, Bool) -> String -> Bool
  helper (vowels, _, gotDoubleLetter) [] = vowels >= 3 && gotDoubleLetter
  helper (vowels, prev, gotDoubleLetter) (x:xs) =
    [prev, x] `notElem` bannedSequences && helper (
    if isVowel x then vowels + 1 else vowels,
    x,
    gotDoubleLetter || x == prev
    ) xs
  in
  helper (0, '\NUL', False) str

niceString2 :: String -> Bool
niceString2 str = let
  helper :: ((Char, Char), Bool, Bool) -> String -> Bool
  helper (_, True, True) _ = True
  helper _ [] = False
  helper ((a, b), gotDoublePair, gotSeparatedPair) (x:xs) = helper (
    (b, x),
    gotDoublePair || isInfixOf [b, x] xs,
    gotSeparatedPair || a == x
    ) xs
  in
  helper (('\NUL', '\NUL'), False, False) str

looper :: Handle -> IO (Int, Int)
looper handle = let
  helper total1 total2 = hIsEOF handle >>= \eof ->
    if eof then return (total1, total2)
    else hGetLine handle >>= \line ->
    let
    newTotal1 = if niceString  line then total1 + 1 else total1
    newTotal2 = if niceString2 line then total2 + 1 else total2
    in helper newTotal1 newTotal2
  in
  helper 0 0

main :: IO ()
main = openFile "d5_input.txt" ReadMode >>= \handle ->
  looper handle >>=
  print >>
  hClose handle
