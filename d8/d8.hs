escapeLength :: Char -> Int
escapeLength 'x' = 1
escapeLength _ = -1

bool2Int :: Bool -> Int
bool2Int True = 1
bool2Int False = 0

process :: String -> (Int, Int, Int)
process str = let
    helper :: (Bool, Int, Int, Int, Int) -> String -> (Int, Int, Int)
    helper (_, _, totalEncoded, totalChars, memoryChars) [] = (totalEncoded, totalChars, memoryChars)
    helper (inString, escapeCount, totalEncoded, totalChars, memoryChars) (x:xs) =
      helper (
        inString',
        if inString then
          if x == '\\' && escapeCount == -1 then -2
          else
            if escapeCount == -2 then escapeLength x
            else max (-1) (escapeCount - 1)
        else -1,
        totalEncoded + atChar + bool2Int (x == '\\' || x == '\"') + bool2Int (inString /= inString'),
        totalChars + atChar,
        memoryChars + bool2Int (inString && inString' && escapeCount == -1)
      ) xs
      where inString' = inString /= (escapeCount == -1 && x == '\"')
            atChar = bool2Int $ inString || inString'
  in
    helper (False, -1, 0, 0, 0) str

main :: IO ()
main = do
  content <- readFile "d8_input.txt"
  let (te, t, m) = process content
  print (te, t, m)
  print (t - m, te - t)
