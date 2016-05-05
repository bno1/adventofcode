import Data.Aeson
import Data.Scientific
import Data.ByteString.Lazy as B (readFile)
import Data.HashMap.Strict (elems)
import Data.Text (pack)

parseTree :: (Value -> a -> a) -> a -> Value -> a
parseTree f v node@(Object obj) = foldl (parseTree f) (f node v) obj
parseTree f v node@(Array arr) = foldl (parseTree f) (f node v) arr
parseTree f v node@(String _) = f node v
parseTree f v node@(Number _) = f node v
parseTree f v node@(Bool _) = f node v
parseTree f v node@Null = f node v

parseTree' :: (Value -> a -> a) -> a -> Value -> a
parseTree' f v node@(Object obj)
  | (String $ pack "red") `elem` elems obj = v
  | otherwise = foldl (parseTree' f) (f node v) obj
parseTree' f v node@(Array arr) = foldl (parseTree' f) (f node v) arr
parseTree' f v node@(String _) = f node v
parseTree' f v node@(Number _) = f node v
parseTree' f v node@(Bool _) = f node v
parseTree' f v node@Null = f node v

func :: Value -> Scientific -> Scientific
func (Number n) = (+n)
func _ = id

main :: IO ()
main = do
  input <- B.readFile "d12_input.txt"
  let result = decode input :: Maybe Value
  case result of
    Nothing -> print "error"
    Just j -> print (parseTree func 0 j, parseTree' func 0 j)
