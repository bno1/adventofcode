import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Char8 (pack)
import Data.ByteString.Builder
import Data.Bits
import Data.Binary

makeMask :: Int -> Word32
makeMask count = shift (shift 1 count - 1) (32 - count)

findHash :: Int -> String -> Int
findHash count str = let
    mask = toStrict . toLazyByteString . word32BE $ makeMask count
    bstr = pack str
    func nounce = if BS.zipWith (.&.) (MD5.hash candidate) mask == replicate 4 0
      then nounce
      else func (nounce + 1)
      where candidate = BS.concat [bstr, pack (show nounce)]
  in
    func 0

main :: IO ()
main = do
  input <- readFile "d4_input.txt"
  print (findHash 20 input, findHash 24 input)
