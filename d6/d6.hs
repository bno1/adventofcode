import System.IO
import Data.Sequence
import Data.Array.IO
import Data.Int
import Data.Word
import Numeric (showIntAtBase)
import Data.Char (intToDigit)
import Data.Bits
import Data.List.Split
import Data.Foldable

type BitGridChunk = Word32
type IntGridType = Int32

--chunk size in bits
chunkSize :: Int
chunkSize = finiteBitSize (0 :: BitGridChunk)

--chunk mask used for % chunkSize
chunkMask :: Int
chunkMask = chunkSize - 1

--chunk shift used for / chunkSize
chunkShift :: Int
chunkShift = countTrailingZeros chunkSize

data IntGrid = IntGrid (IOUArray Int IntGridType) Int Int
data BitGrid = BitGrid (IOUArray Int BitGridChunk) Int Int
data Coord = Coord Int Int
data Command = Set Coord Coord Bool | Flip Coord Coord

class Grid a where
  createGrid :: Int -> Int -> IO a
  showGrid :: a -> IO String
  applyCommand :: a -> Command -> IO ()
  getCount :: a -> IO Int

instance Show Coord where
  show (Coord l c) = concat ["(", show l, ",", show c, ")"]

instance Show Command where
  show (Set c1 c2 t) = concat ["Set ", show c1, " ", show c2, " ", show t]
  show (Flip c1 c2)= concat ["Flip ", show c1, " ", show c2]

instance Grid BitGrid where
  createGrid width height = newArray (0, size - 1) 0 >>=
    return . \array -> BitGrid array width height
    where size = ceiling (fromIntegral (width * height) / fromIntegral chunkSize :: Float)

  showGrid (BitGrid array width height) = let
      showfunc = flip (showIntAtBase 2 intToDigit) ""
      helper :: String -> (Int, Int) -> IO String
      helper str (i, iend)
        | i <= iend = readArray array i >>=
          \val -> helper (str ++ showfunc val++ " ") (i+1, iend)
        | otherwise = return str
    in
      getBounds array >>= helper "" >>=
        return . \srt -> concat [show width, " ", show height, srt]

  applyCommand (BitGrid array width _) (Set (Coord l1 c1) (Coord l2 c2) val) = let
      l1ef = l1 * width
      l2ef = l2 * width
      helper l
        | l <= l2ef = setBits array (l + c1) (l + c2 + 1) val >> helper (l + width)
        | otherwise = return ()
    in
      helper l1ef

  applyCommand (BitGrid array width _) (Flip (Coord l1 c1) (Coord l2 c2)) = let
      l1ef = l1 * width
      l2ef = l2 * width
      helper l
        | l <= l2ef = flipBits array (l + c1) (l + c2 + 1) >> helper (l + width)
        | otherwise = return ()
    in
      helper l1ef

  getCount (BitGrid array _ _) = bitCount array

instance Grid IntGrid where
  createGrid width height = newArray (0, size - 1) 0 >>=
    return . \array -> IntGrid array width height
    where size = width * height

  showGrid grid@(IntGrid _ width height) = let
      showfunc = show
      start = Coord 0 0
      stop = Coord (height - 1) (width - 1)
    in
      gridFoldl grid start stop "" (\str val -> str ++ " " ++ (showfunc val))  >>= 
        return . \srt -> concat [show width, " ", show height, "\n", srt]

  applyCommand grid (Set coord1 coord2 val) =
      gridApply grid coord1 coord2 (if val then (+1) else (\x -> x-1))

  applyCommand grid (Flip coord1 coord2) =
      gridApply grid coord1 coord2 (+2)

  getCount grid@(IntGrid _ width height) = let
      start = Coord 0 0
      stop = Coord (height - 1) (width - 1)
    in
      gridFoldl grid start stop 0 (+) >>= return . fromIntegral

applyBits :: IOUArray Int BitGridChunk -> Int -> Int -> (BitGridChunk -> BitGridChunk -> BitGridChunk) -> IO ()
applyBits array i iend func
  | chunkSBit == 0 && (chunkEInd > chunkSInd) = do
    --set whole chunk
    oldChunk <- readArray array chunkSInd
    writeArray array chunkSInd (func oldChunk (complement 0))
    applyBits array (i + chunkSize) iend func
  | chunkSBit /= 0 && (chunkEInd > chunkSInd) = do
    let mask = shiftL (complement 0 :: BitGridChunk) chunkSBit
    oldChunk <- readArray array chunkSInd
    writeArray array chunkSInd (func oldChunk mask)
    applyBits array (i + chunkSize - chunkSBit) iend func
  | chunkEInd == chunkSInd && chunkEBit > chunkSBit = do
    let mask = shiftL (shiftL 1 (chunkEBit - chunkSBit) - 1) chunkSBit
    oldChunk <- readArray array chunkSInd
    writeArray array chunkSInd (func oldChunk mask)
  | otherwise = return ()
  where chunkSInd = shiftR i chunkShift
        chunkSBit = i .&. chunkMask
        chunkEInd = shiftR iend chunkShift
        chunkEBit = iend .&. chunkMask

setBits :: IOUArray Int BitGridChunk -> Int -> Int -> Bool -> IO ()
setBits array i iend val = applyBits array i iend (if val then (.|.) else (\c m -> c .&. complement m))

flipBits :: IOUArray Int BitGridChunk -> Int -> Int -> IO ()
flipBits array i iend = applyBits array i iend xor

bitCount :: IOUArray Int BitGridChunk -> IO Int
bitCount array = let
    helper :: Int -> (Int, Int) -> IO Int
    helper count (i, iend)
      | i <= iend = do val <- readArray array i; helper (count + popCount val) (i+1, iend)
      | otherwise = return count
  in
    getBounds array >>= helper 0

gridApply :: IntGrid -> Coord -> Coord -> (IntGridType -> IntGridType) -> IO ()
gridApply (IntGrid array width _) (Coord l1 c1) (Coord l2 c2) func = let
    l1ef = l1 * width
    l2ef = l2 * width
    helperl :: Int -> Int -> IO ()
    helperc :: Int -> IO ()
    helperl l c
      | c <= c2 = do oldval <- readArray array (l + c);
                     writeArray array (l + c) (func oldval);
                     helperl l (c + 1);
      | otherwise = return ()
    helperc l
      | l <= l2ef = helperl l c1 >> helperc (l + width)
      | otherwise = return ()
  in
    helperc l1ef

gridFoldl :: IntGrid -> Coord -> Coord -> a -> (a -> IntGridType -> a) -> IO a
gridFoldl (IntGrid array width _) (Coord l1 c1) (Coord l2 c2) int func = let
    l1ef = l1 * width
    l2ef = l2 * width
    helperl :: Int -> Int -> (a -> IntGridType -> a) -> a -> IO a
    helperc :: Int -> (a -> IntGridType -> a) -> a -> IO a
    helperl l c func' ret
      | c <= c2 = do val <- readArray array (l + c);
                     helperl l (c+1) func' (func' ret val);
      | otherwise = return ret
    helperc l func' ret
      | l <= l2ef = helperl l c1 func' ret >>= helperc (l + width) func'
      | otherwise = return ret
  in
    helperc l1ef func int

readLines :: Handle -> IO (Seq String)
readLines handle = let
    helper list = hIsEOF handle >>= \eof ->
      if eof then return list
      else hGetLine handle >>= \line ->
        helper $ list |> line
  in
    helper empty

parseLine :: String -> Command
parseLine str = if parts !! 0 == "toggle" then Flip (toCoord $ parts !! 1) (toCoord $ parts !! 3)
  else Set (toCoord $ parts !! 2) (toCoord $ parts !! 4) (parts !! 1 == "on")
  where parts = splitOn " " str
        toCoord c = let x = map read $ splitOn "," c in (Coord (x !! 0) (x !! 1))

main :: IO ()
main = do
  grid1 <- createGrid 1000 1000 :: IO BitGrid
  grid2 <- createGrid 1000 1000 :: IO IntGrid
  handle <- openFile "d6_input.txt" ReadMode
  hSetBuffering handle (BlockBuffering $ Just (8192*2))
  lines' <- readLines handle
  traverse_ (applyCommand grid1 . parseLine) lines'
  traverse_ (applyCommand grid2 . parseLine) lines'
  cnt1 <- getCount grid1
  cnt2 <- getCount grid2
  print (cnt1, cnt2)
  hClose handle
