import qualified Data.HashMap.Lazy as HM
import Data.List.Split

data Aunt = Aunt Int (HM.HashMap String Int) deriving (Show)

number :: Aunt -> Int
number (Aunt n _) = n

parseLine :: String -> Aunt
parseLine str = Aunt num . HM.fromList . map (\[k, v] -> (k, read v)) . tail $ tokens
  where tokens = chunksOf 2 . words . filter (`notElem` ",:") $ str
        num = read . last . head $ tokens

check :: HM.HashMap String (Int -> Bool) -> Aunt -> Bool
check h1 (Aunt _ h2) = HM.foldl' (&&) True $ HM.intersectionWith ($) h1 h2

main :: IO ()
main = do
  input <- readFile "d16_input.txt" >>= return . lines
  let aunts = map parseLine input

  let aunt1 = HM.fromList [("children", (==3)), ("cats", (==7)),
                           ("samoyeds", (==2)), ("pomeranians", (==3)),
                           ("akitas", (==0)), ("vizslas", (==0)),
                           ("goldfish", (==5)), ("trees", (==3)),
                           ("cars", (==2)), ("perfumes", (==1))]

  let aunt2 = HM.fromList [("children", (==3)), ("cats", (>7)),
                           ("samoyeds", (==2)), ("pomeranians", (<3)),
                           ("akitas", (==0)), ("vizslas", (==0)),
                           ("goldfish", (<5)), ("trees", (>3)),
                           ("cars", (==2)), ("perfumes", (==1))]

  print (map number $ filter (check aunt1) aunts, map number $ filter (check aunt2) aunts)
  return ()
