import qualified Data.Set as S

data House = House Int Int deriving (Eq)
data Santa = Santa House (S.Set House)

instance Num House where
  House x1 y1 - House x2 y2 = House (x1 - x2) (y1 - y2)
  House x1 y1 + House x2 y2 = House (x1 + x2) (y1 + y2)

instance Ord House where
  compare (House x1 y1) (House x2 y2) = if x1 == x2 then compare y1 y2 else compare x1 x2

toDir :: Char -> House
toDir '^' = House 0 1
toDir '<' = House (-1) 0
toDir '>' = House 1 0
toDir 'v' = House 0 (-1)
toDir _ = House 0 0

initialSanta :: Santa
initialSanta = Santa (House 0 0) (S.singleton $ House 0 0)

advance :: Santa -> House -> Santa
advance (Santa h set) dir = Santa (h + dir) (S.insert (h + dir) set)

looper :: (Int, Santa, Santa, Santa) -> Char -> (Int, Santa, Santa, Santa)
looper (step, santa1, santa2, santa3) c = (
  step + 1,
  advance santa1 dir,
  if even step then advance santa2 dir else santa2,
  if  odd step then advance santa3 dir else santa3)
  where dir = toDir c

main :: IO ()
main = do
  input <- readFile "d3_input.txt"
  let (_, Santa _ s1, Santa _ s2, Santa _ s3) = foldl looper (0, initialSanta, initialSanta, initialSanta) input
  print (S.size s1, S.size $ S.union s2 s3)
