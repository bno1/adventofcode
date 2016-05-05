
data Raindeer = Raindeer {name :: String, speed :: Int, duration :: Int, cooldown :: Int}

data RaindeerState = Resting {time :: Int, distance :: Int, points :: Int} |
                     Flying  {time :: Int, distance :: Int, points :: Int}

initState :: Raindeer -> RaindeerState
initState r = Flying (duration r) 0 0

step :: (Raindeer, RaindeerState) -> (Raindeer, RaindeerState)
step (r, s@(Resting t dist pct))
  | t <= 1 = (r, Flying (duration r) dist pct)
  | otherwise = (r, s{time = t - 1})
step (r, s@(Flying t dist pct))
  | t <= 1 = (r, Resting (cooldown r) (dist + speed r) pct)
  | otherwise = (r, s{time = t - 1, distance = dist + speed r})

givePoints :: Int -> RaindeerState -> RaindeerState
givePoints p s = s{points = points s + p}

updatePoints :: [(Raindeer, RaindeerState)] -> [(Raindeer, RaindeerState)]
updatePoints lst = map (\(r, s) -> (r, if distance s == maxDist then givePoints 1 s else s)) lst
  where maxDist = maximum . map (distance . snd) $ lst

simulate :: Int -> [Raindeer] -> Int
simulate t raindeers = let
    helper :: Int -> [(Raindeer, RaindeerState)] -> Int
    helper 0 = maximum . map (points . snd)
    helper tleft = helper (tleft - 1) . updatePoints . map step
  in helper t $ map (\r -> (r, initState r)) raindeers

parseLine :: String -> Raindeer
parseLine str = case words str of
  [name', _, _, speed', _, _, duration', _, _, _, _, _, _, cooldown', _] ->
    Raindeer name' (read speed') (read duration') (read cooldown')
  _ -> Raindeer "error" (-1) (-1) (-1)

computeDistance :: Int -> Raindeer -> Int
computeDistance t (Raindeer _ s d c) =
  (d * steps + min d tleft) * s
  where (steps, tleft) = quotRem t (d + c)

main :: IO ()
main = do
  input <- readFile "d14_input.txt" >>= return . lines
  let raindeers = map parseLine input
  print (maximum $ map (computeDistance 2503) raindeers, simulate 2503 raindeers)
