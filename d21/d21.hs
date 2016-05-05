import Data.List

data Item = Item {cost :: Int, damageVal :: Int, armourVal :: Int} deriving (Eq, Show)
data ItemSet = ItemSet {weapon :: Item, armour :: Item, ring1 :: Item, ring2 :: Item}
  deriving (Eq, Show)

emptyItem = Item 0 0 0

data Turn = Fighter1 | Fighter2 deriving (Eq, Show)

class Fighter a where
  getArmour :: a -> Int
  getDamage :: a -> Int
  getHP :: a -> Int

data Player = Player Int ItemSet
data Boss = Boss Int Int Int deriving (Show)

setCost :: ItemSet -> Int
setCost (ItemSet w a r1 r2) = sum $ map cost [w, a, r1, r2]

setDamage :: ItemSet -> Int
setDamage (ItemSet w a r1 r2) = sum $ map damageVal [w, a, r1, r2]

setArmour :: ItemSet -> Int
setArmour (ItemSet w a r1 r2) = sum $ map armourVal [w, a, r1, r2]

instance Ord ItemSet where
  compare itemset1 itemset2 = compare (setCost itemset1) (setCost itemset2)

dmgFormula :: Int -> Int -> Int
dmgFormula ar dmg = max 1 (dmg - ar)

instance Fighter Player where
  getArmour (Player _ itemset) = setArmour itemset
  getDamage (Player _ itemset) = setDamage itemset
  getHP (Player hp _) = hp

instance Fighter Boss where
  getArmour (Boss _ _ a) = a
  getDamage (Boss _ d _) = d
  getHP (Boss hp _ _) = hp

instance Show Player where
  show (Player hp set) = concat ["Player ", show hp, " ", show $ setDamage set, " ", show $ setArmour set]

weapons :: [Item]
armours :: [Item]
rings :: [Item]
weapons = [Item 8 4 0, Item 10 5 0, Item 25 6 0, Item 40 7 0, Item 74 8 0]
armours = [Item 13 0 1, Item 31 0 2, Item 53 0 3, Item 75 0 4, Item 102 0 5]
rings = [Item 25 1 0, Item 50 2 0, Item 100 3 0, Item 20 0 1, Item 40 0 2, Item 80 0 3]

itemSets :: [ItemSet]
itemSets = sort [ ItemSet w a r1 r2 | w <- weapons, a <- emptyItem:armours,
                  r1 <- emptyItem:rings, r2 <- emptyItem:rings, r1 == emptyItem || r2 == emptyItem || r1/=r2 ]

winner :: (Fighter a, Fighter b) => a -> b -> Turn
winner f1 f2 = let
    dmg12 = dmgFormula (getArmour f2) (getDamage f1)
    dmg21 = dmgFormula (getArmour f1) (getDamage f2)
    t1 = ceiling $ fromIntegral (getHP f2) / fromIntegral dmg12
    t2 = ceiling $ fromIntegral (getHP f1) / fromIntegral dmg21
  in if t1 <= t2 then Fighter1 else Fighter2

parseFile :: String -> Boss
parseFile = foldl (\(Boss hp d a) ln-> case words ln of
    ["Hit", "Points:", hp'] -> Boss (read hp') d a
    ["Damage:", d'] -> Boss hp (read d') a
    ["Armor:", a'] -> Boss hp d (read a')
    _ -> Boss hp d a
  ) (Boss 0 0 0) . lines

main :: IO ()
main = do
  boss <- readFile "d21_input.txt" >>= return . parseFile
  let set1 = head $ dropWhile (\set -> Fighter1 /= winner (Player 100 set) boss) itemSets
  let set2 = head $ dropWhile (\set -> Fighter2 /= winner (Player 100 set) boss) $ reverse itemSets
  print (setCost set1, setCost set2)
