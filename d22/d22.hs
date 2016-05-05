import Data.List
import qualified Data.PQueue.Min as PM

data Turn = PlayerTurn | BossTurn deriving (Eq, Show)
data EffectPhase = Setup | Tick Turn | Remove

type SpellFunc = (Player, Boss) -> EffectPhase -> (Player, Boss)
data Effect = Effect {effectId :: Int, effectDur :: Int, effectFunc :: SpellFunc}
data Spell = SpellInstant {spellCost :: Int, spellEffect :: Effect} |
             SpellDuration {spellCost :: Int, spellEffect :: Effect}

data Player = Player {pHP :: Int, pMana :: Int, pArmour :: Int} deriving (Show)
data Boss = Boss {bHP :: Int, bDmg :: Int, bArmour :: Int} deriving (Show)

data Fight = Fight {getPlayer :: Player, getBoss :: Boss, getEffects :: [Effect]}

data Node = Node Turn Fight Int [Node]

class Character a where
  getHP :: a -> Int
  heal :: a -> Int -> a
  modifyArmour :: a -> (Int -> Int) -> a
  dealSpellDamage :: a -> Int -> a
  dealPhysDamage :: a -> Int -> a

instance Eq Effect where
  (==) effect1 effect2 = effectId effect1 == effectId effect2

instance Eq Spell where
  (==) spell1 spell2 = spellEffect spell1 == spellEffect spell2

instance Ord Spell where
  compare spell1 spell2 = compare (spellCost spell1) (spellCost spell2)

instance Character Player where
  getHP = pHP
  heal player amount = player{pHP = amount + pHP player}
  modifyArmour player func = player{pArmour = func $ pArmour player}

  dealSpellDamage player dmg = player{pHP = pHP player - dmg}
  dealPhysDamage player dmg = player{pHP = pHP player - dmgFormula (pArmour player) dmg}

instance Character Boss where
  getHP = bHP
  heal boss amount = boss{bHP = amount + bHP boss}
  modifyArmour boss func = boss{bArmour = func $ bArmour boss}

  dealSpellDamage boss dmg = boss{bHP = bHP boss - dmg}
  dealPhysDamage boss dmg = boss{bHP = bHP boss - dmgFormula (bArmour boss) dmg}

instance Eq Node where
  (==) node1 node2 = getManaSpent node1 == getManaSpent node2

instance Ord Node where
  compare node1 node2 = compare (getManaSpent node1) (getManaSpent node2)

decrementDuration :: Effect -> Effect
decrementDuration eff = eff{effectDur = effectDur eff - 1}

effectDone :: Effect -> Bool
effectDone eff = effectDur eff <= 0

magicMissle = SpellInstant 53 (Effect 1 0 (\(p, b) _ -> (p, dealSpellDamage b 4)))
drain = SpellInstant 73 (Effect 2 0 (\(p, b) _ -> (heal p 2, dealSpellDamage b 2)))
shield = SpellDuration 113 (Effect 3 6 (\(p, b) phase -> case phase of
  Setup -> (modifyArmour p (+7), b)
  Tick _ -> (p, b)
  Remove -> (modifyArmour p (subtract 7), b)))
poison = SpellDuration 173 (Effect 4 6 (\(p, b) phase -> case phase of
  Tick _ -> (p, dealSpellDamage b 3)
  _ -> (p, b)))
recharge = SpellDuration 229 (Effect 5 5 (\(p, b) phase -> case phase of
  Tick _ -> (modifyMana p (+101), b)
  _ -> (p, b)))
turnDamage = SpellDuration 0 (Effect 6 maxBound (\(p, b) phase -> case phase of
  Tick PlayerTurn -> (dealSpellDamage p 1, b)
  _ -> (p, b)))

playerSpells = sort [magicMissle, drain, shield, poison, recharge]

modifyMana :: Player -> (Int -> Int) -> Player
modifyMana player func = player{pMana = func $ pMana player}

dmgFormula :: Int -> Int -> Int
dmgFormula ar dmg = max 1 (dmg - ar)

castSpell :: Fight -> Spell -> Fight
castSpell fight spell = let
    player' = modifyMana (getPlayer fight) (subtract (spellCost spell))
    (player1, boss1) = effectFunc (spellEffect spell) (player', getBoss fight) Setup
  in case spell of
    SpellInstant _ effect -> Fight player1 boss1 (getEffects fight)
    SpellDuration _ effect -> Fight player1 boss1 (effect:getEffects fight)

updateEffects :: Fight -> Turn -> Fight
updateEffects fight turn = let
    applyEffect :: EffectPhase -> (Player, Boss) -> Effect -> (Player, Boss)
    applyEffect phase pb eff = effectFunc eff pb phase

    afterDecrement = map decrementDuration $ getEffects fight
    (remove, left) = partition effectDone afterDecrement

    afterEffects = foldl (applyEffect (Tick turn)) (getPlayer fight, getBoss fight) $ getEffects fight
    (player', boss') = foldl (applyEffect Remove) afterEffects remove
  in
    Fight player' boss' left

canCast :: Fight -> Spell -> Bool
canCast fight spell = spellCost spell <= pMana (getPlayer fight) &&
                      notElem (spellEffect spell) (getEffects fight)

fightFinished :: Fight -> Bool
fightFinished fight = getHP (getPlayer fight) <= 0 || getHP (getBoss fight) <= 0

parseFile :: String -> Boss
parseFile = foldl (\(Boss hp d a) ln-> case words ln of
    ["Hit", "Points:", hp'] -> Boss (read hp') d a
    ["Damage:", d'] -> Boss hp (read d') a
    ["Armor:", a'] -> Boss hp d (read a')
    _ -> Boss hp d a
  ) (Boss 0 0 0) . lines

getManaSpent :: Node -> Int
getManaSpent (Node _ _ ms _) = ms

genTree :: Turn -> (Fight, Int) -> Node
genTree PlayerTurn (fight, manaSpent) = let
    fight' = updateEffects fight PlayerTurn
    castableSpells = filter (canCast fight') playerSpells
    nextNodes = if fightFinished fight'
                then [genTree BossTurn (fight', manaSpent)]
                else map (\spell -> genTree BossTurn (castSpell fight' spell, manaSpent + spellCost spell)) castableSpells
  in if fightFinished fight
     then Node PlayerTurn fight manaSpent []
     else Node PlayerTurn fight manaSpent nextNodes

genTree BossTurn (fight, manaSpent) = let
    fight1 = updateEffects fight BossTurn
    fight2 = if fightFinished fight1
             then fight1
             else fight1{getPlayer = dealPhysDamage (getPlayer fight1) (bDmg $ getBoss fight1)}
  in if fightFinished fight
     then Node BossTurn fight manaSpent []
     else Node BossTurn fight manaSpent [genTree PlayerTurn (fight2, manaSpent)]

findMin :: PM.MinQueue Node -> Int -> Int
findMin queue ms = if PM.null queue then ms else let
    (Node turn fight manaSpent subnodes) = PM.findMin queue
    queue' = PM.deleteMin queue
    playerWon = getHP (getPlayer fight) > 0 && getHP (getBoss fight) <=0
  in if manaSpent >= ms
     then findMin queue' ms
     else if playerWon
          then findMin queue' manaSpent
          else findMin (foldl (\q n -> if getManaSpent n < ms then PM.insert n q else q) queue' subnodes) ms

main :: IO ()
main = do
  boss <- readFile "d22_input.txt" >>= return . parseFile
  let fight = Fight (Player 50 500 0) boss []
  let sn = genTree PlayerTurn (fight, 0)
  let sn2 = genTree PlayerTurn (castSpell fight turnDamage, 0)
  print (findMin (PM.singleton sn) maxBound, findMin (PM.singleton sn2) maxBound)
