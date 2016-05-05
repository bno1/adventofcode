import qualified Data.HashSet as S
import qualified Data.HashMap.Strict as M
import Data.List.Split
import Data.List
import Data.Char (isUpper)

type Atom = String
type Molecule = [String]

electron :: Atom
electron = "e"

strToMolecule :: String -> Molecule
strToMolecule = tail . split (keepDelimsL $ whenElt Data.Char.isUpper)

moleculeToStr :: Molecule -> String
moleculeToStr = concat

parseLines :: [String] -> (M.HashMap Atom [Molecule], Molecule)
parseLines = foldl (\r@(m, s) ln ->
    case words ln of
      [atom, "=>", molec] -> case M.lookup atom m of
          Nothing -> (M.insert atom [strToMolecule molec] m, s)
          Just lst -> (M.insert atom ((strToMolecule molec):lst) m, s)
      [] -> r
      [molecule] -> (m, strToMolecule molecule)
      _ -> r
  ) (M.empty, [])

components :: [a] -> [([a], a, [a])]
components lst = map (\n -> let (x:xs) = drop n lst in (take n lst, x, xs))
  [0..(length lst -1)]

components2 :: [a] -> [([a], [a])]
components2 lst = map (\n -> (take n lst, drop n lst)) [0..(length lst -1)]

explode :: M.HashMap Atom [Molecule] -> Molecule -> S.HashSet Molecule
explode m = foldl (\set (prefix, atom, suffix) ->
    foldl (\set molecule -> S.insert (prefix ++ molecule ++ suffix) set) set (M.lookupDefault [] atom m)
  ) S.empty . components

replaceOne :: (Eq a) => [a] -> [a] -> [a] -> [a]
replaceOne x y lst = case find (\(_, suffix) -> x `isPrefixOf` suffix) (components2 lst) of
  Nothing -> lst
  Just (prefix, suffix) -> prefix ++ y ++ (drop (length x) suffix)

trim :: [(Atom, Molecule)] -> Molecule -> Molecule
trim lst molecule = case find (\(a, m) -> if a == electron then
                                            if m == molecule then True
                                            else False
                                          else m `isInfixOf` molecule) lst
  of
    Nothing -> molecule
    Just (atom, m) -> replaceOne m [atom] molecule

minSteps :: M.HashMap Atom [Molecule] -> Molecule -> Int
minSteps m molecule = let
    lst = sortBy (\(_, a) (_, b) -> compare (length b) (length a)) .
          concatMap (\(k, v) -> map ((,) k) v) $ M.toList m

    helper :: Int -> Molecule -> Int
    helper step molec
      | molec == [electron] = step
      | otherwise = helper (step+1) $ trim lst molec
  in helper 0 molecule

main :: IO ()
main = do
  input <- readFile "d19_input.txt" >>= return . lines
  let (transf, molecule) = parseLines input
  print (S.size $ explode transf molecule, minSteps transf molecule)
