import qualified Data.HashMap.Strict as Map
import Data.Word
import Data.List.Split
import Data.Bits
import Data.Char
import Control.Monad.Trans.State.Strict

type Signal = Word16

data LogicGate = Invalid |
                 Constant {wire :: String, signal :: Signal} |
                 Repeater {wire :: String, child :: LogicGate} |
                 Unary {wire :: String, func1 :: (Signal -> Signal), child :: LogicGate} |
                 Binary {wire :: String, func2 :: (Signal -> Signal -> Signal),
                         child1 :: LogicGate, child2 :: LogicGate}

instance Show LogicGate where
    show Invalid = "Invalid"
    show (Repeater _ lg) = "Repeater " ++ show lg
    show (Constant _ sig) = "Constant " ++ show sig
    show (Unary _ _ lg) = concat ["U(", show lg, ")"]
    show (Binary _ _ lhs rhs) = concat ["B(", show lhs, ", ", show rhs, ")"]

isInteger :: String -> Bool
isInteger (x:_) = isDigit x
isInteger _ = False

getWire :: String -> State (Map.HashMap String (Either LogicGate [String])) LogicGate
getWire name = if isInteger name then return (Constant "" $ read name)
  else do
    let remember lg = modify (Map.insert name (Left lg)) >> return lg
    m <- get
    case Map.lookup name m of
        Nothing -> return Invalid
        Just (Left lg) -> return lg
        Just (Right str) -> case str of
          [val] -> if isInteger val then remember $ Constant name (read val)
            else getWire val >>= remember . Repeater name
          ["NOT", wire'] -> getWire wire' >>= remember . Unary name complement
          [wire', "LSHIFT", val] -> getWire wire' >>= remember . Unary name (\x -> shiftL x $ read val)
          [wire', "RSHIFT", val] -> getWire wire' >>= remember . Unary name (\x -> shiftR x $ read val)
          [wire1, "AND", wire2] -> getWire wire1 >>= \lg1 -> getWire wire2 >>= remember . Binary name (.&.) lg1
          [wire1, "OR", wire2] -> getWire wire1 >>= \lg1 -> getWire wire2 >>= remember . Binary name (.|.) lg1
          _ -> return Invalid

wireState :: [(String, [String])] -> Map.HashMap String (Either LogicGate [String])
wireState = Map.fromList . map (\(k,v) -> (k, Right v))

evalLogicGate :: LogicGate -> State (Map.HashMap String Signal) (Maybe Signal)
evalLogicGate Invalid = return Nothing
evalLogicGate gate = do
  let name = wire gate
  let remember s = modify (Map.insert name s) >> return (Just s)

  m <- get
  case Map.lookup name m of
    Just lg -> return (Just lg)
    Nothing -> case gate of
      Invalid -> return Nothing
      Constant _ sig -> remember sig
      Repeater _ lg -> evalLogicGate lg >>= maybe (return Nothing) remember
      Unary _ func lg -> evalLogicGate lg >>= maybe (return Nothing) (remember . func)
      Binary _ func lg1 lg2 -> evalLogicGate lg1 >>=
        maybe (return Nothing) (\s1 -> evalLogicGate lg2 >>=
        maybe (return Nothing) (remember . func s1))

parseLine :: String -> (String, [String])
parseLine line = (rhs, lhs)
  where parts = splitOn ["->"] $ splitOn " " line
        lhs = head parts
        rhs = head $ last parts

main :: IO ()
main = do
  inLines <- readFile "d7_input.txt" >>= return . splitOn "\n"
  let st = wireState $ map parseLine inLines
  let start = evalState (getWire "a") st
  print $ do a1 <- evalState (evalLogicGate start) Map.empty
             a2 <- evalState (evalLogicGate start) (Map.singleton "b" a1)
             return (a1, a2)
