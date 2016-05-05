import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Data.Maybe

type Reg = Int
type Program = V.Vector Instruction
data ProcessorState = ProcessorState {program :: Program, pc :: Int, registers :: V.Vector Reg}
data Instruction = NOOP | HLF Reg | TPL Reg | INC Reg |
                   JMP Int | JIE Reg Int | JIO Reg Int

processorInit :: Program -> ProcessorState
processorInit prog = ProcessorState prog 0 (V.replicate 2 0)

getReg :: Int -> ProcessorState -> Reg
getReg reg state = registers state V.! reg

setReg :: Int -> Reg -> ProcessorState -> ProcessorState
setReg reg val state = state{registers = registers state V.// [(reg, val)]}

modifyReg :: Int -> (Reg -> Reg) -> ProcessorState -> ProcessorState
modifyReg reg func state = state{registers = V.modify (\v -> MV.modify v func reg) (registers state)}

getInstr :: ProcessorState -> Instruction
getInstr state = fromMaybe NOOP (program state V.!? pc state)

incPC :: Int -> ProcessorState -> ProcessorState
incPC off state = state{pc = off + pc state}

programDone :: ProcessorState -> Bool
programDone state = pc' < 0 || pc' >= proglen
  where pc' = pc state
        proglen = length $ program state

readOffset :: String -> Int
readOffset ('+':val) = read val
readOffset ('-':val) = - read val
readOffset val = read val

strToRegIdx :: String -> Int
strToRegIdx "a" = 0
strToRegIdx "b" = 1

parseLine :: String -> Instruction
parseLine instr = case words $ filter (/=',') instr of
  ["hlf", regId] -> HLF (strToRegIdx regId)
  ["tpl", regId] -> TPL (strToRegIdx regId)
  ["inc", regId] -> INC (strToRegIdx regId)
  ["jmp", off  ] -> JMP $ readOffset off
  ["jie", regId, off] -> JIE (strToRegIdx regId) (readOffset off)
  ["jio", regId, off] -> JIO (strToRegIdx regId) (readOffset off)
  _ -> NOOP

tick :: ProcessorState -> ProcessorState
tick state = case getInstr state of
  HLF reg -> incPC 1 . modifyReg reg (`div` 2) $ state
  TPL reg -> incPC 1 . modifyReg reg (*3) $ state
  INC reg -> incPC 1 . modifyReg reg (+1) $ state
  JMP off -> incPC off state
  JIE reg off -> incPC (if getReg reg state `mod` 2 == 0 then off else 1) state
  JIO reg off -> incPC (if getReg reg state == 1 then off else 1) state
  NOOP -> incPC 1 state

runUntilDone :: ProcessorState -> ProcessorState
runUntilDone = head . dropWhile (not . programDone) . iterate tick

main :: IO ()
main = do
  input <- readFile "d23_input.txt" >>= return . lines
  let program = V.fromList $ map parseLine input
  let part1 = runUntilDone . processorInit $ program
  let part2 = runUntilDone . setReg 0 1 . processorInit $ program
  print (registers part1, registers part2)
