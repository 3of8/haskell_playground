import Data.Bits
import Data.List
import Data.Char

nimSum :: [Integer] -> Integer
nimSum = foldl' xor 0

hasMove :: [Integer] -> Bool
hasMove = not . all (== 0)

isValidMove :: [Integer] -> (Integer, Integer) -> Bool
isValidMove xs (i, a) = i >= 0 && i < genericLength xs && a > 0 && a <= genericIndex xs i

move :: [Integer] -> (Integer, Integer)
move xs = case [(i,x) | (i,x) <- zip [0..] xs, s `xor` x < x] of
            [] -> arbitraryMove
            (i,x) : _ -> (i, x - (s `xor` x))
  where s = nimSum xs
        arbitraryMove = head [(i,1) | (i,x) <- zip [0..] xs, x > 0]

applyMove :: [Integer] -> (Integer, Integer) -> [Integer]
applyMove xs (i,a) = case genericSplitAt i xs of (xs1, x:xs2) -> xs1 ++ [x - a] ++ xs2

readMove :: [Integer] -> IO (Integer,Integer)
readMove xs =
  do putStr "Human: "
     s <- getLine
     case words s of
       [a,b] | all isDigit a && all isDigit b && isValidMove xs (read a, read b) -> return (read a, read b)
       _ -> putStrLn "Invalid move. Input format is \"POSITION AMOUNT\", e.g. \"0 2\"." >> readMove xs
       
readConfiguration :: IO [Integer]
readConfiguration =
  do s <- getLine
     let c = words s
     if all (all isDigit) c then
       return (map read c)
     else
       putStrLn "Invalid configuration. Input format is \"1 2 3\"" >> readConfiguration

data Player = Human | Computer deriving (Show, Eq)

other :: Player -> Player
other Human = Computer
other Computer = Human

printMove :: (Integer,Integer) -> IO ()
printMove (a,b) = putStrLn (show a ++ " " ++ show b)

printConfiguration :: [Integer] -> IO ()
printConfiguration = putStrLn . intercalate " " . map show

playNim :: Player -> [Integer] -> IO ()
playNim p xs
  | not (hasMove xs) = putStrLn $ show (other p) ++ " wins."
playNim p xs =
  do m <- if p == Human then readMove xs else let m = move xs in putStr "Computer: " >> printMove m >> return m
     let xs' = applyMove xs m
     putStr "Game state: "
     printConfiguration xs'
     playNim (other p) xs'

main = 
  do putStrLn "Enter initial configuration"
     c <- readConfiguration 
     putStr "Game state: "
     printConfiguration c
     playNim Computer c

