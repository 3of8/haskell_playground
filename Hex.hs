import Control.Arrow
import Data.Maybe
import Data.List
import Data.Set (Set)
import qualified Data.Set as S
import Data.Array (Array, (!), (//))
import Data.Ix
import qualified Data.Array as A
import System.Random


type Position = (Int, Int)
data Player   = Red | Blue deriving (Eq, Ord, Show, Read)
newtype Field = Field (Array Position (Maybe Player)) deriving (Eq)
type Strategy = Player -> Field -> Position

instance Show Field where
  show f = intercalate "\n" [showLine y | y <- [fieldHeight, fieldHeight - 1 .. 1]]
    where showLine y = replicate (y - 1) ' ' ++ intercalate " " [showPos (x,y) | x <- [1..fieldWidth]]
          showPos pos = case playerAt pos f of
                          Nothing   -> "○"
                          Just Red  -> "●"
                          Just Blue -> "◍"
                          
otherPlayer :: Player -> Player
otherPlayer Red  = Blue
otherPlayer Blue = Red
                          

fieldDimensions :: (Int, Int)
fieldDimensions = (11, 11)
(fieldWidth, fieldHeight) = fieldDimensions


fieldBounds :: ((Int,Int), (Int,Int))
fieldBounds = ((1, 1), fieldDimensions)

emptyField :: Field
emptyField = Field (A.listArray fieldBounds (take (rangeSize fieldBounds) (repeat Nothing)))

isValidPosition :: Position -> Bool
isValidPosition = inRange fieldBounds

neighbors :: Position -> [Position]
neighbors (x,y) = filter isValidPosition [(x+1,y), (x-1,y), (x,y+1), (x,y-1), (x+1,y-1), (x-1,y+1)]

neighbored :: Position -> Position -> Bool
neighbored p q = p `elem` neighbors q

playerAt :: Position -> Field -> Maybe Player
playerAt p (Field a) = if isValidPosition p then a ! p else Nothing

setPlayerAt :: Position -> Maybe Player -> Field -> Maybe Field
setPlayerAt pos pl (Field a)
  | isValidPosition pos = Just (Field (a // [(pos, pl)]))
  | otherwise           = Nothing

dfs :: Player -> [Position] -> Field ->  Set Position
dfs pl xs f = dfs' xs (S.fromList xs)
  where dfs' [] visited = visited
        dfs' (x:xs) visited =
          let ns = [x' | x' <- neighbors x, playerAt x' f == Just pl, x' `S.notMember` visited]
          in  dfs' (ns ++ xs) (S.union (S.fromList ns) visited)

hasWon :: Player -> Field -> Bool
hasWon pl f = any isFinal (S.toList (dfs pl initial f))
  where initial = case pl of Red  -> [(1,i) | i <- [1..fieldHeight]]
                             Blue -> [(i,1) | i <- [1..fieldWidth]]
        isFinal (x,y) = (pl == Red && x == fieldWidth) || (pl == Blue && y == fieldHeight)

isValidMove :: Position -> Field -> Bool
isValidMove p f = isValidPosition p && isNothing (playerAt p f)

makeMove :: Player -> Position -> Field -> Maybe Field
makeMove pl pos f
  | isValidMove pos f = setPlayerAt pos (Just pl) f
  | otherwise         = Nothing


trivialStrategy :: Strategy
trivialStrategy _ f = head [pos | pos <- range fieldBounds, isValidMove pos f]

randomStrategy :: [Position] -> Strategy
randomStrategy rs _ f = head [pos | pos <- rs, isValidMove pos f]

playGame :: Strategy -> Strategy -> ([Field], Player)
playGame s1 s2 = playGame' emptyField Red
  where strategy p f = case p of {Red -> s1 p f; Blue -> s2 p f}
        playGame' f pl
          | hasWon (otherPlayer pl) f = ([f], otherPlayer pl)
          | otherwise = 
              let pos = strategy pl f
              in  case makeMove pl pos f of
                    Nothing -> error ("Invalid move by player " ++ show pl ++ ": " ++ show pos)
                    Just f' -> first (f:) (playGame' f' (otherPlayer pl))

main = 
  do g <- newStdGen
     let f xs = case xs of (x:y:xs') -> (x,y) : f xs'
     let rs = f (randomRs (1,11) g)
     let (fs, pl) = playGame (randomStrategy rs) (randomStrategy rs)
     mapM_ print fs
     putStrLn ("Winner: " ++ show pl)

