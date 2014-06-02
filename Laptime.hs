import Data.Time
import Data.Time.Clock
import Data.List
import Text.Printf
import Control.Monad
import Control.Concurrent
import System.Environment
import System.Locale
import System.IO

showTimeDiff :: Double -> String
showTimeDiff d = sgn ++ printf "%02d:%04.1f" min (abs sec)
  where min = abs (truncate (d / 60)) :: Integer
        sec = d - fromIntegral min * 60
        sgn = if d >= 0 then "" else "-"
        
readTimeDiff :: String -> Double
readTimeDiff s = case s2 of
                     [] -> fromIntegral (read s1 :: Integer)
                     ':' : s2 -> fromIntegral (read s1 :: Integer) * 60 + read s2
                     _ -> error "no parse"
  where (s1, s2) = break (== ':') s

deleteLine = putStr "\ESC[1A" >> hFlush stdout
prompt s = putStrLn s >> hFlush stdout >> getLine >> deleteLine >> getCurrentTime              

updateTime lap t remainingTime lapTime = forever $
  do threadDelay 10000
     t' <- getCurrentTime
     let delta = realToFrac (diffUTCTime t' t)
     deleteLine
     printLapInfo lap (remainingTime - delta) lapTime

printLapInfo lap remainingTime lapTime =
  putStrLn $ printf "Lap %d (remaining time: %s, remaining time per lap: %s)" 
               (lap + 1) (showTimeDiff remainingTime) (showTimeDiff lapTime)

processLap lap _ nLaps remainingTime time lapTimes
  | lap >= nLaps =
      do let avgTime = (time - remainingTime) / fromIntegral nLaps
         putStrLn $ replicate 80 '-'
         putStrLn $ "Lap times:\n" ++ intercalate "\n" (map showTimeDiff lapTimes)
         putStrLn $ replicate 80 '-'
         putStrLn $ printf "Race finished. Remaining time: %s, average lap time: %s"
                       (showTimeDiff remainingTime) (showTimeDiff avgTime)
         return lapTimes
processLap lap t nLaps remainingTime time lapTimes =
  do let lapTime = remainingTime / fromIntegral (nLaps - lap)
     tid <- forkIO (updateTime lap t remainingTime lapTime)
     t' <- prompt ""
     killThread tid
     let delta = realToFrac (diffUTCTime t' t)
     processLap (lap + 1) t' nLaps (remainingTime - delta) time (lapTimes ++ [delta])

makeLog :: UTCTime -> UTCTime -> Integer -> Double -> [Double] -> String
makeLog t t' nLaps time lapTimes = printf
  ("Race with %d laps, total allowed time: %s\nStart: %s\nEnd:   %s\n" ++ 
   replicate 80 '-' ++ "\nLap times:\n%s\n" ++ replicate 80 '-' ++
   "\nTime taken:       %s\nRemaining time:   %s\nAverage lap time: %s\n")
  nLaps (showTimeDiff time) (show t) (show t')
  (intercalate "\n" lapInfos) (showTimeDiff delta) (showTimeDiff (time - delta))
  (showTimeDiff (delta / fromIntegral nLaps))
  where delta = realToFrac (diffUTCTime t' t)
        lapInfos = zipWith (\a b -> showTimeDiff a ++ " (rem. time/lap: " ++ showTimeDiff b ++ ")")
                       lapTimes remTimes :: [String]
        partialSums = snd $ mapAccumL (\a b -> (a-b,a-b)) time (0:lapTimes)
        remTimes = zipWith (\a b -> a / fromIntegral (nLaps - b)) partialSums [0..]

main =
  do args <- getArgs
     if length args /= 2 && length args /= 3 then
       putStrLn "Usage: laptime N_LAPS TOTAL_TIME [LOGFILE]"
     else do
       t <- prompt "Press RETURN to start measurement."
       let (nLaps, time) = (read (args !! 0) :: Integer, readTimeDiff (args !! 1))
       lapTimes <- processLap 0 t nLaps time time []
       let logFile = 
             if length args == 3 then args !! 2 
             else "laptime_" ++ formatTime defaultTimeLocale "%F_%T" t ++ ".log"
       t' <- getCurrentTime
       writeFile logFile (makeLog t t' nLaps time lapTimes)

