import DBsimulation hiding (main)
import System.Random
import Text.Printf
import Data.List

main = writeFile "testrr" (show getdb)

queueAdd :: Ord b => (a -> b) -> [a] -> [a] -> [a]

queueAdd f x q = quickSort f (x ++ q)

--data ACQueue = ACQueue {q :: [AirCraft]}
--               deriving (Show)

eliminate :: Maybe a -> a
eliminate (Just a) = a

data SimulationState = SimulationState {step :: Int,
                                        db :: [AirCraft],
                                        queue :: [AirCraft],
                                        lastTakeOff :: Int}
                       deriving (Show)

simStep :: SimulationState -> SimulationState
simStep SimulationState {step = s,
                         db = d,
                         queue = q,
                         lastTakeOff = f}
  = SimulationState {step = s+1,
                     db = d,
                     queue = newqueue,
                     lastTakeOff = f}
    where
      acatstep = (filter (\x->(s+1)==(numpass x)) d)
      newqueue = queueAdd numpass acatstep q

initSim = SimulationState {step = -1,
                           db = getdb,
                           queue = [],
                           lastTakeOff = -15}

getdb = quickSort numpass (take 50 (randACs 5))

numpass :: AirCraft -> Int
numpass AirCraft {aType=ac1,scheduleTime=ac2,
                 actualTime=ac3,numPass=ac4,
                 passengers=ac5,arrivalTime=ac6}
  = ac4

--quickSort
--cf: comparison function
--xs: list
quickSort :: Ord b => (a -> b) -> [a] -> [a]
quickSort cf [] = []
quickSort cf (p:xs) = quickSort cf [x | x<-xs, (cf x)< (cf p)] ++ [p] ++
                      quickSort cf [x | x<-xs, (cf x)>=(cf p)]



