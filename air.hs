import DBsimulation hiding (main)
import System.Random
import Text.Printf
import Data.List
import Data.Maybe

--queueAdd :: Ord b => (a -> b) -> [a] -> [a] -> [a]

--queueAdd f x q = quickSort f (x ++ q)

queueNumpass q = quickSort numpass q

--data ACQueue = ACQueue {q :: [AirCraft]}
--               deriving (Show)

eliminate :: Maybe a -> a
eliminate (Just a) = a

----l@Here we prepare the simulation state data structure.@

data SimulationState = SimulationState {step :: Int,
                                        qFun :: ([AirCraft] -> [AirCraft]),
                                        db :: [AirCraft],
                                        queue :: [AirCraft],
                                        acOnRunway :: Maybe AirCraft,
                                        lastTakeOff :: Int}
                       --deriving (Show)

simStep :: SimulationState -> SimulationState
simStep SimulationState {step = s,
                         qFun = f,
                         db = d,
                         queue = q,
                         acOnRunway = r,
                         lastTakeOff = t}
  = SimulationState {step = ns,
                     qFun = f,
                     db = newdb,
                     queue = newqueue,
                     acOnRunway = if ns==lastt then Just (head addqueue) else Nothing,
                     lastTakeOff = lastt}
    where
      ns = s+1
      acatstep = (filter (\x -> ns == (actualT x)) d)
      newdb = (filter (\x -> ns /= (actualT x)) d)
      addqueue = f (acatstep ++ q)
      newqueue = if ns==lastt then takeoff addqueue else addqueue
      takeoff [] = []
      takeoff (x:xs) = xs
      lastt = if (ns>=(t+5))&&(not.null$addqueue) then ns else t

initSim f = SimulationState {step = (head (quickSort id (map actualT getdb)))-1,
                             qFun = f,
                             db = getdb,
                             queue = [],
                             acOnRunway = Nothing,
                             lastTakeOff = -15}

finalSim :: SimulationState -> Bool
finalSim SimulationState {db = d,
                         queue = q}
  = if (null d)&&(null q) then True else False

getdb = quickSort numpass (take 288 (randACs 5))

simEnumR :: SimulationState -> [SimulationState]
simEnumR s = if (finalSim s)
                 then []
                 else [s] ++ (simEnumR (simStep s))

simEnum :: ([AirCraft] -> [AirCraft]) -> [SimulationState]
simEnum f = simEnumR (initSim f)

numpass :: AirCraft -> Int
numpass AirCraft {aType=ac1,scheduleTime=ac2,
                 actualTime=ac3,numPass=ac4,
                 passengers=ac5,arrivalTime=ac6}
  = ac4

actualT :: AirCraft -> Int
actualT AirCraft {aType=ac1,scheduleTime=ac2,
                 actualTime=ac3,numPass=ac4,
                 passengers=ac5,arrivalTime=ac6}
  = ac3

schT :: AirCraft -> Int
schT AirCraft {scheduleTime=ac2}
  = ac2  

acOnRW :: SimulationState -> (Maybe AirCraft, Int)
acOnRW SimulationState {acOnRunway = r,
                         lastTakeOff = t}
  = (r,t)

----l@This is a standard implementation of quicksort@
----l@such that $q(xs)_{cf} = qs\left(\right)$@
quickSort :: Ord b => (a -> b) -> [a] -> [a]
quickSort cf [] = []
quickSort cf (p:xs) = quickSort cf [x | x<-xs, (cf x)< (cf p)] ++ [p] ++
                      quickSort cf [x | x<-xs, (cf x)>=(cf p)]

join :: String -> [String] -> String
join d [] = ""
join d (a:as) = foldl' (\x y -> x++d++y) a as

simToStr :: SimulationState -> String
simToStr SimulationState {step = s,
                         qFun = f,
                         db = d,
                         queue = q,
                         lastTakeOff = t}
  = join "," (map (show.numpass) q)

acs = map (\(a,b) -> (fromJust a,b))
      (filter (\(a,b) -> isJust a) (map acOnRW (simEnum queueNumpass)))

main = do
  writeFile "testrr.out" (join "\n" (map (show.(\(a,b)->(acToDB a,b-(actualT a),b))) acs))
