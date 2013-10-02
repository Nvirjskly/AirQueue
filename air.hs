import DBsimulation hiding (main)
import System.Random
import Text.Printf
import Data.List
import Data.Maybe
import Graphics.Gnuplot.Simple

--queueAdd :: Ord b => (a -> b) -> [a] -> [a] -> [a]

--queueAdd f x q = quickSort f (x ++ q)

queueNumpass q = reverse $ quickSort numpass q

----l@Here we prepare the simulation state data structure.@

data SimulationState = SimulationState {step :: Int,
                                        qFun :: ([AirCraft] -> [AirCraft]),
                                        db :: [AirCraft],
                                        queue :: [AirCraft],
                                        acOnRunway :: Maybe AirCraft,
                                        lastTakeOff :: Int}

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
      p = 3
      acatstep = (filter (\x -> ns == (actualT x)) d)
      newdb = (filter (\x -> ns /= (actualT x)) d)
      addqueue = f (acatstep ++ q)
      newqueue = if ns==lastt then takeoff addqueue else addqueue
      takeoff [] = []
      takeoff (x:xs) = xs
      lastt = if (ns>=(t+p))&&(not.null$addqueue) then ns else t

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

getdb = quickSort numpass (take 200 (randACs 5))

simEnumR :: SimulationState -> [SimulationState]
simEnumR s = if (finalSim s)
                 then []
                 else [s] ++ (simEnumR (simStep s))

simEnum :: ([AirCraft] -> [AirCraft]) -> [SimulationState]
simEnum f = simEnumR (initSim f)

numpass :: AirCraft -> Int
numpass AirCraft {numPass=ac4}
  = ac4

actualT :: AirCraft -> Int
actualT AirCraft {actualTime=ac3}
  = ac3

schT :: AirCraft -> Int
schT AirCraft {scheduleTime=ac2}
  = ac2  

acOnRW :: SimulationState -> (Maybe AirCraft, Int)
acOnRW SimulationState {acOnRunway = r,
                         lastTakeOff = t}
  = (r,t)

----l@This is a standard implementation of quicksort@
----l@such that $q(xs)_{cf} = qs\left([x|x\in xs, cf(x)<cf(p)]\right)$@
quickSort :: Ord b => (a -> b) -> [a] -> [a]
quickSort cf [] = []
quickSort cf (p:xs) = quickSort cf [x | x<-xs, (cf x)<(cf p)] ++ [p] ++
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

acs :: [(AirCraft,Int)]
acs = map (\(a,b) -> (fromJust a,b))
      (filter (\(a,b) -> isJust a) (map acOnRW (simEnum queueNumpass)))

ind :: Int -> Int -> Float
ind a b = (fromIntegral a)/(fromIntegral b)

hasConn Passenger{haveConnection = b} = b

--perpass :: [(Int, Float)]
perpass = (map (\(a,b)->(b,ind (b-(actualT a)) (numpass a)) ) acs)

ps :: AirCraft -> [Passenger]
ps AirCraft {passengers = p} = p

tlbp :: Passenger -> Int -> Int
tlbp Passenger {haveConnection = b,
                connectionTime = t} at
  = if b then at - t
      else 0


timePassLateBy :: AirCraft -> Int -> Int
timePassLateBy ac to = sum lates
  where
  lates = filter (\p -> p >= 0) (map (\p -> tlbp p (to+60)) $ ps ac)

percConLate :: AirCraft -> Int -> Float
percConLate ac to = ind (length late) (length hc) where
  hc = filter hasConn (ps ac)
  late = filter (\p->(tlbp p (to+60))>0) hc

main = do
  plotListStyle [EPS "test.eps",XLabel "Time of takeoff", YLabel "Time waiting in queue", Key Nothing] (defaultStyle{plotType = Points}) (map (\(a,b)->(b,(b-(actualT a))) ) acs)
  plotListStyle [EPS "test2.eps",XLabel "Time of takeoff", YLabel "Time waiting in queue/numpass", Key Nothing] (defaultStyle{plotType = Points}) perpass
  plotListStyle [EPS "test3.eps",XLabel "Time of takeoff", YLabel "Sum of Time Passengers late to connection", Key Nothing] (defaultStyle{plotType = Points}) (map (\(a,b)->(b,timePassLateBy a b)) acs)
  plotListStyle [EPS "test4.eps",XLabel "Time of takeoff", YLabel "Percent late", Key Nothing] (defaultStyle{plotType = Points}) (map (\(a,b)->(b,percConLate a b)) acs)
  --writeFile "tt.csv" (join "\n" (map (\(a,b)->(show b)++","++(show (b-(actualT a)))) acs))
  --writeFile "testrr.out" (join "\n" (map (show.(\(a,b)->(acToDB a,b-(actualT a),toTime b))) acs))
