import System.Random
import System.Environment
import Text.Printf

data Passenger = Passenger {haveConnection :: Bool,
                            connectionTime :: Int} deriving(Show)

data ACType = Int

data AirCraft = AirCraft {aType :: Int,
                          scheduleTime :: Int,
                          actualTime :: Int,
                          numPass :: Int,
                          passengers :: [Passenger],
                          arrivalTime :: Int} deriving(Show)
						  
prng :: Random g => Int -> (g,g) -> [g]
prng seed (a,b) =  randomRs (a, b) . mkStdGen $ seed

--Box-Muller method
uniform :: Int -> [Float]
uniform seed = map (\(a,b)->a*b) $ zip (map (\(a,b)-> a) xys) ds
               where
                 xs = prng seed (-1.0,1.0)
                 ys = prng (seed+1) (-1.0,1.0)
                 xys = [(x,y) | (x,y) <- (zip xs ys), (x*x)+(y*y)<1]
                 rs = map (\(a,b)->(a*a)+(b*b)) xys
                 ds = map sqrt $ map (\x -> -2.0*x) $ map (\(a,b)->a/b) $ zip (map log rs) rs
				 
--Random Passenger
randPass :: Int -> Int -> Int -> Passenger
randPass scht seed n = Passenger {haveConnection = (prng seed (0::Int,9)!!n)==0,
                             connectionTime = scht+120+(floor (15*(uniform seed !! n)))}

--Random A/C
randAC :: Int -> Int -> AirCraft
randAC seed n = AirCraft {aType = maxPass,
                      scheduleTime = randTime,
                      actualTime = randTime+(floor$meanDepart+(stdDepart*(uniform seed!!n))),
                      numPass = randNumPass,
                      passengers = take randNumPass randPassengers,
                      arrivalTime = randTime+60}
                where
                  maxPass = 50 + (50 * ((prng seed (1,7))!! n))
                  randNumPass = prng seed (10,maxPass) !! n
                  randTime = prng seed (180,1260) !! n
                  stdDepart = 25.5200688 --Standard Deviation of departure
                  meanDepart = -4.932468
                  randPassengers = map (\x->randPass randTime seed x) [0..]

randACs seed = map (randAC seed) [1..]

toTime :: Int -> String
toTime t = concat [(printf "%02d" hour),":",(printf "%02d" minute)]
  where
    minute = mod t 60
    hour = quot (t-minute) 60

acToDB :: AirCraft -> String
acToDB AirCraft {aType=ac1,scheduleTime=ac2,
                 actualTime=ac3,numPass=ac4,
                 passengers=ac5,arrivalTime=ac6}
  =  concat [show ac1,",",toTime ac2,",",
             toTime ac3,",",show ac4,",","...,",toTime ac6]

acsToDB :: [AirCraft] -> String
acsToDB (ac:acs) = foldl (\a b->concat [a,"\n",b]) (acToDB ac) (map acToDB acs)

rInt :: [String] -> [Int]
rInt xs = map read xs
                   
--getIntArg :: [Int]
getIntArg = fmap rInt getArgs

main = do
  xs <- getIntArg
  writeFile "aircraftdb.csv" "A/C Type,Sched. Time,Actual Time,No. Pass.,Pass.,Ariv. Time\n"
  appendFile "aircraftdb.csv" (acsToDB (take (xs!!1) . randACs $ xs!!0))
