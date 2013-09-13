import System.Random
import GHC.Float

main = putStrLn "test"

data Passenger = Passenger {haveConnection :: Bool,
                            connectionTime :: Int} deriving(Show)

data ACType = Int

data AirCraft = AirCraft {aType :: Int,
                          scheduleTime :: Int,
                          actualTime :: Int,
                          numPass :: Int,
                          passengers :: [Passenger],
                          arrivalTime :: Int} deriving(Show)

queueAdd :: Ord b => (a -> b) -> a-> [a] -> [a]

queueAdd f x q = quickSort f (x : q)


prng :: Random g=>Int->(g,g) -> [g]
prng seed (a,b) =  randomRs (a, b) . mkStdGen $ seed

rintToDouble :: Int -> Double
rintToDouble x = (fromIntegral x) *(1.0/4294967296.0)

--Box-Muller method
uniform :: Int -> [Float]
uniform seed = map (\(a,b)->a*b) $ zip (map (\(a,b)-> a) xys) ds
               where
                 xs = prng seed (-1.0,1.0)
                 ys = prng (seed+1) (-1.0,1.0)
                 xys = [(x,y) | (x,y) <- (zip xs ys), (x*x)+(y*y)<1]
                 rs = map (\(a,b)->(a*a)+(b*b)) xys
                 ds = map sqrt (map (\x -> -2.0*x) $ map (\(a,b)->a/b) $ zip (map log rs) rs)
                 
--Random A/C
--the 4 will be changed
randAC :: Int -> Int -> AirCraft
randAC seed n = AirCraft {aType = maxPass,
                          scheduleTime = randTime,
                          actualTime = randTime + (floor $ uniform seed !! n),
                          numPass = randPass,
                          passengers = [],
                          arrivalTime = 0}
           where
             maxPass = 50 + (50 * ((prng seed (1,7))!! n))
             randPass = prng seed (10,maxPass) !! n
             randTime = prng seed (0,1440) !! n

si :: Int -> Int
si x = x

test :: [Int]
test = [7, 6, 4, 1, 2, 10, 11]

testair ac1 = ac1.numPass

--quickSort
--cf: comparison function
--xs: list
quickSort :: Ord b => (a -> b) -> [a] -> [a]
quickSort cf [] = []
quickSort cf (p:xs) = quickSort cf [x | x<-xs, (cf x)< (cf p)] ++ [p] ++
                      quickSort cf [x | x<-xs, (cf x)>=(cf p)]



