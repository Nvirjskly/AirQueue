import System.Random

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


prng :: Int->(Int,Int) -> [Int]
prng seed (a,b) =  randomRs (a, b) . mkStdGen $ seed

rintToDouble :: Int -> Double
rintToDouble x = (fromIntegral x) *(1.0/4294967296.0)

uniform :: Int -> [(Double,Double)]
uniform seed = [(x,y) | (x,y) <- (unify xs ys),
                (x*x)+(y*y)<1]
               where
                 xs = map rintToDouble (prng seed (minBound::Int,maxBound::Int))
                 ys = map rintToDouble (prng (seed+1) (minBound::Int,maxBound::Int))
                 unify (a:as) (b:bs) = (a,b) : unify as bs


--Random A/C
randAC :: Int -> AirCraft
randAC n = AirCraft {aType = maxPass,
                     scheduleTime = randTime,
                     actualTime = 4,
                     numPass = randPass,
                     passengers = [],
                     arrivalTime = 0}
           where
             maxPass = 50 + (50* ((prng 4 (1,7))!! n))
             randPass = prng 4 (10,maxPass) !! n
             randTime = prng 4 (0,1440) !! n
             uniform = rintToDouble (prng 4 (minBound::Int,maxBound::Int) !! n)
             




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



