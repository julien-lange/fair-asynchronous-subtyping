import Data.List
import System.FilePath.Posix
import System.Environment
import System.Process
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)


data Dir = Send | Rcv
         deriving (Show, Eq, Ord, Read)

data Tree = Choice [((Dir, String), Tree)]
          | RecDef String Tree
          | RecVar String
          | End
  deriving (Show, Eq, Ord, Read)


genTop :: Int -> Int -> Int -> Tree
genTop i j k = RecDef "X" (genTopRcv i j k)


-- [ ?tc1, ?tc2, ?tc3, ... ?done] 
genTopRcv :: Int -> Int -> Int -> Tree
genTopRcv i 0 k =
  let tcs =   map (\x -> ((Rcv, "tc"++(show x)), genTopSnd i 0 k))[0..i-1]
  in Choice $ tcs ++ [((Rcv, "done"), genSendLeaf k)]
genTopRcv i j k =
  let tcs =   map (\x -> ((Rcv, "tc"++(show x)), genTopRcv i (j-1) k))[0..i-1]
  in Choice $ tcs ++ [((Rcv, "done"), genSendLeaf k)]


-- 
genTopSnd :: Int -> Int -> Int -> Tree
genTopSnd i j k =
  let tms = map (\x -> ((Send, "tm"++(show x)), (RecVar "X")))[0..k-1]
  in Choice $ tms ++ [((Send, "over"), genRcvLeaf i)]


-- -----------------------------------------------------------------
-- -----------------------------------------------------------------


genBot :: Int -> Int -> Int -> Tree
genBot i j k = RecDef "X" (genBotSnd i j k)

genBotSnd :: Int -> Int -> Int -> Tree
genBotSnd i j k = 
  let tms = map (\x -> ((Send, "tm"++(show x)), (RecVar "X")))[0..k-1]
  in Choice $ tms ++ [((Send, "over"), genRcvLeaf i)]


-- -----------------------------------------------------------------
-- -----------------------------------------------------------------


-- rec Y . [?tc1 ; Y ,
--          ?tc2 ; Y , 
--          ?done ; end
--         ]
genRcvLeaf :: Int -> Tree
genRcvLeaf i =
  let tcs =   map (\x -> ((Rcv, "tc"++(show x)), (RecVar "Y")))[0..i-1]
  in RecDef "Y" $ Choice $ tcs ++ [((Rcv, "done"), End)]


-- rec Z . [!tm1 ; Z ,
--          !tm2 ; Z ,  
--          !over ; end
--         ]
genSendLeaf :: Int -> Tree
genSendLeaf k =
  let tcs =   map (\x -> ((Send, "tm"++(show x)), (RecVar "Z")))[0..k-1]
  in RecDef "Z" $ Choice $ tcs ++ [((Send, "over"), End)]


printDir Send = "!"
printDir Rcv = "?"

printTree :: Tree -> String
printTree (Choice xs) =
  let body = intercalate ", " $
             map (\((d,m),t) -> (printDir d)++m++";"++(printTree t)) xs      
  in "[" ++ body++"]"
printTree (RecDef var t) = "rec "++var++"."++(printTree t)
printTree (RecVar var) = var
printTree End = "end"


main :: IO ()
main = do  args <- getArgs
           if ((length args) /= 3)
             then do putStrLn "Usage: GenAsyncTypes <rcv-width> <rcv-depth> <snd-width> "
                     return ()
             else do let x = read (args!!0) :: Int
                         y = read (args!!1) :: Int
                         k = read (args!!2) :: Int
                         f = if length args > 1
                             then args!!1
                             else ""
                         
                     runChecker y x k f
                     

runChecker :: Int -> Int -> Int -> String -> IO ()
runChecker y x k flag =
  let sup = printTree $ genTop x y k
      sub = printTree $ genBot x y k
      cmd = "./Checker t1.txt t2.txt"
  in do writeToFile "t1.txt" sub
        writeToFile "t2.txt" sup
        putStrLn sub
        putStrLn sup
        start <- getCurrentTime
        out <- readProcess "bash" ["-c", cmd] []
        end <- getCurrentTime
        putStrLn $ (show $ diffUTCTime end start)
        print out
        return ()
        



writeToFile :: FilePath -> String -> IO()
writeToFile file content = writeFile file content



