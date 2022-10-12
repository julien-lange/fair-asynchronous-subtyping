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


genTop :: Int -> Int -> Tree
genTop i j = RecDef "X" (genTopRcv i j)


-- [ ?tc1, ?tc2, ?tc3, ... ?done] 
genTopRcv :: Int -> Int -> Tree
genTopRcv i 0 =
  let tcs =   map (\x -> ((Rcv, "tc"++(show x)), genTopSnd i 0))[0..i-1]
  in Choice $ tcs ++ [((Rcv, "done"), genSendLeaf i)]
genTopRcv i j =
  let tcs =   map (\x -> ((Rcv, "tc"++(show x)), genTopRcv i (j-1)))[0..i-1]
  in Choice $ tcs ++ [((Rcv, "done"), genSendLeaf i)]


-- 
genTopSnd :: Int -> Int -> Tree
genTopSnd i j =
  let tms = map (\x -> ((Send, "tm"++(show x)), (RecVar "X")))[0..i-1]
  in Choice $ tms ++ [((Send, "over"), genRcvLeaf i)]


-- -----------------------------------------------------------------
-- -----------------------------------------------------------------


genBot :: Int -> Int -> Tree
genBot i j = RecDef "X" (genBotSnd i j)

genBotSnd :: Int -> Int -> Tree
genBotSnd i j = 
  let tms = map (\x -> ((Send, "tm"++(show x)), (RecVar "X")))[0..i-1]
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
genSendLeaf i =
  let tcs =   map (\x -> ((Send, "tm"++(show x)), (RecVar "Z")))[0..i-1]
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
           if ((length args) /= 2)
             then do putStrLn "Usage: GenAsyncTypes <int> <int> [flag]"
                     return ()
             else do let x = read (args!!0) :: Int
                         y = read (args!!1) :: Int
                         f = if length args > 1
                             then args!!1
                             else ""
                         
                     runChecker y x f
                     

runChecker :: Int -> Int -> String -> IO ()
runChecker y x flag =
  let sup = printTree $ genTop x y
      sub = printTree $ genBot x y
      cmd = "./Checker -i "++flag++" '"++sub++"'  '"++sup++"'"
  in do writeToFile "t1.txt" sub
        writeToFile "t2.txt" sup
        -- putStrLn sub
        -- putStrLn sup
        -- start <- getCurrentTime
        -- out <- readProcess "bash" ["-c", cmd] []
        -- end <- getCurrentTime
        -- putStrLn $ (show $ diffUTCTime end start)
        -- print out
        return ()
        



writeToFile :: FilePath -> String -> IO()
writeToFile file content = writeFile file content



