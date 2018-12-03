module Main where
  import System.Environment
  import Control.Lens
  import Data.Maybe
  import Data.List

  -- |This takes in a sorted string and finds duplicate characters.
  findDups :: String -> Int
  -- Check to make sure that two elements match (but not more than two)
  findDups (x:y:xs) = if (x == y)
                         -- If more than two match, delete the rest of the
                         -- matching characters and keep searching
                         then if (length xs > 0) && (head xs == x)
                                 then findDups $ filter (x /=) xs
                                 else 1
                         else findDups (y : xs)
  findDups _        = 0

  -- |This takes in a sorted string and finds triplicate characters.
  findTrips :: String -> Int
  -- Check to make sure that three elements match (but not more than three)
  findTrips (x:y:z:xs) = if (x == y) && (y == z)
                         -- If more than three match, delete the rest of the
                         -- matching characters and keep searching
                         then if (length xs > 0) && (head xs == x)
                                 then findTrips $ filter (x /=) xs
                                 else 1
                         else findTrips (y : z : xs)
  findTrips _          = 0

  -- |Returns (1, 1) if the ID contains any duplicate characters and any 
  -- triplicate characters. The first elem in the tuple corresponds to 
  -- duplicates and the second elem corresponds to triplicates.
  scanID :: String -> (Int, Int)
  scanID id = (findDups sID, findTrips sID)
    where sID = sort id

  -- |Returns true if the two IDs are only off by one character
  compIDs :: String -> String -> Bool
  compIDs lhs rhs
    | (comp lhs rhs) == 1 = True
    | otherwise           = False
    where
      comp (l:ls) (r:rs) = if l == r
                           then comp ls rs
                           else 1 + (comp ls rs)
      comp _ _           = 0

  main :: IO ()
  main = do
    args <- getArgs
    -- Let the user supply the filename. If the user didn't pass in any
    -- arguments then just use the file "input" in the current directory
    let filename = fromMaybe "input" (args ^? element 0)

    -- Convert the string file contents into a list of lines
    ids <- fmap lines $ readFile filename

    -- Part 1
    -- Look for duplicates and triplicates in all of the ids, then add them 
    -- together
    let dupstrips = fmap scanID ids
    let sumTuples (d1, t1) (d2, t2) = (d1 + d2, t1 + t2)
    let (dups, trips) = foldr sumTuples (0,0) dupstrips

    let checksum = dups * trips in
        putStrLn ("Checksum: " ++ (show checksum))

    -- Part 2
    -- Check each id against every other id and filter out the ids that don't
    -- have any off by one matches
    let correctIDs = filter (\id -> or $ map (compIDs id) ids) ids

    mapM_ (putStrLn . ("Correct ID: " ++)) correctIDs
