module Main where
  import System.Environment
  import Control.Lens
  import Data.Maybe
  import Data.Set
  import Text.Read

  -- |Looks at the first character in the input string and returns +1 if the
  -- first character is a '+' and -1 if the first character is '-' or Nothing 
  -- otherwise
  getSign :: String -> Maybe Int
  getSign line = do
    (line ^? element 0) >>= signToBool
      where 
        signToBool '+' = Just 1
        signToBool '-' = Just (-1)
        signToBool _   = Nothing

  -- |Takes in a string like "-245" and returns -245. The first character has to
  -- be either '+' or '-' and the next characters have to be numbers or Nothing
  -- is returned
  getOffset :: String -> Maybe Int
  getOffset line = do
    sign <- getSign line
    offset <- readMaybe (tail line) :: Maybe Int
    return (sign * offset)

  -- |Converts a string list of offsets to a list of integer offsets
  getOffsets :: [String] -> [Int]
  getOffsets inLines = catMaybes $ fmap (getOffset) inLines

  -- |Searches an integer list for duplicates, stops and returns the duplicate
  -- when it finds it.
  -- It finds duplicates by maintaining a separate set and checking each element
  -- in the input list against the set
  getDuplicate :: [Int] -> Int
  getDuplicate offsets = searchDuplicates offsets empty
    where
      -- |Originally this function checked duplicates against haskell's list
      -- type but it took 13 minutes to find the answer. With a set it takes
      -- 0.2 seconds. Oof
      searchDuplicates (x:xs) dupSet = 
        if (member (x) dupSet)
           then x 
           else (searchDuplicates xs $ insert x dupSet)

  main :: IO ()
  main = do
    args <- getArgs
    -- Let the user supply the filename. If the user didn't pass in any
    -- arguments then just use the file "input" in the current directory
    let filename = fromMaybe "input" (args ^? element 0)

    -- Convert the string file contents into a list of lines
    fileLines <- fmap lines $ readFile filename

    let offsets = getOffsets fileLines
    -- To get the final offset for part 1 you sum the list of offsets
    putStrLn ("The offset is: " ++ (show $ sum offsets))

    -- Convert the offsets into an infinite circular list, then add the elements
    -- together
    let frequencys = scanl (+) 0 $ cycle offsets

    putStrLn $ "Duplicate: " ++ (show (getDuplicate frequencys))

