import System.Environment (getArgs)
import SplitLines (splitLines)

interactWith f inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (f input)

fixLines :: String -> String
fixLines input = unlines (splitLines input)

main = mainWith myFunction
  where mainWith f = do
          args <- getArgs
          case args of
            [input, output] -> interactWith f input output
            _ -> putStrLn "error: exactly 2 args needed"

        myFunction = fixLines
