import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(ExitFailure))

interactWith f input1File input2File = do
  input1 <- readFile input1File
  input2 <- readFile input2File
  if f input1 input2
    then do
      putStrLn "identical"
    else do
      putStrLn "different"
      exitWith (ExitFailure 1)

main = do
  args <- getArgs
  case args of
    [input1, input2] -> interactWith f input1 input2
    _ -> putStrLn "error: exactly 2 args needed"
  where f = (==)

