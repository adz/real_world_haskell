main = do
   src <- readFile "quux.txt"
   putStr $ wordCount src
 where wordCount input = (show . length . lines $ input) ++ "\n"
