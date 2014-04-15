-- file: ch01/WC.hs
-- lines beginning with "--" are comments.

main = do
   src <- readFile "quux.txt"
   putStr $ wordCount src
 where wordCount input = show . length . lines input ++ "\n"
