-- file: ch01/WC.hs
-- lines beginning with "--" are comments.

wordCount = length . lines

main = interact (\input -> (showWordCount input ++ "\n"))
  where showWordCount = show . wordCount
