-- file: ch01/WC.hs
-- lines beginning with "--" are comments.

-- This is the original supplied file, called WC, but really it's a line counters
main = interact wordCount
    where wordCount input = show (length (lines input)) ++ "\n"
