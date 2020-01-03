module Left
    ( lefts
    ) where

lefts :: Int -> String -> String
lefts n = unlines . map (left n) . lines

left :: Int -> String -> String
left = slice 0

slice :: Int -> Int -> String -> String
slice from to = take (to - from) . drop from
