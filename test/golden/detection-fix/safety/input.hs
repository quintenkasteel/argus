module SafetyExample where

-- Pattern rule: avoid partial head function
getFirstElement :: [a] -> a
getFirstElement xs = head xs

-- Security rule: avoid undefined
placeholder :: Int -> String
placeholder _ = undefined
