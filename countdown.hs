import Data.List
import Control.Monad (replicateM)
import Data.Function (on)

ops = ['*', '+', '/', '-']

operatorCombinations :: Int -> [[Char]]
operatorCombinations n = replicateM n ops

opCombosForList :: [Float] -> [[Char]]
opCombosForList numsList =
  let len = length numsList
  in operatorCombinations (len - 1)

allPairs :: [Float] -> [([Float],[Char])]
allPairs numsList =
  let perms = permutations numsList
      opsList = opCombosForList numsList
  in [(nums,ops) | nums <- perms, ops <- opsList]

evalNext :: Float -> Float -> Char -> Float
evalNext c n '+' = c + n
evalNext c n '-' = c - n
evalNext c n '*' = c * n
evalNext c n '/' = c / n
evalNext c n _ = error "Invalid op"
        
processSubCombo :: [Float] -> [Char] -> Float
processSubCombo [num] [] = num
processSubCombo (num:nums) (op:ops) = evalNext num (processSubCombo nums ops) op

tryCombo :: Float -> ([Float],[Char]) -> [(Float,([Float],[Char]))]
tryCombo _ ([],_) = []
tryCombo _ (_,[]) = []
tryCombo target (numsList,opsList)
  | diff < 10.0 = [(diff,(numsList,opsList))] ++ tryCombo target (tail numsList, tail opsList)
  | otherwise = tryCombo target (tail numsList, tail opsList)
  where result = processSubCombo numsList opsList
        diff = abs(result-target)

tryCombos :: Float -> [([Float],[Char])] -> [(Float,([Float],[Char]))]
tryCombos _ [] = []
tryCombos t (x:xs) = tryCombo t x ++ tryCombos t xs

findCountdownSolutions :: Float -> [Float] -> [(Float,([Float],[Char]))]
findCountdownSolutions target numsList =
  let pairs = allPairs numsList
  in tryCombos target pairs
  
sortSolsByDiff :: (Float,([Float],[Char])) -> (Float,([Float],[Char])) -> Ordering
sortSolsByDiff (diff1,(nums1,_)) (diff2,(nums2,_))
  | diff1 > diff2 = GT
  | diff1 < diff2 = LT
  | otherwise = sortSolsByLength nums1 nums2

sortSolsByLength :: [Float] -> [Float] -> Ordering
sortSolsByLength nums1 nums2
  | len1 > len2 = GT
  | otherwise = LT
  where len1 = length nums1
        len2 = length nums2
  
playCountDown :: Float -> [Float] -> (Float,([Float],[Char]))
playCountDown target numsList = 
  let sols = findCountdownSolutions target numsList
      ordd = sortBy sortSolsByDiff sols
  in head ordd
