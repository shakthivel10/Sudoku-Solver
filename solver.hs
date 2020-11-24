import Data.List

class (Eq c, Show c) => Config c where
  successors :: c -> [c]
  isGoal :: c -> Bool

solveAll :: (Config c) => c -> [c]
solveAll c = let restSolutions = concat [solveAll c' | c' <- successors c] 
             in if isGoal c then c:restSolutions else restSolutions

solve :: (Config c) => c -> (Maybe c)
solve c = case solveAll c of
            []   -> Nothing
            x:xs -> Just x

data Element = Element Integer Integer Integer Integer deriving Eq -- value row column subMatrixIndex  

instance Show Element where
  show (Element value row column subMatrixIndex) = if (value /= 0) then show value else id "_"

getValue :: Element -> Integer
getValue (Element v _ _ _) = v

getRow :: Element -> Integer
getRow (Element _ r _ _) = r


getColumn :: Element -> Integer
getColumn (Element _ _ c _) = c

getSubMatrixIndex :: Element -> Integer
getSubMatrixIndex (Element _ _ _ s) = s


data SudokuConfig = SudokuConfig [Element] [Element] Integer deriving Eq

instance Show SudokuConfig where
  show (SudokuConfig lst1 lst2 _) = display (lst1 ++ lst2)

display :: [Element] -> [Char]
display l = "\n" ++ displayThreeRows [0..2] l ++ "\n\n" ++ displayThreeRows [3..5] l ++ "\n\n" ++ displayThreeRows [6..8] l

displayThreeRows :: [Integer] -> [Element] -> [Char]
displayThreeRows (x1:(x2:xs)) l = displayRow x1 l ++ "\n" ++ displayRow x2 l ++ "\n" ++ displayRow (head xs) l  

displayRow :: Integer -> [Element] -> [Char]
displayRow r l = displayThree r [0..2] l ++ "   " ++ displayThree r [3..5] l ++ "   " ++ displayThree r [6..8] l

displayThree :: Integer -> [Integer] -> [Element] -> [Char]
displayThree r (x1:(x2:xs)) l =  show (head [ e |  e <- l, getRow e == r, getColumn e == x1]) ++ " " ++ show (head [ e |  e <- l, getRow e == r, getColumn e == x2]) ++ " " ++ show (head [ e |  e <- l, getRow e == r, getColumn e == (head xs)]) 

listFromSudokuConfig :: SudokuConfig -> [Integer]
listFromSudokuConfig (SudokuConfig k u _) = listFromSudokuConfigAddRows (k++u) [0..8]

listFromSudokuConfigAddRows :: [Element] -> [Integer] -> [Integer]
listFromSudokuConfigAddRows  l []     = []
listFromSudokuConfigAddRows  l (x:xs) = listFromSudokuConfigAddRow l x ++ (listFromSudokuConfigAddRows l xs)

listFromSudokuConfigAddRow :: [Element] -> Integer -> [Integer]
listFromSudokuConfigAddRow l r = listFromSudokuConfigAddValues l r [0..8]

listFromSudokuConfigAddValues :: [Element] -> Integer -> [Integer] -> [Integer]
listFromSudokuConfigAddValues l r []     = []
listFromSudokuConfigAddValues l r (x:xs) = [getValue e | e <- l, getRow e == r, getColumn e == x] ++ listFromSudokuConfigAddValues l r xs

addToKnownValues :: SudokuConfig -> Element ->SudokuConfig
addToKnownValues (SudokuConfig k u l) e = (SudokuConfig (e:k) u (l+1))

addToUnknownValues :: SudokuConfig -> Element ->SudokuConfig
addToUnknownValues (SudokuConfig k u l) e = (SudokuConfig k (e:u) (l+1))

sudokuConfigFromList :: [Integer] -> SudokuConfig
sudokuConfigFromList lst = sudokuConfigFromListHelper lst (SudokuConfig [] [] 0)

sudokuConfigFromListHelper :: [Integer] -> SudokuConfig -> SudokuConfig
sudokuConfigFromListHelper [] s@(SudokuConfig k u l) = s
sudokuConfigFromListHelper (x:xs) s@(SudokuConfig k u l) 
            | x == 0    = sudokuConfigFromListHelper xs (addToUnknownValues s (Element 0 (calcRow l) (calcColumn l) (calcSubMatrixIndex l))) 
            | otherwise = sudokuConfigFromListHelper xs (addToKnownValues s (Element x (calcRow l) (calcColumn l) (calcSubMatrixIndex l)))

calcRow :: Integer -> Integer
calcRow v = div v 9

calcColumn :: Integer -> Integer
calcColumn v = mod v 9

calcSubMatrixIndex :: Integer -> Integer
calcSubMatrixIndex v = ((div (calcRow v) 3) * 3 )+ div (calcColumn v) 3

instance Config SudokuConfig where
  successors (SudokuConfig lst [] _)     = [] 
  successors (SudokuConfig lst (x:xs) l) = 
      let used = nub [v | v <- [getValue e | e <- lst, (getRow e == getRow x || getColumn e == getColumn x || getSubMatrixIndex e == getSubMatrixIndex x) ]] in
      [(SudokuConfig ((Element n (getRow x) (getColumn x) (getSubMatrixIndex x)):lst) xs l) | n <- ([1..9] \\ used) ]  

  isGoal (SudokuConfig lst unfilled _) = (null unfilled) && (allRowsValid lst [0..8]) && (allColumnsValid lst [0..8]) && (allSubMatricesValid lst [0..8])

allRowsValid :: [Element] -> [Integer] -> Bool
allRowsValid lst []         = True 
allRowsValid lst (num:nums) = (validRow lst num) && allRowsValid lst nums

validRow :: [Element] -> Integer -> Bool
validRow lst num = null ([1..9] \\ [getValue e | e <- lst, getRow e == num]) 

allColumnsValid :: [Element] -> [Integer] -> Bool
allColumnsValid lst []         = True 
allColumnsValid lst (num:nums) = (validColumn lst num) && allColumnsValid lst nums

validColumn :: [Element] -> Integer -> Bool
validColumn lst num = null ([1..9] \\[getValue e | e <- lst, getColumn e == num]) 

allSubMatricesValid :: [Element] -> [Integer] -> Bool
allSubMatricesValid lst []         = True 
allSubMatricesValid lst (num:nums) = (validSubMatrix lst num) && allSubMatricesValid lst nums

validSubMatrix :: [Element] -> Integer -> Bool
validSubMatrix lst num = null ([1..9] \\ [getValue e | e <- lst, getSubMatrixIndex e == num])

sudokuSolve :: SudokuConfig -> (Maybe SudokuConfig)
sudokuSolve s = solve s