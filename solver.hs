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

