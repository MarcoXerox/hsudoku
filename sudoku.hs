import Control.Monad (msum, (>=>))
import Data.List (delete, sort, group)
import Data.Either (isRight)
import Data.Foldable (toList)
import Data.Sequence hiding (zipWith, filter, sort)
import Prelude hiding (replicate)

type Value  = Int
type Cell   = Either Value [Value]
type Puzzle = Seq Cell

emptyPuzzle :: Puzzle
emptyPuzzle = replicate (9 * 9) (Right [1..9])

newPuzzle :: [[Int]] -> Maybe Puzzle
newPuzzle xs = mcompose (zipWith adjustCell [0..] $ concat xs) emptyPuzzle

puzzleToList :: Puzzle -> [[Int]]
{- for containers=0.5.8.0
puzzleToList = map fromList . fromList . chunksOf 9 
-}
puzzleToList = go . toList
    where go [] = []
          go (x1:x2:x3:x4:x5:x6:x7:x8:x9:xs)
            = (to <$> [x1, x2, x3, x4, x5, x6, x7, x8, x9]) : go xs
          to (Left  e) = e
          to (Right _) = 0

mcompose :: (Foldable t, Monad m) => t (a -> m a) -> a -> m a
mcompose = foldr (>=>) return

sudoku :: Puzzle -> Maybe Puzzle
sudoku puzzle = maybe (Just puzzle) aggregator (findIndexL isRight puzzle)
    where aggregator = msum . map sudoku . makePuzzles puzzle

makePuzzles :: Puzzle -> Int -> [Puzzle]
makePuzzles puzzle k = let Right es = puzzle `index` k in [x | Just x <- [adjustCell k e puzzle | e <- es]]

adjustCell :: Int -> Value -> Puzzle -> Maybe Puzzle
adjustCell _ 0 = Just
adjustCell k e = mcompose [updateAdjacents j e | j <- adjacents k] . update k (Left e)

updateAdjacents :: Int -> Value -> Puzzle -> Maybe Puzzle
updateAdjacents k e puzzle = case delete e <$> puzzle `index` k of
    Right []    -> Nothing
    Left _      -> Just puzzle
    c           -> Just (update k c puzzle)

adjacents :: Int -> [Int]
adjacents n = delete n . uniq $ concat pos
    where pos = [ [x, x + 9 .. x + 9 * 8]
                , [9 * y .. 9 * y + 8]
                , [9 * (q + 3 * (y `div` 3)) + (p + 3 * (x `div` 3))
                    | p <- [0..2], q <- [0..2]]
                ]
          (y, x) = n `divMod` 9

uniq :: Ord a => [a] -> [a]
uniq = map head . group . sort

sample :: [[Int]]
sample = [[5,3,0,0,7,0,0,0,0],
          [6,0,0,1,9,5,0,0,0],
          [0,9,8,0,0,0,0,6,0],
          [8,0,0,0,6,0,0,0,3],
          [4,0,0,8,0,3,0,0,1],
          [7,0,0,0,2,0,0,0,6],
          [0,6,0,0,0,0,2,8,0],
          [0,0,0,4,1,9,0,0,5],
          [0,0,0,0,8,0,0,7,9]]

solved :: [[Int]]
solved = [[5,3,4,6,7,8,9,1,2],
          [6,7,2,1,9,5,3,4,8],
          [1,9,8,3,4,2,5,6,7],
          [8,5,9,7,6,1,4,2,3],
          [4,2,6,8,5,3,7,9,1],
          [7,1,3,9,2,4,8,5,6],
          [9,6,1,5,3,7,2,8,4],
          [2,8,7,4,1,9,6,3,5],
          [3,4,5,2,8,6,1,7,9]]

main :: IO ()
main = getContents >>= solve . map (map read . words) . lines
    where solve lints = case newPuzzle lints >>= sudoku of
            Nothing -> putStrLn "No solution found"
            Just su -> mapM_ print $ puzzleToList su
