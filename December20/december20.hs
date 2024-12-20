import Data.Array
import Data.List
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import Debug.Trace (trace)

type Point = (Int, Int)
type Grid = Array Point Char

data State = State
  { position :: Point
  , time :: Int
  , cheated :: Bool
  , cheatStart :: Maybe Point
  , cheatEnd :: Maybe Point
  } deriving (Eq, Ord, Show)

parseInput :: String -> (Grid, Point, Point)
parseInput input = (grid, start, end)
  where
    rows = lines input
    height = length rows
    width = length (head rows)
    grid = array ((0, 0), (height - 1, width - 1))
           [((r, c), rows !! r !! c) | r <- [0..height-1], c <- [0..width-1]]
    start = head [(r, c) | r <- [0..height-1], c <- [0..width-1], rows !! r !! c == 'S']
    end = head [(r, c) | r <- [0..height-1], c <- [0..width-1], rows !! r !! c == 'E']

neighbors :: Point -> [Point]
neighbors (r, c) = [(r-1, c), (r+1, c), (r, c-1), (r, c+1)]

isValid :: Grid -> Point -> Bool
isValid grid (r, c) = inRange (bounds grid) (r, c) && grid ! (r, c) /= '#'

move :: Grid -> State -> Point -> State
move grid state pos
  | isValid grid pos = state { position = pos, time = time state + 1 }
  | not (cheated state) = 
      let timeSaved = abs (fst (position state) - fst pos) + abs (snd (position state) - snd pos)
      in state { position = pos, time = time state + 1, cheated = True, cheatStart = Just (position state), cheatEnd = Just pos }
  | otherwise = state

updateCheats :: State -> [(Int, Int)] -> [(Int, Int)]
updateCheats state cheats
  | cheated state =
      let cheatStartPos = fromJust (cheatStart state)
          cheatEndPos = fromJust (cheatEnd state)
          preciseTimeSaved = calculatePreciseTimeSaved cheatStartPos cheatEndPos
      in trace ("Cheat from " ++ show cheatStartPos ++ " to " ++ show cheatEndPos ++ " with timeSaved: " ++ show preciseTimeSaved)
             (preciseTimeSaved, time state) : cheats
  | otherwise = cheats

calculatePreciseTimeSaved :: Point -> Point -> Int
calculatePreciseTimeSaved (r1, c1) (r2, c2) = abs (r1 - r2) + abs (c1 - c2)

bfs :: Grid -> Point -> Point -> [(Int, Int)]
bfs grid start end = go Set.empty (Seq.singleton initialState) []
  where
    initialState = State start 0 False Nothing Nothing

    go _ Seq.Empty cheats = cheats
    go visited (current Seq.:<| queue) cheats
      | position current == end = go visited queue (updateCheats current cheats)
      | otherwise = go visited' queue' cheats
      where
        visited' = Set.insert (position current, cheated current) visited
        queue' = foldl' (Seq.|>) queue (nextStates current)
        
        nextStates state = filter isValidState $ map (move grid state) (neighbors (position state))
        isValidState state = not (Set.member (position state, cheated state) visited')

main :: IO ()
main = do
  input <- readFile "December20/input.txt"
  let (grid, start, end) = parseInput input
  let cheats = bfs grid start end
  print cheats
  let validCheats = filter (\(_, saved) -> saved >= 100) cheats
  print validCheats
  print (length validCheats)
