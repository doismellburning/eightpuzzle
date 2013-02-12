-- http://6brand.com/solving-8-puzzle-with-artificial-intelligence.html

import Data.Array
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Map as Map

type Coord = (Integer, Integer)
type State = Array Coord Integer

zero_position :: State -> Coord
-- TODO Implement this better... totally some kind of filter / map / something
zero_position state = let zp ((x, 0):xs) = x
                          zp (_:xs) = zp xs
                          zp [] = error "Couldn't find a zero_position in the State"
                      in zp (assocs state)

swap :: State -> Coord -> State
swap state coord = let z = zero_position state
                       o = state!coord
                   in state // [(z, o), (coord, 0)]

data Direction = Up
               | Down
               | Left
               | Right deriving (Enum, Show)

directions = enumFrom Up -- TODO Haxx?? :(

type Branch = (Direction, State)
branches :: State -> [Branch]
branches state = let maybe_branches = [(direction, branch_toward state direction) | direction <- directions]
                 in map (\(x,y) -> (x, fromJust y)) (filter (\(_,y) -> isJust y) maybe_branches) -- HAXX - I have (x, Maybe y) and I want all the (x, y) where it's Just...

coord_direction :: Coord -> Direction -> Coord
coord_direction (x,y) Up = (x, y+1)
coord_direction (x,y) Down = (x, y-1)
coord_direction (x,y) Main.Left = (x-1, y) -- TODO Main.Right / Prelude.Right (Data.Either)? Scope fail I guess, but why/how/better?
coord_direction (x,y) Main.Right = (x+1, y)

move_coord :: State -> Direction -> Maybe Coord
-- Return Just new coord for direction if in bounds or Nothing if out of bounds
move_coord state direction = let cd = coord_direction (zero_position state) direction
                             in case (inRange (bounds state) cd) of
                                  True -> Just cd
                                  False -> Nothing

branch_toward :: State -> Direction -> Maybe State
branch_toward state direction = let mc = move_coord state direction
                                in case mc of
                                   Just coord -> Just (swap state coord)
                                   Nothing -> Nothing

type SearchState = Map.Map State [Direction]
data StateQueue = SimpleQueue [State]

start_state_queue :: State -> StateQueue
start_state_queue state = SimpleQueue [state]

state_queue_get :: StateQueue -> (StateQueue, State)
state_queue_get (SimpleQueue (x:xs)) = (SimpleQueue xs, x)

state_queue_push :: StateQueue -> State -> StateQueue
state_queue_push (SimpleQueue states) state = SimpleQueue (states ++ [state])

search_iteration :: (SearchState, StateQueue) -> (SearchState, StateQueue)
search_iteration (search_state, state_queue) = let (new_state_queue, badger) = state_queue_get state_queue
                                                   new_branches = branches badger
                                                   path_so_far = fromJust (Map.lookup badger search_state)
                                                   graft_branches_to_search_state (ss, sq) ((direction, new_state):bs) = case new_state `Map.member` ss of
                                                                                                                             True -> graft_branches_to_search_state (ss, sq) bs
                                                                                                                             False -> graft_branches_to_search_state ((Map.insert new_state (path_so_far ++ [direction]) ss), state_queue_push sq new_state) bs

                                                   graft_branches_to_search_state (ss, sq) [] = (ss, sq)
                                               in graft_branches_to_search_state (search_state, new_state_queue) new_branches


search :: State -> (SearchState, StateQueue) -> SearchState
search solution (search_state, _) | solution `Map.member` search_state = search_state
search solution (search_state, x) = search solution (search_iteration (search_state, x))

solve :: State -> State -> SearchState
solve initial_state solution = let initial_search_state = Map.fromList [(initial_state, [])]
                                in search solution (initial_search_state, start_state_queue initial_state)

solution_path :: State -> SearchState -> [Direction]
solution_path solution search_state = fromJust (Map.lookup solution search_state)

solve_path :: State -> State -> [Direction]
solve_path initial_state solution = solution_path solution (solve initial_state solution)

-- example1 = array ((0,0), (2,2)) [((x,y), [1,4,2,3,0,5,6,7,8]!!(x+3*y)) | x <- [0..2], y <- [0..2]] -- This is (Int, Int)
example1 = array ((0,0), (2,2)) [((x,y), [1,4,2,3,0,5,6,7,8]!!(fromInteger (x+3*y))) | x <- [0..2], y <- [0..2]] -- This is Integer Integer


solution_3puzzle = array ((0,0),(1,1)) [((x,y), 2*x+y) | x <- [0..1], y <- [0..1]]
solution_8puzzle = array ((0,0),(2,2)) [((x,y), 3*x+y) | x <- [0..2], y <- [0..2]]
example2 = array ((0,0),(2,2)) [((x,y), 3*y + x) | x <- [0..2], y <- [0..2]]
example3 = snd((branches solution_8puzzle)!!0)
example4 = snd((branches example3)!!0)
example5 = snd((branches example4)!!1)
example6 = snd((branches example5)!!0)

example_3p_1 = array ((0,0),(1,1)) [((x,y), 2*y+x) | x <- [0..1], y <- [0..1]]


-- 3puzzle can't be arbitrarily rearranged - not enough degrees of freedom - example3p1 can't be shifted into solution3puzzle
--
-- *Main> solution_3puzzle
-- array ((0,0),(1,1)) [((0,0),0),((0,1),1),((1,0),2),((1,1),3)]
--
-- *Main> (search_iteration(search_iteration(search_iteration(search_iteration(search_iteration(search_iteration(search_iteration(search_iteration(search_iteration(search_iteration(search_iteration (search_iteration (initial_search_state,[example_3p_1])))))))))))))
-- (fromList [
-- (array ((0,0),(1,1)) [((0,0),0),((0,1),1),((1,0),3),((1,1),2)],[Right,Up,Left,Down]),
-- (array ((0,0),(1,1)) [((0,0),0),((0,1),2),((1,0),1),((1,1),3)],[]),
-- (array ((0,0),(1,1)) [((0,0),0),((0,1),3),((1,0),2),((1,1),1)],[Up,Right,Down,Left]),
-- (array ((0,0),(1,1)) [((0,0),1),((0,1),0),((1,0),3),((1,1),2)],[Right,Up,Left]),
-- (array ((0,0),(1,1)) [((0,0),1),((0,1),2),((1,0),0),((1,1),3)],[Right]),
-- (array ((0,0),(1,1)) [((0,0),1),((0,1),2),((1,0),3),((1,1),0)],[Right,Up]),
-- (array ((0,0),(1,1)) [((0,0),2),((0,1),0),((1,0),1),((1,1),3)],[Up]),
-- (array ((0,0),(1,1)) [((0,0),2),((0,1),3),((1,0),0),((1,1),1)],[Up,Right,Down]),
-- (array ((0,0),(1,1)) [((0,0),2),((0,1),3),((1,0),1),((1,1),0)],[Up,Right]),
-- (array ((0,0),(1,1)) [((0,0),3),((0,1),0),((1,0),2),((1,1),1)],[Up,Right,Down,Left,Up]),
-- (array ((0,0),(1,1)) [((0,0),3),((0,1),1),((1,0),0),((1,1),2)],[Right,Up,Left,Down,Right]),
-- (array ((0,0),(1,1)) [((0,0),3),((0,1),1),((1,0),2),((1,1),0)],[Up,Right,Down,Left,Up,Right])],[])
