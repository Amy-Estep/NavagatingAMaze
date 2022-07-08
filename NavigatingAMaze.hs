data Orientation = H | V
 deriving (Show,Eq)
type Wall = (Int, Int, Orientation)
type Maze = ((Int,Int),[Wall])

data Pos 
    = Pos Int Int
    deriving (Eq, Show)

exampleMaze :: Maze
exampleMaze = ((4,4), hWalls ++ vWalls)
  where
  vWalls = map (\ (i,j) -> (i,j,V))[(0,0),(0,1),(0,2),(0,3),(1,1),(1,2),(2,1),(2,2),(3,2), (3,3),(4,0),(4,1),(4,2)]
  hWalls = map (\ (i,j) -> (i,j,H))[(0,0),(1,0),(2,0),(3,0),(0,1),(2,1),(2,2),(0,4),(1,4),(2,4),(3,4)]

exampleMazeNoExit :: Maze
exampleMazeNoExit = ((4,4), hWalls ++ vWalls)
  where
  vWalls = map (\ (i,j) -> (i,j,V))[(0,0),(0,1),(0,2),(0,3),(1,1),(1,2),(2,1),(2,2),(3,2), (3,3),(4,0),(4,1),(4,2),
    (4,3)]
  hWalls = map (\ (i,j) -> (i,j,H))[(0,0),(1,0),(2,0),(3,0),(0,1),(2,1),(2,2),(0,4),(1,4),(2,4),(3,4)]

getDirectionsOut :: Maze -> Maybe [Direction]
getDirectionsOut m = 
  if (isExit m) 
  then Just (tryGoLeft m (0,0) (0,0) North)
  else
  Nothing
  
-- determines if given maze has an exit
isExit :: Maze -> Bool
isExit ((width,height), ws) =
  if (length ([(x,y,o)| (x,y,o)<- (listOfVerWalls ws), x == 0 || x == width])) == (2*height) 
    && (length ([(x,y,o)| (x,y,o)<- (listOfHorWalls ws), y == 0 || y == height])) == (2*width)
  then False
  else True

directions = []
-- function that goes left if path is blocked or at an intersection otherwise moves straight on
tryGoLeft :: Maze -> (Int,Int) -> (Int,Int) -> Direction -> [Direction]
tryGoLeft ((width,height), ws) (previousx,previousy) (x,y) d = 
  if (isNotPosInMaze (width,height)(x,y)==False)
    then
     {- this checks if the path is clear in the current direction and that the next coordinate is not equal to the
     previous coordinate unless we are currently at a dead end, and that the next coordinate is an intersection
     that you can turn left at-}
     if ( isPathClear ((width,height), ws) (x,y) (d) ) && ( ((followDirection (x,y) d) /= (previousx,previousy)) || 
      (isDeadEnd ((width,height), ws) (x,y)) ) &&  ((isIntersection exampleMaze (followDirection (x,y) d) d) == True)
      && ((isPathClear ((width,height), ws) (followDirection (x,y) (d)) (leftDirection d)) == True)
     then do
       {- adds the current direction to the list to output and then reruns the function with the new coordinate
       and direction-}
       directions  ++ [d] ++ tryGoLeft ((width,height), ws) (x,y) (followDirection (x,y) (d)) (leftDirection d) 
     else
       {-this is the same as the first if statement but we cannot turn left at the next coordinates intersection-}
       if ( isPathClear ((width,height), ws) (x,y) (d) ) && ( ((followDirection (x,y) d) /= (previousx,previousy)) ||
         (isDeadEnd ((width,height), ws) (x,y)) ) &&  ((isIntersection exampleMaze (followDirection (x,y) d) d) == True)
         && ((isPathClear ((width,height), ws) (followDirection (x,y) (d)) (leftDirection d)) == False)
      then
        {-adds the current direction to the list to output and then reruns the function with the new coordinate and
        current direction-}
        directions ++ [d] ++ tryGoLeft ((width,height), ws) (x,y) (followDirection (x,y) (d)) (d)
      else
        {-this checks if the path is clear in the current direction and that the next coordinate is not equal to
        the previous coordinate unless we are currently at a dead end, and that the next coordinate is not an 
        intersection-}
        if isPathClear ((width,height), ws) (x,y) (d)  && ( (followDirection (x,y) d) /= (previousx,previousy) ||
          isDeadEnd ((width,height), ws) (x,y) ) &&  ((isIntersection exampleMaze (followDirection (x,y) d) d) == False)
        then do
          {-adds the current direction to the list to output and then reruns the function with the new coordinate and
          current direction-}
          directions ++ [d] ++ ( tryGoLeft ((width,height), ws) (x,y) (followDirection (x,y) (d)) (d))
        else
          if ((isNotPosInMaze (width,height) (x,y)) == False)
          then do
            {-reruns the function with the same coordinate
            and new left direction-}
            tryGoLeft ((width,height), ws) (previousx,previousy) (x,y) (leftDirection d)
          else   directions
  else  directions

-- all 5 variations of intersections, returns true if coordinate is an intersection
isIntersection :: Maze -> (Int,Int) -> Direction -> Bool
isIntersection ((width,height), ws) (x,y) d = 
  ( ((( x+1,y,V) `elem` listOfVerWalls ws)  == False) && ((( x,y+1,H) `elem` listOfHorWalls ws) == False) &&
  (( x,y,V) `elem` listOfVerWalls ws) == False )
  || ( ((( x,y,V) `elem` listOfVerWalls ws)  == False) && ((( x,y,H) `elem` listOfHorWalls ws) == False) &&
  (( x+1,y,V) `elem` listOfVerWalls ws) == False  )
  || ( ((( x,y,V) `elem` listOfVerWalls ws)  == False) && ((( x,y,H) `elem` listOfHorWalls ws) == False) &&
  (( x,y+1,H) `elem` listOfHorWalls ws) == False )
  || ( ((( x,y,H) `elem` listOfHorWalls ws)  == False) && ((( x,y+1,H) `elem` listOfHorWalls ws) == False) &&
  (( x+1,y,V) `elem` listOfVerWalls ws) == False)
  || ( ((( x,y,H) `elem` listOfHorWalls ws)  == False) && ((( x,y,V) `elem` listOfVerWalls ws) == False) &&
  (( x+1,y,V) `elem` listOfVerWalls ws) == False && (( x,y+1,H) `elem` listOfHorWalls ws) == False)  

-- all 4 variations of dead ends, returns true if coodinate is a dead end
isDeadEnd :: Maze -> (Int,Int) -> Bool
isDeadEnd ((width,height), ws) (x,y) = 
  ((( x,y,H) `elem` listOfHorWalls ws) && ((x,y,V) `elem` listOfVerWalls ws) && ((x+1,y,V) `elem` listOfVerWalls ws) )
  || ((( x,y,H) `elem` listOfHorWalls ws) && ((x,y,V) `elem` listOfVerWalls ws) && ((x,y+1,H) `elem` listOfHorWalls ws))
  || ((( x,y,H) `elem` listOfHorWalls ws) && ((x+1,y,V) `elem` listOfVerWalls ws) && ((x,y+1,H) `elem` listOfHorWalls ws))
  || ((( x,y+1,H) `elem` listOfHorWalls ws) && ((x,y,V) `elem` listOfVerWalls ws) && ((x+1,y,V) `elem` listOfVerWalls ws))

-- direction to the left of the direction we are facing
leftDirection :: Direction -> Direction
leftDirection North = West
leftDirection West = South
leftDirection South = East
leftDirection East = North

-- checks if we were to move in the given direction that the path is clear by by checking if the new
-- coordinate is in the list of horizontal or vertical walls depending on the direction
-- moving north and south is dependent on vertical walls
-- moving east and west is dependent on horizontal walls
-- important ot note that moving south and west is dependent on the horizontal and vertical walls at the current coordinate
-- not the next
isPathClear :: Maze -> (Int,Int) -> Direction -> Bool
isPathClear ((width,height), ws) (x,y) d
  | d == North && (((x,(y+1),H) `elem` (listOfHorWalls ws)) == True)= False
  | d == South && (( (x,(y), H) `elem` (listOfHorWalls ws)) == True ) = False
  | d == East && ( (((x+1),y, V) `elem` (listOfVerWalls ws)) == True ) = False 
  | d == West && ( (((x),y,V) `elem` (listOfVerWalls ws)) == True ) = False
  | otherwise = True

-- tests that listOfHorWalls and listOfVerWalls functions work
testListWalls :: Maze -> [Wall]
testListWalls ((width,height), ws) = listOfVerWalls ws

-- takes the concatinated list of horzontal and vertical walls  and returns just the horizontal walls
listOfHorWalls :: [Wall] -> [Wall]
listOfHorWalls ws = [ (x,y,o) | (x,y,o) <- ws, o == H] 

-- takes the concatinated list of horzontal and vertical walls  and returns just the vertical walls
listOfVerWalls :: [Wall] -> [Wall]
listOfVerWalls ws = [ (x,y,o) | (x,y,o) <- ws, o == V] 

-- function returns boolean, if the position is out of the maze then it returns true otherwise it returns false
isNotPosInMaze :: (Int,Int)-> (Int,Int)-> Bool
isNotPosInMaze (width,height) (x,y) = ((x < 0) || (x >= width)) || ((y < 0) || (y >= height))
