type heading = North | South | East | West
type direction = Left | Right
type instruction = direction * int
type position = {heading : heading;
                 x : int;
                 y : int}

let initial_pos = {heading = North;
                   x = 0;
                   y = 0}

let turn dirn hdg : heading = 
  match dirn with
  | Right -> (match hdg with
      | North -> East
      | East -> South
      | South -> West
      | West -> North)
  | Left -> (match hdg with
      | North -> West
      | West -> South
      | South -> East
      | East -> North)

let step pos dist : position = 
  match pos.heading with
  | North -> {pos with y = pos.y + dist}
  | South -> {pos with y = pos.y - dist}
  | East -> {pos with x = pos.x + dist}
  | West -> {pos with x = pos.x - dist}

let walk pos (dirn, dist) : position =
  let newhdg = turn dirn pos.heading in
  step {pos with heading = newhdg} dist

let findHQ instructions =
  List.fold_left walk initial_pos instructions

let distance_from_origin pos : int =
  abs pos.x + abs pos.y
