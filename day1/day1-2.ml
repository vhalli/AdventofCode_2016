#load "str.cma"

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

let rec verbosestep pos dist : position list =
  if dist > 0 then
    step pos dist :: verbosestep pos (dist-1)
  else
    []

let walk pos (dirn, dist) : position list =
  let newhdg = turn dirn pos.heading in
  verbosestep {pos with heading = newhdg} dist

let verbosewalk poslist inst : position list =
  let pos = List.hd poslist in
  walk pos inst @ poslist

let findHQ instructions : position =
  List.fold_left walk initial_pos instructions

let distance_from_origin pos : int =
  abs pos.x + abs pos.y

let getinstructions filename : instruction list =
  let rawinput = input_line (open_in filename) in
  let input = Str.split (Str.regexp ", ") rawinput in
  let parseint rawinst : int = 
    int_of_string (String.sub rawinst 1 ((String.length rawinst)-1)) in
  let str_to_inst rawinst : instruction =
    match rawinst.[0] with
    | 'R' -> (Right, parseint rawinst)
    | 'L' -> (Left, parseint rawinst)
    | _ -> raise (Invalid_argument "Unexpected character")
  in
  List.map str_to_inst input

let d = distance_from_origin (findHQ (getinstructions "in.txt"));;
print_int d
