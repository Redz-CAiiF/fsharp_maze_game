module general

open globals

//cell (x:int, y:int, (wall1:bool,wall2:bool,wall3:bool,wall4:bool), visited:bool)
type walls = bool*bool*bool*bool
type cell = (int*int*walls*bool)
type cell_reduced = (int*int*int)
let ERROR_CELL = (-1,-1,(false,false,false,false),false)
let ERROR_CELL_REDUCED = (-1,-1,PATH)

exception ERROR_NEGATIVE_COLUMNS_NUMBER
exception ERROR_NEGATIVE_ROWS_NUMBER
exception ERROR_NEGATIVE_INDEX_NUMBER

// general function that is used to transform 2D cordinates in a 1D cordinate
let index_general = fun (rows:int) (cols:int) (x:int) (y:int) -> if (x < 0 || y < 0 || y > cols-1 || x > rows-1) then -1 else y + x * cols

let extend_size (size:int) = size*2+1

///BAD PRACTISE FOR GENERALIZATION
//let index_expanded = index_general expanded_ROWS expanded_COLUMNS
//let index = index_general ROWS COLUMNS

let get_map_height (map:cell_reduced list) =
    let (l_x,_,_)::_ = List.rev map
    l_x+1

let get_map_width (map:cell_reduced list) =
    let (_,l_y,_)::_ = List.rev map
    l_y+1

let get_maze_height (map:cell list) =
    let (l_x,_,_,_)::_ = List.rev map
    l_x+1

let get_maze_width (map:cell list) =
    let (_,l_y,_,_)::_ = List.rev map
    l_y+1


