module game_map

open globals
open general

//map generation module
let rec generate_cols (current_row:int) (col_number:int) predicate =
    match col_number with
    | 0 -> []
    | x when x > 0 -> (generate_cols current_row (col_number-1) predicate) @ [(predicate (current_row-1) (col_number-1))]
    | _ -> raise ERROR_NEGATIVE_COLUMNS_NUMBER

let rec generate_rows (rows:int) (cols:int) predicate = 
    match rows with
    | 0 -> []
    | x when x > 0 -> (generate_rows (rows-1) cols predicate) @ (generate_cols x cols predicate)
    | _ -> raise ERROR_NEGATIVE_ROWS_NUMBER

let generate_map (rows:int) (cols:int) =
    let res:cell list = generate_rows rows cols (fun row col -> (row, col, (true,true,true,true), false))
    res

let find_cell (cell:cell) (map:cell list) = 
    let rec find (cell:cell) (position:int) (map:cell list) = 
        match map with
        | [] -> -1
        | x::xs -> (if x <> cell then find cell (position+1) xs else position)
    find cell 0 map

let get_cell (position:int) (map:list<'A>) = 
    map.[position]

let replace_cell (position:int) cell (map:list<'A>) = 
    map.[..(position-1)]@[cell]@map.[(position+1)..]


let update_visited (cell:cell) = 
    let c_x,c_y,c_walls,_ = cell
    (c_x,c_y,c_walls,true)


//MAP EXPANSION functions
let generate_extended_map (rows:int) (cols:int) =
    let res:cell_reduced list = generate_rows rows cols (fun row col -> (row, col, WALL))
    res

let isEven x = (x % 2) = 0
let isOdd x = isEven x = false

let expand (map:cell list) =
    let n_cols = (get_maze_width map)*2+1
    let n_rows = (get_maze_height map)*2+1
    let ext_map = generate_extended_map n_rows n_cols
    let rec set_unconditional_cells = fun map -> 
        match map with
        | [] -> []
        | (x,y,value)::tail ->  (if (x |> isOdd) && (y |> isOdd) then (x,y,PATH) else (x,y,value))::(set_unconditional_cells tail)
    let rec set_walls = fun (map:cell list) (ext_map:cell_reduced list) ->
        //tolgo una cella dalla mappa è modifico ext_map
        match map with
        | [] -> ext_map
        | cell::tail -> (
                        //do the logic here, what to return, how to set the walls on the extended map
                        // use replace_cell on ext_map
                        let x,y,walls,_ = cell
                        let wall_top,wall_right,wall_bottom,wall_left = walls
                        let n_x = 2 * x + 1
                        let n_y = 2 * y + 1
                        //per ogni muro controllo se non c'è, se non c'è modifico la mappa estesa
                        let wall_check = fun wall x y map -> if not wall then replace_cell (index_general n_rows n_cols x y) (x,y,PATH) map else map

                        let ext_map = wall_check wall_top    (n_x-1) (n_y)   ext_map
                        let ext_map = wall_check wall_right  (n_x)   (n_y+1) ext_map
                        let ext_map = wall_check wall_bottom (n_x+1) (n_y)   ext_map
                        let ext_map = wall_check wall_left   (n_x)   (n_y-1) ext_map

                        set_walls tail ext_map
                        )
    
    set_walls map (set_unconditional_cells ext_map)
