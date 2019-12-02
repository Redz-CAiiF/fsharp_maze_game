//maze generation algorithm module
module maze_generator_algorithm

open System
open globals
open general
open game_map

open printing

// return true if the map is completely explored
let rec is_explored (map:cell list) =
    match map with
    | [] -> true
    | (x:int,y:int,walls:walls,visited:bool)::tail -> if visited then is_explored(tail) else false


//find and return all not visited neighbours of the current cell
let get_unvisited_neighbours (cell:cell) (map:cell list) =
    let map_h = (get_maze_height map)
    let map_w = (get_maze_width map)
    let ind = index_general map_h map_w
    //current cell separated variables
    let x,y,_,visited = cell

    let get_cell = fun (index:int) -> try map.[index] with _ -> ERROR_CELL
    //neighbours cells
    let top    = get_cell (ind (x-1) y)/////////////////////////////////////////////////////////
    let right  = get_cell (ind x (y+1))
    let bottom = get_cell (ind (x+1) y)
    let left   = get_cell (ind x (y-1))

    let is_visited = fun (cell:cell) -> let _,_,_,visited = cell in visited
    let add_neighbour = fun (cell:cell) -> if cell <> ERROR_CELL && not (is_visited cell) then [cell] else []
    //neighbours array, with only existing and not visited neighbours
    let neighbours = (add_neighbour top)@(add_neighbour right)@(add_neighbour bottom)@(add_neighbour left)
    neighbours

// return the number of the not visited neighbours
let unvisited_neighbours_number (cell:cell) (map:cell list) =
    let neighbours = get_unvisited_neighbours cell map
    neighbours.Length

// return true if the current cell has a not visited neighbour
let has_unvisited_neighbours (cell:cell) (map:cell list) =
    let neighbours = get_unvisited_neighbours cell map
    if neighbours.Length <> 0 then true else false

// return a random neighbour from all possible neighbours
let get_random_unvisited_neighbour (cell:cell) (map:cell list) =
    let neighbours = get_unvisited_neighbours cell map
    let random_index = System.Random().Next(0, neighbours.Length)
    let random_neighbour = neighbours.[random_index]
    random_neighbour


//remove the connected wall
let remove_common_wall (current:cell) (next:cell) =
    let c_x,c_y,(c_wall1,c_wall2,c_wall3,c_wall4),c_visited = current
    let n_x,n_y,(n_wall1,n_wall2,n_wall3,n_wall4),n_visited = next
    
    let on_same_row = c_x = n_x
    let on_same_column = c_y = n_y

    if on_same_row && (c_y = (n_y-1)) then
        ((c_x,c_y,(c_wall1,false,c_wall3,c_wall4),c_visited),(n_x,n_y,(n_wall1,n_wall2,n_wall3,false),n_visited))
    elif on_same_row && (c_y = (n_y+1)) then
        ((c_x,c_y,(c_wall1,c_wall2,c_wall3,false),c_visited),(n_x,n_y,(n_wall1,false,n_wall3,n_wall4),n_visited))
    elif on_same_column && (c_x = (n_x-1)) then
        ((c_x,c_y,(c_wall1,c_wall2,false,c_wall4),c_visited),(n_x,n_y,(false,n_wall2,n_wall3,n_wall4),n_visited))
    elif on_same_column && (c_x = (n_x+1)) then
        ((c_x,c_y,(false,c_wall2,c_wall3,c_wall4),c_visited),(n_x,n_y,(n_wall1,n_wall2,false,n_wall4),n_visited))
    else
        (current,next)


//maze generator
//exception ERROR_INCORRECT_PROGRAM_LOGIC
let rec recursive_backtracker (stack:cell list) (map:cell list) (current:cell) =
    let map = replace_cell (find_cell current map) (update_visited current) map
    let current = (update_visited current)
    //printfn "evaluating: %A" current
    if not (is_explored map) then
        let unvisited_neighbours = unvisited_neighbours_number current map
        if (unvisited_neighbours) > 0 then
            let next = get_random_unvisited_neighbour current map
            
            let new_current,new_next = remove_common_wall current next
            let new_stack = if unvisited_neighbours > 1 then new_current::stack else stack
            let new_map = replace_cell (find_cell next map) new_next (replace_cell (find_cell current map) new_current map)

            //printfn "current iteration: %A" new_current
            //printfn "current iteration: %A %A %A map:\n %A" next new_stack (new_current,new_next) new_map
            
            recursive_backtracker new_stack new_map new_next

        elif (stack.Length <> 0) then
            //printfn "current iteration on stack: %A" current
            let next::new_stack = stack
            
            recursive_backtracker new_stack map (update_visited next)
            
        else
            recursive_backtracker stack map current
    else
        map


let generate_maze (map:cell list) =
    recursive_backtracker [] map map.[0]



// CONNECT 2 MAZES TOGHETER
let connect_default (maze1:cell list) (maze2:cell list) =

    let rec aux height (maze1:cell list) (maze2:cell list) =
        match maze2 with
        | [] -> maze1
        | (x,y,walls,v)::maze2_tail -> aux height (maze1@[(x+height,y,walls,v)]) maze2_tail
    
    let open_path (height:int) (width:int) (maze:cell list) = 
        //posizione della cella da modificare
        let random_column = (new System.Random()).Next(0,(width))
        //i need to change 2 cells
        let c1 = ((get_cell (index_general (height*2) width (height-1) random_column) maze):cell)
        let c2 = ((get_cell (index_general (height*2) width height random_column) maze):cell)
        let nc1,nc2 = remove_common_wall c1 c2
        let maze = replace_cell (index_general (height*2) width (height-1) random_column) nc1 maze
        let maze = replace_cell (index_general (height*2) width height random_column) nc2 maze
        maze

    let map_h = (get_maze_height maze1)
    let map_w = (get_maze_width maze1)
    //conect the two mazes
    let unp_map = aux map_h maze1 maze2
    //create a path between the two mazes
    let p_map = open_path map_h map_w unp_map
    p_map

let connect_extended (maze1:cell_reduced list) (maze2:cell_reduced list) =

    let rec aux height (maze1:cell_reduced list) (maze2:cell_reduced list) =
        match maze2 with
        | [] -> maze1
        | (x,y,state)::maze2_tail -> if x<>0 then aux height (maze1@[(x+height-1,y,state)]) maze2_tail else aux height maze1 maze2_tail
    
    let open_path (height:int) (width:int) (maze:cell_reduced list) = 
        //posizione della cella da modificare
        let random_column = (((new System.Random()).Next(0, (width-1)/2))*2+1)
        let (x,y,state) = ((height-1),random_column,PATH)
        replace_cell (index_general height width x y) (x,y,state) maze

    let map_h = get_map_height maze1
    let map_w = get_map_width maze1
    //conect the two mazes
    let unp_map = aux map_h maze1 maze2
    //create a path between the two mazes
    let p_map = open_path map_h map_w unp_map
    p_map
