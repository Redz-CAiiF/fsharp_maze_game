//maze generation algorithm module
module maze_generator

open System
open globals
open general
open map


// ┌─────────────────────────────────────────────────────────────────────────┐
// │                                CONSTANT 
// │    NAME:          SEED
// │    DESCRIPTION:   avvia un generatore random
// │    CREATOR:       ML
// │    OLD NAME:      .
// │
// └─────────────────────────────────────────────────────────────────────────┘
let SEED = System.Random()

// ┌─────────────────────────────────────────────────────────────────────────┐
// │                                FUNCTION 
// │    NAME:          is_explored
// │    DESCRIPTION:   data una mappa, ritorna true se la mappa è completamante esplorata
// │    CREATOR:       ML
// │    OLD NAME:      .
// │
// └─────────────────────────────────────────────────────────────────────────┘
let rec is_explored (map:cell list) =
    match map with
    | [] -> true
    | (x:int,y:int,walls:walls,visited:bool)::tail -> if visited then is_explored(tail) else false

// ┌─────────────────────────────────────────────────────────────────────────┐
// │                                FUNCTION 
// │    NAME:          get_unvisited_neighbours
// │    DESCRIPTION:   dati la cella corrente e la mappa, trova e ritorna tutti i vicini non visitati della cella corrente
// │    CREATOR:       ML
// │    OLD NAME:      .
// │
// └─────────────────────────────────────────────────────────────────────────┘
let get_unvisited_neighbours (cell:cell) (map:cell list) =
    let map_h = ((get_sizes map MAP_TYPE) |> get_map_height)
    let map_w = ((get_sizes map MAP_TYPE) |> get_map_width)
    let ind = index_general map_h map_w
    //current cell separated variables
    let x,y,_,visited = cell

    let get_cell = fun (index:int) -> try map.[index] with _ -> ERROR_CELL
    //neighbours cells
    let top    = get_cell (ind (x-1) y)
    let right  = get_cell (ind x (y+1))
    let bottom = get_cell (ind (x+1) y)
    let left   = get_cell (ind x (y-1))

    let is_visited = fun (cell:cell) -> let _,_,_,visited = cell in visited
    let add_neighbour = fun (cell:cell) -> if cell <> ERROR_CELL && not (is_visited cell) then [cell] else []
    //neighbours array, with only existing and not visited neighbours
    let neighbours = (add_neighbour top)@(add_neighbour right)@(add_neighbour bottom)@(add_neighbour left)
    neighbours

// ┌─────────────────────────────────────────────────────────────────────────┐
// │                                FUNCTION 
// │    NAME:          unvisited_neighbours_number
// │    DESCRIPTION:   dati la cella corrente e la mappa, ritorna il numero dei vicini non visitati
// │    CREATOR:       ML
// │    OLD NAME:      .
// │
// └─────────────────────────────────────────────────────────────────────────┘
let unvisited_neighbours_number (cell:cell) (map:cell list) =
    let neighbours = get_unvisited_neighbours cell map
    neighbours.Length

// ┌─────────────────────────────────────────────────────────────────────────┐
// │                                FUNCTION 
// │    NAME:          has_unvisited_neighbours
// │    DESCRIPTION:   dati la cella corrente e la mappa, ritorna true se la cella corrente ha dei vicini non visitati altrimenti false
// │    CREATOR:       ML
// │    OLD NAME:      .
// │
// └─────────────────────────────────────────────────────────────────────────┘
let has_unvisited_neighbours (cell:cell) (map:cell list) =
    let neighbours = get_unvisited_neighbours cell map
    if neighbours.Length <> 0 then true else false

// ┌─────────────────────────────────────────────────────────────────────────┐
// │                                FUNCTION 
// │    NAME:          get_random_unvisited_neighbour
// │    DESCRIPTION:   dati la cella corrente e la mappa, ritorna un vicino casuale tra quelli non visitati
// │    CREATOR:       ML
// │    OLD NAME:      .
// │
// └─────────────────────────────────────────────────────────────────────────┘
let get_random_unvisited_neighbour (cell:cell) (map:cell list) =
    let neighbours = get_unvisited_neighbours cell map
    let random_index = SEED.Next(0, neighbours.Length)
    let random_neighbour = neighbours.[random_index]
    random_neighbour


//remove the connected wall
// ┌─────────────────────────────────────────────────────────────────────────┐
// │                                FUNCTION 
// │    NAME:          remove_common_wall
// │    DESCRIPTION:   data la cella corrente e quella successiva, rimuove il muro adiacente alle due celle
// │    CREATOR:       ML
// │    OLD NAME:      .
// │
// └─────────────────────────────────────────────────────────────────────────┘
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




// ┌─────────────────────────────────────────────────────────────────────────┐
// │                                ALGORITHM
// │    NAME:          recursive_backtracker
// │    DESCRIPTION:   .
// │    SOURCE:        https://en.wikipedia.org/wiki/Maze_generation_algorithm#Recursive_backtracker
// │
// └─────────────────────────────────────────────────────────────────────────┘
// ┌─────────────────────────────────────────────────────────────────────────┐
// │                                FUNCTION 
// │    NAME:          recursive_backtracker
// │    DESCRIPTION:   genera un labirinto usando l'algoritmo recursive backtracker
// │    CREATOR:       ML
// │    OLD NAME:      .
// │
// └─────────────────────────────────────────────────────────────────────────┘
let recursive_backtracker (map:cell list) (current:cell) = 
    let rec aux (stack:cell list) (map:cell list) (current:cell) =
        let map = replace_cell (find_cell current map) (set_visited current) map
        let current = (set_visited current)

        if not (is_explored map) then
            let all_unvisited_neighbours = get_unvisited_neighbours current map
            let unvisited_neighbours = all_unvisited_neighbours.Length
            if (unvisited_neighbours) > 0 then
                let next = all_unvisited_neighbours.[SEED.Next(0,unvisited_neighbours)]
                
                let new_current,new_next = remove_common_wall current next
                let new_stack = if unvisited_neighbours > 1 then new_current::stack else stack
                let new_map = (replace_cell (find_cell current map) new_current map)
                let new_map = replace_cell (find_cell next map) new_next new_map

                aux new_stack new_map new_next

            else // ci arriverà solo se lo stack è vuoto
                let next::new_stack = stack
                aux new_stack map (set_visited next)
            
        else
            map

    aux [] map current

///non ottengo nessun vantaggio
let recursive_backtracker_imper (map:cell list) (initial:cell) = 
    let aux (map:cell list) (initial:cell) =
        let init = (set_visited initial)
        let mutable stack = [init]
        let mutable map = replace_cell (find_cell initial map) init map
        //printfn "evaluating: %A" current

        while stack.Length <> 0 do
            let mutable current::n_stack = stack
            stack <- n_stack

            let unvisited_neighbours = get_unvisited_neighbours current map
            let unvisited_neighbours_number = unvisited_neighbours.Length
            if unvisited_neighbours_number > 0 then
                
                //get random neighbour cell
                let mutable next = unvisited_neighbours.[SEED.Next(0,unvisited_neighbours_number)]
                //remove common wall
                let ncurrent,nnext = (remove_common_wall current next)
                
                map <- (replace_cell (find_cell current map) (set_visited ncurrent) map)
                map <- replace_cell (find_cell next map) (set_visited nnext) map

                current <- set_visited ncurrent
                next <- set_visited nnext
                stack <- if unvisited_neighbours_number > 1 then next::current::stack else next::stack

        map
    aux map initial


// ┌─────────────────────────────────────────────────────────────────────────┐
// │                                ALGORITHM
// │    NAME:          sidewinder_algorithm 
// │    DESCRIPTION:   It allows arbitrarily tall mazes. It’s closely related to the Binary Tree algorithm.
// │                   The currently used algorithm is based on the sourced one
// │    SOURCE:        http://weblog.jamisbuck.org/2011/2/3/maze-generation-sidewinder-algorithm
// │
// └─────────────────────────────────────────────────────────────────────────┘
// ┌─────────────────────────────────────────────────────────────────────────┐
// │                                FUNCTION 
// │    NAME:          sidewinder_algorithm 
// │    DESCRIPTION:   genera un labirinto usando l'algoritmo sidewinder maze
// │    CREATOR:       ML
// │    OLD NAME:      .
// │
// └─────────────────────────────────────────────────────────────────────────┘
let sidewinder_algorithm (map:cell list) =

    let get_random_cell_in_set (set:cell list) = 
        let r = SEED.Next(0, set.Length)
        set.[r]

    let TOP = 1
    let RIGHT = 2
    let BOTTOM = 3
    let LEFT = 4

    let carve (wall:int) ((x,y,(w1,w2,w3,w4),v):cell) = 
        if wall = TOP then (x,y,(false,w2,w3,w4),v)
        elif wall = RIGHT then (x,y,(w1,false,w3,w4),v)
        elif wall = BOTTOM then (x,y,(w1,w2,false,w4),v)
        else (x,y,(w1,w2,w3,false),v)

    let rec generate_row (set:cell list) (map_row:cell list) (current:cell) =
        /// function used to chose if the next cell is going to be carved
        let carving = if SEED.Next(0, 2) = 0 then true else false

        //let new_set =  // da sistemare perche i muri non saranno aggiornati
        let (_,position,walls,v) = current
        let next_exitance = ((position+1)<map_row.Length)

        let n_set = set@[current]

        if next_exitance then
            if carving then //carve east
                //ottengo la prossima cella
                let next = map_row.[(position+1)]
                //apro il muro che connette la cella corrente con quella successiva
                let carved_c,carved_n = remove_common_wall current next
                //sostituisco le vecchie celle con quelle nuove
                let u_map_row = replace_cell position carved_c map_row
                let u_map_row = replace_cell (position+1) carved_n u_map_row
                //richiamo la funzione con il nuovo set
                generate_row (n_set) u_map_row carved_n
                
                
            else //dont carve
                //randomly choose one of the cells in the run set
                let (_,y,_,_) = get_random_cell_in_set n_set
                //carve the random cell top wall
                let carved_c = carve TOP map_row.[(y)]
                //set the next cell to be the new current
                let next = map_row.[(position+1)]
                //update the map
                let u_map_row = replace_cell y carved_c map_row
                //richiama la funzione con set vuoto
                generate_row [] u_map_row next
        else
            //randomly choose one of the cells in the run set
            let (_,y,_,_) = get_random_cell_in_set n_set
            //carve the random cell top wall
            let carved_c = carve TOP map_row.[(y)]
            //update the map
            let u_map_row = replace_cell y carved_c map_row
            //return u_map_row
            (u_map_row:cell list)

    //fix the walls on the maze
    let rec game_map_row_fix (cols:int) (map:cell list) (current:int) = 
        if (current+cols) < map.Length then
            let (cx,cy,(cw1,cw2,cw3,cw4),cv) = map.[current]
            let (_,_,(bw1,_,_,_),_) = map.[cols+current]
            let n_map = replace_cell current (cx,cy,(cw1,cw2,bw1,cw4),cv) map
            game_map_row_fix cols n_map (current+1)
        else map
    
    //fix the top map wall on the maze
    let rec game_map_top_fix (cols:int) (map:cell list) (current:int) = 
        if (current) < cols then
            let (cx,cy,(cw1,cw2,cw3,cw4),cv) = map.[current]
            let n_map = replace_cell current (cx,cy,(true,false,cw3,cw4),cv) map
            game_map_top_fix cols n_map (current+1)
        else map

    let rec generate_map (cols:int) (gen_map:cell list) (map:cell list) =
        match map with
        | [] -> game_map_top_fix cols (game_map_row_fix cols gen_map 0) 0
        | _ -> generate_map cols (gen_map@(generate_row [] map.[..(cols-1)] map.[0])) (map.[(cols)..])


    let map_w = ((get_sizes map MAP_TYPE) |> get_map_width)
    let par_gen = generate_map map_w [] map
    //apro dei muri tra le varie zone separate
    par_gen
