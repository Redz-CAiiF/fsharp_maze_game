module maze

open globals
open general
open map
open maze_generator
open maze_resolutor



// ┌─────────────────────────────────────────────────────────────────────────┐
// │                                FUNCTION 
// │    NAME:          connect
// │    DESCRIPTION:   connette due mappe di cell assieme
// │    CREATOR:       ML
// │    OLD NAME:      connect_default
// │
// └─────────────────────────────────────────────────────────────────────────┘
let connect (maze1:cell list) (maze2:cell list) =

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

    let map_h = ((get_sizes maze1 MAP_TYPE) |> get_map_height)
    let map_w = ((get_sizes maze1 MAP_TYPE) |> get_map_width)
    //conect the two mazes
    let unp_map = aux map_h maze1 maze2
    //create a path between the two mazes
    let p_map = open_path map_h map_w unp_map
    //game_map_row_fix map_w p_map 0
    p_map

// ┌─────────────────────────────────────────────────────────────────────────┐
// │                                FUNCTION 
// │    NAME:          generate_maze
// │    DESCRIPTION:   chiama recursive_backtracker con la mappa di cell passata come parametro e con la cella (0,0,..) come prima cella
// │    CREATOR:       ML
// │    OLD NAME:      .
// │
// └─────────────────────────────────────────────────────────────────────────┘
let stopWatch = System.Diagnostics.Stopwatch.StartNew()
let generate_maze (map:cell list) =
    stopWatch.Restart()
    let res = recursive_backtracker map map.[0]
    //let res = sidewinder_algorithm map
    stopWatch.Stop()
    printfn "GENERATION time: %A" stopWatch.Elapsed.TotalMilliseconds
    res
    


// ┌─────────────────────────────────────────────────────────────────────────┐
// │                                FUNCTION 
// │    NAME:          disconnect
// │    DESCRIPTION:   elimina la prime height righe alla mappa fornita, chiudendo il muro sovrastante, 
// │                    rimuovere delle righe può portare il labitinto a non avere souzioni
// │    CREATOR:       ML
// │    OLD NAME:      disconnect_default
// │
// └─────────────────────────────────────────────────────────────────────────┘
let disconnect (height:int) (maze:cell list) = //height <=maze height
    let rec remove_cells (cells_number:int) (maze:cell list) = //cells_number <=maze cells
        match cells_number with
        | 0 -> maze
        | _ -> let _::maze_tail = maze in remove_cells (cells_number-1) maze_tail
    
    //maze_updated should start from [] when the function is called for the first time
    let rec reduce_indexes (n:int) (maze_updated:cell list) (maze_old:cell list) = 
        match maze_old with
        | [] -> maze_updated
        | (x,y,w,v)::maze_old_tail -> reduce_indexes n (maze_updated@[((x-n),y,w,v)]) maze_old_tail

    let maze_w = ((get_sizes maze MAP_TYPE) |> get_map_width)
    let cells_number = height*maze_w

    let rec close_top_wall (cells_number:int) (maze:cell list) = //cells_number <=maze cells
        match cells_number with
        | 0 -> maze
        | _ -> let (x,y,(tw,rw,bw,lw),visited)::maze_tail = maze in (x,y,(true,rw,bw,lw),visited)::(close_top_wall (cells_number-1) maze_tail)

    close_top_wall maze_w (reduce_indexes height [] (remove_cells cells_number maze))



///automatize the generate, it should only take rows and cols and it should choose by itself the chunks
let rec generate rows cols chunk =
    match chunk with
    | 1 -> (generate_maze (generate_map rows cols))
    | _ -> (connect (generate rows cols (chunk-1)) (generate_maze (generate_map rows cols)))
