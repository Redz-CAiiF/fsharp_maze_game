module maze

open globals
open general
open map
open maze_generator
open maze_resolutor



// ┌─────────────────────────────────────────────────────────────────────────┐
// │                                FUNCTION 
// │    NAME:          connect_default
// │    DESCRIPTION:   connette due mappe di cell assieme
// │    CREATOR:       ML
// │    OLD NAME:      .
// │
// └─────────────────────────────────────────────────────────────────────────┘
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

    let map_h = ((map_sizes maze1) |> get_map_height)
    let map_w = ((map_sizes maze1) |> get_map_width)
    //conect the two mazes
    let unp_map = aux map_h maze1 maze2
    //create a path between the two mazes
    let p_map = open_path map_h map_w unp_map
    p_map

// ┌─────────────────────────────────────────────────────────────────────────┐
// │                                FUNCTION 
// │    NAME:          connect_extended
// │    DESCRIPTION:   connette due mappe di cell_reduced assieme
// │    CREATOR:       ML
// │    OLD NAME:      .
// │
// └─────────────────────────────────────────────────────────────────────────┘
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

    let map_h = ((map_expanded_sizes maze1) |> get_map_height)
    let map_w = ((map_expanded_sizes maze1) |> get_map_width)
    //conect the two mazes
    let unp_map = aux map_h maze1 maze2
    //create a path between the two mazes
    let p_map = open_path map_h map_w unp_map
    p_map


// ┌─────────────────────────────────────────────────────────────────────────┐
// │                                FUNCTION 
// │    NAME:          generate_maze
// │    DESCRIPTION:   chiama recursive_backtracker con la mappa di cell passata come parametro e con la cella (0,0,..) come prima cella
// │    CREATOR:       ML
// │    OLD NAME:      .
// │
// └─────────────────────────────────────────────────────────────────────────┘
let generate_maze (map:cell list) =
    recursive_backtracker map map.[0]


///disconnect heigth map
//test connect disconnect connect to see how indexes work when they dont start from 0


///automatize the generate, it should only take rows and cols and it should choose by itself the chunks
let rec generate rows cols chunk =
    match chunk with
    | 1 -> (generate_maze (generate_map rows cols))
    | _ -> (connect_default (generate rows cols (chunk-1)) (generate_maze (generate_map rows cols)))
