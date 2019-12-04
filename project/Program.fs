open System



open globals
open general
open printing
open map
open maze


//to do
//A* to resolve the maze
//printing the maze with the highlited solution path


[<EntryPoint>]
let main argv =

    set_colour COLOUR_PLAYER COLOUR_BACKGROUND
    let maze_tot = generate ROWS COLUMNS 3
    reset_colour

    //let custom_index = (index_general (get_maze_height maze_tot) (get_maze_width maze_tot))
    //print_debug (get_maze_height maze_tot) (get_maze_width maze_tot) custom_index maze_tot
    
    let maze_tot_exp = expand maze_tot

    print_map SET_THIN  maze_tot_exp
    print_map SET_THICK maze_tot_exp
    print_map_generic (((get_sizes maze_tot_exp MAP_EXPANDED_TYPE) |> get_map_height)) (((get_sizes maze_tot_exp MAP_EXPANDED_TYPE) |> get_map_width)) maze_tot_exp

    System.Console.ReadKey() |> ignore
    0 // return exit code
    