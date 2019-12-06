open System



open globals
open general
open printing
open map
open maze


//to do
//new printing function that doesnt require an expanded_map
//A* to resolve the maze
//printing the maze with the highlited solution path


[<EntryPoint>]
let main argv =

    set_colour COLOUR_PLAYER COLOUR_BACKGROUND
    let maze_tot = generate ROWS COLUMNS CHUNKS
    reset_colour

    //let custom_index = (index_general (((get_sizes maze_tot MAP_TYPE) |> get_map_height)) (((get_sizes maze_tot MAP_TYPE) |> get_map_width)))
    //print_debug (((get_sizes maze_tot MAP_TYPE) |> get_map_height)) (((get_sizes maze_tot MAP_TYPE) |> get_map_width)) custom_index maze_tot
    
    let maze_tot_exp = expand maze_tot

    //let custom_index = (index_general (((get_sizes maze_tot_exp MAP_EXPANDED_TYPE) |> get_map_height)) (((get_sizes maze_tot_exp MAP_EXPANDED_TYPE) |> get_map_width)))
    //print_debug (((get_sizes maze_tot_exp MAP_EXPANDED_TYPE) |> get_map_height)) (((get_sizes maze_tot_exp MAP_EXPANDED_TYPE) |> get_map_width)) custom_index maze_tot_exp

    print_map SET_THIN maze_tot_exp
    //print_map SET_THICK maze_tot_exp
    print_map_generic maze_tot_exp

    //let maze_tot_exp_deloaded = expand (disconnect ROWS maze_tot)

    //print_map SET_THIN maze_tot_exp_deloaded

    printfn "%A" (map_to_chars EXPANDED_ROWS EXPANDED_COLUMNS maze_tot)

    System.Console.ReadKey() |> ignore
    0 // return exit code



