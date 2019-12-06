open System



open globals
open printing
open maze


//to do
//A* to resolve the maze
//printing the maze with the highlited solution path


[<EntryPoint>]
let main argv =

    set_colour COLOUR_PLAYER COLOUR_BACKGROUND
    let maze_tot = generate ROWS COLUMNS CHUNKS
    reset_colour

    //let custom_index = (index_general (((get_sizes maze_tot MAP_TYPE) |> get_map_height)) (((get_sizes maze_tot MAP_TYPE) |> get_map_width)))
    //print_debug (((get_sizes maze_tot MAP_TYPE) |> get_map_height)) (((get_sizes maze_tot MAP_TYPE) |> get_map_width)) custom_index maze_tot
    
    
    let chars_map = map_to_chars maze_tot

    print_map (improve_output_map chars_map)

    System.Console.ReadKey() |> ignore
    0 // return exit code



