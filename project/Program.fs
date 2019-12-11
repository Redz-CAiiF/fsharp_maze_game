module FMaze.Program
open FMaze.Core
//to do
//A* to resolve the maze
//printing the maze with the highlited solution path

let ROWS = 20
let COLS = 20

[<EntryPoint>]
let main argv =
    //old printing functions are used for printing the maze. TODO printing functions with the given GUI Engine library.    
    let map = Maze.create ROWS COLS
    let maze_tot= printing.convert_celltype_to_cell_list  map.map map.cols

    let chars_map = printing.map_to_chars maze_tot map
    
    printing.print_map (printing.improve_output_map chars_map)

    System.Console.ReadKey() |> ignore
    0 // return exit code



