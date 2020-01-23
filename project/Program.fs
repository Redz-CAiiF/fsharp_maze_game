module FMaze.Program
open FMaze.Core
open globals

//to do
//A* to resolve the maze
//printing the maze with the highlited solution path

[<EntryPoint>]
let main argv =
    //old printing functions are used for printing the maze. TODO printing functions with the given GUI Engine library.    
    let map = Maze.create ROWS COLUMNS
    let expanded_map = Maze.Expand.convert_maze_to_expandedmaze map

    printing.print_map expanded_map
    printfn "%A" expanded_map
    
    let resolution = Maze.Resolutor.resolve expanded_map

    printfn "%A" resolution

    System.Console.ReadKey() |> ignore
    0 // return exit code
