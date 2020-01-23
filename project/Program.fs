module FMaze.Program
open FMaze.Core
open globals

//to do
//A* to resolve the maze
//printing the maze with the highlited solution path

[<EntryPoint>]
let main argv =
    //old printing functions are used for printing the maze. TODO printing functions with the given GUI Engine library.    
    let maze = Maze.create ROWS COLUMNS
    let expanded_maze = Maze.expand maze
    
    printing.print_map expanded_maze
    printfn "\n%A" expanded_maze

    let solution = Maze.solve expanded_maze

    printfn "\n%A" solution.path

    System.Console.ReadKey() |> ignore
    0 // return exit code
