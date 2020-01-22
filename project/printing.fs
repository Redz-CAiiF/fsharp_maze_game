module printing
open System
open FMaze.Core
open globals

// ┌─────────────────────────────────────────────────────────────────────────┐
// │                                MODULE 
// │    NAME:          TERMINAL COLORS
// │    DESCRIPTION:   contiene le funzioni per la gestione del colore sul terminale
// │    CREATOR:       ML      
// │
// └─────────────────────────────────────────────────────────────────────────┘ 
let set_terminal_color fore back = 
    Console.ForegroundColor <- fore
    Console.BackgroundColor <- back

let set_colour fore back = 
    set_terminal_color fore back

let reset_colour = 
    Console.ResetColor()
// ┌─────────────────────────────────────────────────────────────────────────┐
// │                               END MODULE 
// └─────────────────────────────────────────────────────────────────────────┘


//  ┌─────────────────────────────────────────────────────────────────────────┐
//  │                                FUNCTION 
//  │    NAME:          print_map
/// │    DESCRIPTION:   .
//  │    CREATOR:       ML
//  │    OLD NAME:      .
//  │
//  └─────────────────────────────────────────────────────────────────────────┘
let print_map (maze:ExpandedMazeType) = 
    for x in 0..(maze.rows-1) do
        printf "\n"
        for y in 0..(maze.cols-1) do
            let index = Utils.from_bidim_to_monodim maze.rows maze.cols x y
            let char = (if maze.map.[index] = Walls.CLOSED then WALL_CHARACTER else PATH_CHARACTER)
            printf "%c%c" char char
    printf "\n"
