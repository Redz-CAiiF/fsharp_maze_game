module printing

open System
open globals
open general
open map


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
//  │    NAME:          map_to_chars
/// │    DESCRIPTION:   data una mappa ritorna una lista di stringhe, che compongono la mappa graficamente
//  │    CREATOR:       ML
//  │    OLD NAME:      .
//  │
//  └─────────────────────────────────────────────────────────────────────────┘
let map_to_chars (map:cell list) = 
    let generate_walls_map (rows:int) (cols:int) = 
        let mutable res = []
        for x in 0..(rows-1) do
            let mutable row = ""
            for y in 0..(cols-1) do
                //se sia x che y sono dispari
                row <- row + (if (x |> isOdd) && (y |> isOdd) then string GENERIC_PATH else string GENERIC_WALL)

            res <- row::res
        res

    let replace_char (x:int) (y:int) wall (res:string list) =
        let line = res.[x]
        let new_line = ((line.[..(y-1)]) + (string (if not wall then GENERIC_PATH else GENERIC_WALL)) + (line.[(y+1)..]))
        let n_res = ((res.[..(x-1)])@[new_line]@(res.[(x+1)..]))
        n_res

    let set_maze (c_map:string list) (map:cell list) = 
        let mutable res = c_map
        let mutable map = map
        for iter in 0..(map.Length-1) do
            //per ogni cella in map prendo gli stati dei muri destra e basso e gli cambio nella mappa dei chars
            let (x,y,(_,w2,w3,_),_)::map_tail = map
            map <- map_tail
            let x = extend_size x
            let y = extend_size y
            res <- replace_char x (y+1) w2 res
            res <- replace_char (x+1) y w3 res

        res

    let rows, cols = (get_sizes map MAP_TYPE)
    set_maze (generate_walls_map rows cols) map
    


// ┌─────────────────────────────────────────────────────────────────────────┐
// │                                 OTHER 
// │    NAME:          FUNZIONI PER MIGLIORARE LA MAPPA DI CHAR IN OUTPUT
// │    DESCRIPTION:   .
// │    CREATOR:       ML
// │
// └─────────────────────────────────────────────────────────────────────────┘
let improve_chars (char:char) = 
    (string char) + (string char)

let rec set_line (row:string) = 
    match row with
    | "" -> ""
    | _ -> (improve_chars row.[0]) + (set_line row.[1..])

let rec improve_output_map (chars_map:string list) = 
    match chars_map with
    | [] -> []
    | x::xs -> (set_line x)::(improve_output_map xs)

// ┌─────────────────────────────────────────────────────────────────────────┐
// │                               END OTHER 
// └─────────────────────────────────────────────────────────────────────────┘

//  ┌─────────────────────────────────────────────────────────────────────────┐
//  │                                FUNCTION 
//  │    NAME:          print_map
/// │    DESCRIPTION:   .
//  │    CREATOR:       ML
//  │    OLD NAME:      .
//  │
//  └─────────────────────────────────────────────────────────────────────────┘
let rec print_map (chars_map:string list) = 
    match chars_map with
    | [] -> ()
    | x::xs -> printfn "%s" x
               print_map xs



// ┌─────────────────────────────────────────────────────────────────────────┐
// │                                 OTHER 
// │    NAME:          FUNZIONI OBSOLETE O DI DEBUG
// │    DESCRIPTION:   funzioni obsolete per fare l'output
// │    CREATOR:       ML
// │
// └─────────────────────────────────────────────────────────────────────────┘
let print_debug (rows:int) (cols:int) (index_f:(int->int->int)) (map:'A list)= 
    for x in 0..(rows-1) do
        for y in 0..(cols-1) do
            printf "\t  %A" (map.[(index_f x y)])
        printf "\n"

// ┌─────────────────────────────────────────────────────────────────────────┐
// │                               END OTHER 
// └─────────────────────────────────────────────────────────────────────────┘
