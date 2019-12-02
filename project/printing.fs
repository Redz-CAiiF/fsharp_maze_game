module printing

open System
open globals
open general
open game_map


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

// ┌─────────────────────────────────────────────────────────────────────────┐
// │                                FUNCTION 
// │    NAME:          get_proper_wall
// │    DESCRIPTION:   ritorna il muro corretto, per la stampa in output, in base allo stato dei muri adiacenti
// │                    w_type -> (wall type) può essere SET_THIN o SET_THICK, cambia il tipo del muro ritornato
// │    CREATOR:       ML
// │    OLD NAME:      .
// │
// └─────────────────────────────────────────────────────────────────────────┘
let get_proper_wall w_type ((w_top,w_right,w_bottom,w_left):walls) =
    match (w_top,w_right,w_bottom,w_left) with
    | ( false, false, false, false ) -> if w_type = SET_THIN then WALL_COLUMN               else THICK_WALL_COLUMN 
    | ( false, _,     false, _     ) -> if w_type = SET_THIN then WALL_HORIZONTAL           else THICK_WALL_HORIZONTAL
    | ( _,     false, _,     false ) -> if w_type = SET_THIN then WALL_VERTICAL             else THICK_WALL_VERTICAL
    | ( false, false, true,  true  ) -> if w_type = SET_THIN then WALL_ANGLE_TOP_RIGHT      else THICK_WALL_ANGLE_TOP_RIGHT
    | ( false, true,  true,  false ) -> if w_type = SET_THIN then WALL_ANGLE_TOP_LEFT       else THICK_WALL_ANGLE_TOP_LEFT
    | ( false, true,  true,  true  ) -> if w_type = SET_THIN then WALL_CONNECTION_BOTTOM    else THICK_WALL_CONNECTION_BOTTOM
    | ( true,  false, false, true  ) -> if w_type = SET_THIN then WALL_ANGLE_BOTTOM_RIGHT   else THICK_WALL_ANGLE_BOTTOM_RIGHT
    | ( true,  false, true,  true  ) -> if w_type = SET_THIN then WALL_CONNECTION_LEFT      else THICK_WALL_CONNECTION_LEFT
    | ( true,  true,  false, false ) -> if w_type = SET_THIN then WALL_ANGLE_BOTTOM_LEFT    else THICK_WALL_ANGLE_BOTTOM_LEFT
    | ( true,  true,  false, true  ) -> if w_type = SET_THIN then WALL_CONNECTION_TOP       else THICK_WALL_CONNECTION_TOP
    | ( true,  true,  true,  false ) -> if w_type = SET_THIN then WALL_CONNECTION_RIGHT     else THICK_WALL_CONNECTION_RIGHT
    | ( true,  true,  true,  true  ) -> if w_type = SET_THIN then WALL_INTERSECTION         else THICK_WALL_INTERSECTION

// ┌─────────────────────────────────────────────────────────────────────────┐
// │                                FUNCTION 
// │    NAME:          get_proper_spacing
// │    DESCRIPTION:   ritorna lo spacing corretto, per la stampa in output, in base al carattere successivo ad esso
// │    CREATOR:       ML
// │    OLD NAME:      .
// │
// └─────────────────────────────────────────────────────────────────────────┘
let get_proper_spacing w_type character =
    let get_hor_w_type w_type = if w_type = SET_THIN then WALL_HORIZONTAL else THICK_WALL_HORIZONTAL
    match character with
    | w when w = WALL_HORIZONTAL         || w = THICK_WALL_HORIZONTAL         -> get_hor_w_type w_type
    | w when w = WALL_ANGLE_TOP_RIGHT    || w = THICK_WALL_ANGLE_TOP_RIGHT    -> get_hor_w_type w_type
    | w when w = WALL_CONNECTION_BOTTOM  || w = THICK_WALL_CONNECTION_BOTTOM  -> get_hor_w_type w_type
    | w when w = WALL_ANGLE_BOTTOM_RIGHT || w = THICK_WALL_ANGLE_BOTTOM_RIGHT -> get_hor_w_type w_type
    | w when w = WALL_CONNECTION_LEFT    || w = THICK_WALL_CONNECTION_LEFT    -> get_hor_w_type w_type
    | w when w = WALL_CONNECTION_TOP     || w = THICK_WALL_CONNECTION_TOP     -> get_hor_w_type w_type
    | w when w = WALL_INTERSECTION       || w = THICK_WALL_INTERSECTION       -> get_hor_w_type w_type
    | _                                                                       -> GENERIC_PATH

// ┌─────────────────────────────────────────────────────────────────────────┐
// │                                FUNCTION 
// │    NAME:          get_neighbours_state
// │    DESCRIPTION:   ritorna una enupla*4 di bool che contiene lo stato dei muri adiacenti alla cella di indice x,y
// │                    index_f è un predicato che permette ad due variabili bi-dimensionali di ottenere una variabile mono-dimensionale
// │    CREATOR:       ML
// │    OLD NAME:      .
// │
// └─────────────────────────────────────────────────────────────────────────┘
let get_neighbours_state (x:int) (y:int) (index_f:(int->int->int)) (map:cell_reduced list) = 
    let get_cell = fun (map:cell_reduced list) (pos:int) -> if pos < 0 then ERROR_CELL_REDUCED else let cx,cy,cstate = map.[pos] in (cx,cy,cstate)
    let _,_,s_top =    get_cell map (index_f (x-1) (y)   )
    let _,_,s_right =  get_cell map (index_f (x)   (y+1) )
    let _,_,s_bottom = get_cell map (index_f (x+1) (y)   )
    let _,_,s_left =   get_cell map (index_f (x)   (y-1) )
    let b_s_top    = if s_top    = WALL then true else false
    let b_s_right  = if s_right  = WALL then true else false
    let b_s_bottom = if s_bottom = WALL then true else false
    let b_s_left   = if s_left   = WALL then true else false
    (b_s_top,b_s_right,b_s_bottom,b_s_left)

// ┌─────────────────────────────────────────────────────────────────────────┐
// │                                FUNCTION 
// │    NAME:          print_map
// │    DESCRIPTION:   data una mappa stampa la mappa passata nel terminale
// │    CREATOR:       ML
// │    OLD NAME:      .
// │
// └─────────────────────────────────────────────────────────────────────────┘
let print_map wall_type (map:cell_reduced list) =
    let rows = get_map_height map
    let cols = get_map_width map
    set_colour COLOUR_WALL COLOUR_BACKGROUND
    let index_f = index_general rows cols
    for x in 0..(rows-1) do
        for y in 0..(cols-1) do
            let x,y,block = map.[(index_f x y)]
            let character = if block = PATH then GENERIC_PATH else (get_proper_wall wall_type (get_neighbours_state x y index_f map))
            let spacing = get_proper_spacing wall_type character
            printf "%c%c" spacing character
        printf "\n"
    reset_colour





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

let print_map_generic (rows:int) (cols:int) (map:cell_reduced list) =
    let index_f = index_general rows cols
    for x in 0..(rows-1) do
        for y in 0..(cols-1) do
            let x,y,block = map.[(index_f x y)]
            let character = if block = PATH then GENERIC_PATH else GENERIC_WALL
            printf "%c" character
        printf "\n"

// ┌─────────────────────────────────────────────────────────────────────────┐
// │                               END OTHER 
// └─────────────────────────────────────────────────────────────────────────┘
