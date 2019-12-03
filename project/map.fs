module map

open globals
open general


// ┌─────────────────────────────────────────────────────────────────────────┐
// │                                 OTHER 
// │    NAME:          MAP SIZING FUNCTIONS
// │    DESCRIPTION:   tutte le funzioni per ottenere le dimensioni di una mappa
// │    CREATOR:       ML
// │
// └─────────────────────────────────────────────────────────────────────────┘
let get_sizes (map:'A list) predicate = 
    let (h,w) = predicate map
    (h+1,w+1)
    
let map_sizes map = get_sizes map (fun (map:cell list) -> let (l_x,l_y,_,_)::_ = List.rev map in (l_x,l_y))
let map_expanded_sizes map = get_sizes map (fun (map:cell_reduced list) -> let (l_x,l_y,_)::_ = List.rev map in (l_x,l_y))

let get_map_height (h:int,w:int) = 
    h
let get_map_width (h:int,w:int) = 
    w

// ┌─────────────────────────────────────────────────────────────────────────┐
// │                               END OTHER 
// └─────────────────────────────────────────────────────────────────────────┘


// ┌─────────────────────────────────────────────────────────────────────────┐
// │                                FUNCTION 
// │    NAME:          generate_matrix
// │    DESCRIPTION:   dati il numero di righe e colonne genera una lista basata sul predicato passato, che puo essere traslata in una matrice
// │    CREATOR:       ML
// │    OLD NAME:      .
// │
// └─────────────────────────────────────────────────────────────────────────┘
let generate_matrix (rows:int) (cols:int) (predicate:(int -> int -> 'A)) =
    // ┌─────────────────────────────────────────────────────────────────────────┐
    // │                               AUX FUNCTION 
    // │    DESCRIPTION:   genera una serie di celle basate sul predicato passato, gli indici delle celle
    // │                    vanno da (current_row,0,..) a (curren_row,coll_number-1,..)
    // └─────────────────────────────────────────────────────────────────────────┘
    let rec generate_row_cols (current_row:int) (col_number:int) predicate =
        match col_number with
        | 0 -> []
        | x when x > 0 -> (generate_row_cols current_row (col_number-1) predicate) @ [(predicate (current_row-1) (col_number-1))]
        | _ -> raise ERROR_NEGATIVE_COLUMNS_NUMBER
    
    // ┌─────────────────────────────────────────────────────────────────────────┐
    // │                               AUX FUNCTION 
    // │    DESCRIPTION:   per ogni riga chiama generate_row_cols e aggiunge quel risultato in coda alla lista di ritorno
    // └─────────────────────────────────────────────────────────────────────────┘
    let rec generate_rows (rows:int) (cols:int) predicate = 
        match rows with
        | 0 -> []
        | x when x > 0 -> (generate_rows (rows-1) cols predicate) @ (generate_row_cols x cols predicate)
        | _ -> raise ERROR_NEGATIVE_ROWS_NUMBER

    generate_rows rows cols predicate

// ┌─────────────────────────────────────────────────────────────────────────┐
// │                                FUNCTION 
// │    NAME:          generate_map
// │    DESCRIPTION:   chiama generate_row con il predicato per creare una cell
// │    CREATOR:       ML
// │    OLD NAME:      .
// │
// └─────────────────────────────────────────────────────────────────────────┘
let generate_map (rows:int) (cols:int) =
    let res:cell list = generate_matrix rows cols (fun row col -> (row, col, (true,true,true,true), false))
    res

// ┌─────────────────────────────────────────────────────────────────────────┐
// │                                FUNCTION 
// │    NAME:          generate_expanded_map
// │    DESCRIPTION:   chiama generate_row con il predicato per creare una cell_reduced
// │    CREATOR:       ML
// │    OLD NAME:      .
// │
// └─────────────────────────────────────────────────────────────────────────┘
let generate_expanded_map (rows:int) (cols:int) =
    let res:cell_reduced list = generate_matrix rows cols (fun row col -> (row, col, WALL))
    res


// ┌─────────────────────────────────────────────────────────────────────────┐
// │                                FUNCTION 
// │    NAME:          find_cell
// │    DESCRIPTION:   data un elemento e una lista ritorna l'indice di quell'elemento nella lista
// │    CREATOR:       ML
// │    OLD NAME:      .
// │
// └─────────────────────────────────────────────────────────────────────────┘
let find_cell (cell:'A) (map:'A list) = 
    let rec find (cell:'A) (position:int) (map:'A list) = 
        match map with
        | [] -> -1
        | x::xs -> (if x <> cell then find cell (position+1) xs else position)
    find cell 0 map

// ┌─────────────────────────────────────────────────────────────────────────┐
// │                                FUNCTION 
// │    NAME:          get_cell
// │    DESCRIPTION:   dato un indice e una lista ritorna ll valore a quel indice
// │    CREATOR:       ML
// │    OLD NAME:      .
// │
// └─────────────────────────────────────────────────────────────────────────┘
let get_cell (position:int) (map:list<'A>) = 
    map.[position]

// ┌─────────────────────────────────────────────────────────────────────────┐
// │                                FUNCTION 
// │    NAME:          replace_cell
// │    DESCRIPTION:   data un indice un elemento e una lista sostituisce l'elemento, nella lista, 
// │                    in posizione indice con quello passato e ritorna la nuova lista
// │    CREATOR:       ML
// │    OLD NAME:      .
// │
// └─────────────────────────────────────────────────────────────────────────┘
let replace_cell (position:int) cell (map:'A list) = 
    map.[..(position-1)]@[cell]@map.[(position+1)..]


// ┌─────────────────────────────────────────────────────────────────────────┐
// │                                FUNCTION 
// │    NAME:          update_visited
// │    DESCRIPTION:   data una cella e il valore di visited, la funzione sostituisce visited nella cell con quello nuovo e ritorna la nuova cell
// │    CREATOR:       ML
// │    OLD NAME:      .
// │
// └─────────────────────────────────────────────────────────────────────────┘
let update_visited (cell:cell) (visited:bool) = 
    let c_x,c_y,c_walls,_ = cell
    (c_x,c_y,c_walls,true)

// ┌─────────────────────────────────────────────────────────────────────────┐
// │                                FUNCTION 
// │    NAME:          set_visited
// │    DESCRIPTION:   richiama update_visited con il valore di visited sempre a true
// │    CREATOR:       ML
// │    OLD NAME:      update_visited
// │
// └─────────────────────────────────────────────────────────────────────────┘
let set_visited (cell:cell) = 
    update_visited cell true



// ┌─────────────────────────────────────────────────────────────────────────┐
// │                                FUNCTION 
// │    NAME:          expand
// │    DESCRIPTION:   data una mappa di cell, ritorna la mappa estesa della mappa passata
// │    CREATOR:       ML
// │    OLD NAME:      .
// │
// └─────────────────────────────────────────────────────────────────────────┘
let expand (map:cell list) =
    let isEven x = (x % 2) = 0
    let isOdd x = isEven x = false

    let n_cols = ((map_sizes map) |> get_map_width)*2+1
    let n_rows = ((map_sizes map) |> get_map_height)*2+1
    let ext_map = generate_expanded_map n_rows n_cols
    let rec set_unconditional_cells = fun map -> 
        match map with
        | [] -> []
        | (x,y,value)::tail ->  (if (x |> isOdd) && (y |> isOdd) then (x,y,PATH) else (x,y,value))::(set_unconditional_cells tail)
    let rec set_walls = fun (map:cell list) (ext_map:cell_reduced list) ->
        //tolgo una cella dalla mappa è modifico ext_map
        match map with
        | [] -> ext_map
        | cell::tail -> (
                        //do the logic here, what to return, how to set the walls on the extended map
                        // use replace_cell on ext_map
                        let x,y,walls,_ = cell
                        let wall_top,wall_right,wall_bottom,wall_left = walls
                        let n_x = 2 * x + 1
                        let n_y = 2 * y + 1
                        //per ogni muro controllo se non c'è, se non c'è modifico la mappa estesa
                        let wall_check = fun wall x y map -> if not wall then replace_cell (index_general n_rows n_cols x y) (x,y,PATH) map else map

                        let ext_map = wall_check wall_top    (n_x-1) (n_y)   ext_map
                        let ext_map = wall_check wall_right  (n_x)   (n_y+1) ext_map
                        let ext_map = wall_check wall_bottom (n_x+1) (n_y)   ext_map
                        let ext_map = wall_check wall_left   (n_x)   (n_y-1) ext_map

                        set_walls tail ext_map
                        )
    
    set_walls map (set_unconditional_cells ext_map)

