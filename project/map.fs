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
let MAP_TYPE = (fun ((f_x,f_y,_,_):cell) ((l_x,l_y,_,_):cell) -> (l_x,f_x,l_y,f_y))

let get_sizes (map:'A list) predicate = 
    let get_first_last (map:'A list) = (map.[0])::(map.[(map.Length-1)])::[]
    let f::l::[] = get_first_last map
    let (l_x,f_x,l_y,f_y) = predicate f l
    let (h,w) = ((l_x-f_x+1),(l_y-f_y+1))
    (h,w)

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

