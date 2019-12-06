module general

open globals


// ┌─────────────────────────────────────────────────────────────────────────┐
// │                                 OTHER 
// │    NAME:          DEFINITION OF CELL AND CELL_REDUCED
// │    DESCRIPTION:   tutte le definizioni dei walls, cell, cell_reduced e delle celle di errore
// │    CREATOR:       ML
// │
// └─────────────────────────────────────────────────────────────────────────┘
// ┌─────────────────────────────────────────────────────────────────────────┐
// │                               DEFINITION
// │    NAME:          walls
// │    DEFINITION:    (top:bool,right:bool,bottom:bool,left:bool)
// │    CREATOR:       ML
// │
// └─────────────────────────────────────────────────────────────────────────┘
type walls = bool*bool*bool*bool

// ┌─────────────────────────────────────────────────────────────────────────┐
// │                               DEFINITION
// │    NAME:          cell
// │    DEFINITION:    (x:int, y:int, walls:walls, visited:bool)
// │    CREATOR:       ML
// │
// └─────────────────────────────────────────────────────────────────────────┘
type cell = (int*int*walls*bool)

// ┌─────────────────────────────────────────────────────────────────────────┐
// │                               DEFINITION
// │    NAME:          cell_reduced
// │    DEFINITION:    (x:int, y:int, status:int) -> status: PATH / WALL / PLAYER / PORTAL
// │    CREATOR:       ML
// │
// └─────────────────────────────────────────────────────────────────────────┘
type cell_reduced = (int*int*int)

// ┌─────────────────────────────────────────────────────────────────────────┐
// │                          DEFAULT ERROR CELLS
// └─────────────────────────────────────────────────────────────────────────┘
let ERROR_CELL = (-1,-1,(false,false,false,false),false)
let ERROR_CELL_REDUCED = (-1,-1,PATH)

// ┌─────────────────────────────────────────────────────────────────────────┐
// │                               END OTHER 
// └─────────────────────────────────────────────────────────────────────────┘

// ┌─────────────────────────────────────────────────────────────────────────┐
// │                              EXCEPTIONS 
// │    CREATOR:       ML
// │
// └─────────────────────────────────────────────────────────────────────────┘
exception ERROR_NEGATIVE_COLUMNS_NUMBER
exception ERROR_NEGATIVE_ROWS_NUMBER
exception ERROR_NEGATIVE_INDEX_NUMBER


// ┌─────────────────────────────────────────────────────────────────────────┐
// │                                FUNCTION 
// │    NAME:          index_general
// │    DESCRIPTION:   funzione generica che trasforma delle cordinate bi-dimensionali a una cordinata mono-dimensionale
// │    CREATOR:       ML
// │    OLD NAME:      .
// │
// └─────────────────────────────────────────────────────────────────────────┘
let index_general = fun (rows:int) (cols:int) (x:int) (y:int) -> if (x < 0 || y < 0 || y > cols-1 || x > rows-1) then -1 else y + x * cols

// ┌─────────────────────────────────────────────────────────────────────────┐
// │                                FUNCTION 
// │    NAME:          extend_size
// │    DESCRIPTION:   data una dimensione ritorna la dimensione estesa f(x):x*2+1
// │    CREATOR:       ML
// │    OLD NAME:      .
// │
// └─────────────────────────────────────────────────────────────────────────┘
let extend_size (size:int) = size*2+1


