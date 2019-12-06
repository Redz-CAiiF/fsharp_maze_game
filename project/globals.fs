module globals

open System

//map general dimensions
let ROWS = 2 //height of a chunk
let COLUMNS = 2 //width of a chunk
let CHUNKS = 1 //number of chunks generated

//map printing constants
let EXPANDED_ROWS = ROWS*2+1
let EXPANDED_COLUMNS = COLUMNS*2+1

//map cell states
let PORTAL = 3
let PLAYER = 2
let WALL = 1
let PATH = 0

//wall status
let CLOSED = true
let OPEN = false

// ┌─────────────────────────────────────────────────────────────────────────┐
// │                                 OTHER 
// │    NAME:          FIXED PRINTING_MODULE VALUES
// │    DESCRIPTION:   tutti i valori fissati nel modulo printing
// │    CREATOR:       ML
// │
// └─────────────────────────────────────────────────────────────────────────┘
//colours
let COLOUR_WALL = ConsoleColor.White
let COLOUR_BACKGROUND = ConsoleColor.Black
let COLOUR_BACKGROUND_VISITED = ConsoleColor.Red
let COLOUR_BACKGROUND_VISITED2 = ConsoleColor.DarkRed
let COLOUR_PLAYER = ConsoleColor.Blue
let COLOUR_PORTAL = ConsoleColor.Yellow

// characters for graphical module
//path
let GENERIC_PATH = ' '

//wall
let GENERIC_WALL = '█'

let SET_THIN = '0'
//THIN
let WALL_COLUMN = '-'
let WALL_VERTICAL = '│'
let WALL_HORIZONTAL = '─'
let WALL_ANGLE_TOP_RIGHT = '┐'
let WALL_ANGLE_TOP_LEFT = '┌'
let WALL_ANGLE_BOTTOM_RIGHT = '┘'
let WALL_ANGLE_BOTTOM_LEFT = '└'
let WALL_CONNECTION_RIGHT = '├'
let WALL_CONNECTION_LEFT = '┤'
let WALL_CONNECTION_TOP = '┴'
let WALL_CONNECTION_BOTTOM = '┬'
let WALL_INTERSECTION = '┼'

let SET_THICK = '1'
//THICK
let THICK_WALL_COLUMN = '='
let THICK_WALL_VERTICAL = '║'
let THICK_WALL_HORIZONTAL = '═'
let THICK_WALL_ANGLE_TOP_RIGHT = '╗'
let THICK_WALL_ANGLE_TOP_LEFT = '╔'
let THICK_WALL_ANGLE_BOTTOM_RIGHT = '╝'
let THICK_WALL_ANGLE_BOTTOM_LEFT = '╚'
let THICK_WALL_CONNECTION_RIGHT = '╠'
let THICK_WALL_CONNECTION_LEFT = '╣'
let THICK_WALL_CONNECTION_TOP = '╩'
let THICK_WALL_CONNECTION_BOTTOM = '╦'
let THICK_WALL_INTERSECTION = '╬'

//character
let CHARACTER_1 = '©'
let CHARACTER_2 = '®'
let CHARACTER_3 = '■'

//portal
let PORTAL_1 = '¤'
let PORTAL_3 = '»'

//other

// ┌─────────────────────────────────────────────────────────────────────────┐
// │                               END OTHER 
// └─────────────────────────────────────────────────────────────────────────┘
