module globals

open System

//map default dimensions
let ROWS = 5 //height of the map
let COLUMNS = 10 //width of the map

//map cell states
let PORTAL = 3
let PLAYER = 2
let WALL = 1
let PATH = 0


// ┌─────────────────────────────────────────────────────────────────────────┐
// │                                 OTHER 
// │    NAME:          PRINTING MODULE DEFAULT VALUES
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
let PATH_CHARACTER      = ' '
let WALL_CHARACTER      = '█'
let PLAYER_CHARACTER    = '■'
let PORTAL_CHARACTER    = '¤'

// ┌─────────────────────────────────────────────────────────────────────────┐
// │                               END OTHER 
// └─────────────────────────────────────────────────────────────────────────┘
