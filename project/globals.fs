module globals

open System

//map default dimensions
let ROWS = 2 //height of the map
let COLUMNS = 3 //width of the map

//map cell states
let PATH = 0
let WALL = 1
let PLAYER = 2
let PORTAL = 3

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
