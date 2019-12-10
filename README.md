# fsharp_maze_game
F# project for december 2019, currently maintained by:
* @Redz-CAiiF
* @EPMatt
* @whitedemond

## License
GNU General Public License v3.0, please refer to [LICENSE](https://github.com/Redz-CAiiF/fsharp_maze_game/blob/master/LICENSE) or to the last LICENSE file inside the last commit of [this repository](https://github.com/Redz-CAiiF/fsharp_maze_game), if the repository is unavailable please refer to the last electronic of phisical copy you currently have access to, if you dont have access to any copy of the license ask the person, team or company that provided you with this program, if no copy of the license can be obtained please refer to the GNU GENERAL PUBLIC LICENSE v3.0 that can be found online.

### Code standards:
Our code should follow some basic rules in order to make it more readable.

#### CONSTANTS
Constants names should be upper-case, multiple words should be divided by an underscore
> for example: 
> ```F#
> let ROWS = 3
> let COLUMNS =7
> let COLOUR_WALL = ConsoleColor.White
> let WALL_CONNECTION_LEFT = '┤'
> ```

#### VARIABLES
Variables names should be lower-case, multiple words should be divided by an underscore
> for example: 
> ```F#
> let test = 6
> let maze_tot = generate ROWS COLUMNS 3
> ```

#### FUNCTION
Function names should be lower-case, multiple words should be divided by an underscore
> for example: 
> ```F#
> let rec generate rows cols chunk =
>       match chunk with
>       | 1 -> let gen = (generate_maze (generate_map rows cols))
>              printfn "Generating chunk n°%A" chunk
>              gen
>       | _ -> let gen = (connect_default (generate rows cols (chunk-1)) (generate_maze (generate_map rows cols)))
>              printfn "Generating chunk n°%A" chunk
>              gen
> ```

#### FILES
Files names should be lower-case, multiple words should be divided by an underscore
Files must be ordered from the more general ones to the more specific, the last file in the solution explorer must me the ```[<EntryPoint>]```
> for example: 
> ```F#
> globals.fs
> game_map.fs
> ```

#### COMMENTS
General comments should follow some basic standard, in order to speed up the process of finding the right resource
> ```F#
> // ┌─────────────────────────────────────────────────────────────────────────┐
> // │                                 OTHER 
> // │    NAME:          DEFINITION OF CELL AND CELL_REDUCED
> // │    DESCRIPTION:   tutte le definizioni dei walls, cell, cell_reduced e delle celle di errore
> // │    CREATOR:       ML
> // │
> // └─────────────────────────────────────────────────────────────────────────┘
> 
> // ┌─────────────────────────────────────────────────────────────────────────┐
> // │                               END OTHER 
> // └─────────────────────────────────────────────────────────────────────────┘
> 
> // ┌─────────────────────────────────────────────────────────────────────────┐
> // │                                FUNCTION 
> // │    NAME:          extend_size
> // │    DESCRIPTION:   data una dimensione ritorna la dimensione estesa f(x):x*2+1
> // │    CREATOR:       ML
> // │    OLD NAME:      .
> // │
> // └─────────────────────────────────────────────────────────────────────────┘
> ```

### DONE:
- [x] basic maze generation algorithm, [Maze algorithm documentation](https://en.wikipedia.org/wiki/Maze_generation_algorithm#Recursive_backtracker)
- [x] map expansion and map updater module
- [x] basic graphics engine

### TO DO:
- [ ] graphics engine/module
- [ ] maze solving algorithm
- [ ] automatic chunk generating, loading, deloading/unloading
- [ ] game menu design
- [ ] game menu module
- [ ] more game modes, for example infinite maze mode

### OTHER:
##### game menu design:
this is part is an example of some possible elements in a menu
> General list element
> ```F#
> ┌──────────────┐
> │ ELEMENT NAME |
> └──────────────┘
> ```
> General list element with a button
> ```F#
> ┌──────────────┐ ┌───────┐
> │ ELEMENT NAME | │ START |
> └──────────────┘ └───────┘
> ```
> some element with a field to increase or decrease some value, for example a field to increase or reduce the game map size
> ```F#
> ┌──────────────┐ ┌─────┐┌────────┐┌─────┐
> │ ELEMENT NAME | │  <  |│ NUMBER |│  >  |
> └──────────────┘ └─────┘└────────┘└─────┘
> ```
> * The elements should change color based on their current selection
> * The menu should be WASD or arrows driven
