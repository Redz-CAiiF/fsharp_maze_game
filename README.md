# fsharp_maze_game

F# project for december 2019, currently maintained by:

* [@Redz-CAiiF](https://github.com/Redz-CAiiF)
* [@EPMatt](https://github.com/EPMatt)
* [@whitedemond](https://github.com/whitedemond)

## License

GNU General Public License v3.0, please refer to [LICENSE](https://github.com/Redz-CAiiF/fsharp_maze_game/blob/master/LICENSE) or to the last LICENSE file inside the last commit of [this repository](https://github.com/Redz-CAiiF/fsharp_maze_game), if the repository is unavailable please refer to the last electronic of phisical copy you currently have access to, if you dont have access to any copy of the license ask the person, team or company that provided you with this program, if no copy of the license can be obtained please refer to the GNU GENERAL PUBLIC LICENSE v3.0 that can be found online.

### Code standards:

Our code should follow some basic rules in order to make it more readable.

#### CONSTANTS

Constants names should be upper-case, multiple words should be divided by an underscore
> for example:  
>
> ```F#
> let ROWS = 3
> let COLUMNS =7
> let COLOUR_WALL = ConsoleColor.White
> let WALL_CONNECTION_LEFT = '┤'
> ```

#### VARIABLES

Variables names should be lower-case, multiple words should be divided by an underscore
> for example:  
>
> ```F#
> let test = 6
> let maze_tot = generate ROWS COLUMNS 3
> ```

#### FUNCTION

Function names should be lower-case, multiple words should be divided by an underscore
> for example:  
>
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

Files names must begin with a capital letter and match the main module (or namespace) defined in the file.
Files must be ordered from the more general ones to the more specific, the last file in the solution explorer must me the ```[<EntryPoint>]```
> for example:  
>
> ```F#
> Maze.fs
> Utils.fs
> ```

#### COMMENTS

Comments should follow the [XML standard](https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/xml-documentation) used for .NET projects. The documentation is generated at compile time using [DocFX](https://dotnet.github.io/docfx/).

> ```F#
>    ///<summary>Gets the cell at the specified index in the given Maze.</summary>
>    ///<param name="position">The index of the desired element in the maze</param>
>    ///<param name="maze">The maze to select the element from</param>
>    ///<returns>The cell at the specified index</returns>
>    let get_cell (position:int) (maze:MazeType):CellType =

### TO DO

- [x] basic maze generation algorithm, [Maze algorithm documentation](https://en.wikipedia.org/wiki/Maze_generation_algorithm#Recursive_backtracker)
- [x] map expansion and map updater module
- [x] basic graphics engine
- [x] graphics engine/module
- [x] maze solving algorithm
- [ ] automatic chunk generating, loading, deloading/unloading
- [ ] game menu design
- [ ] game menu module
- [ ] more game modes, for example infinite maze mode