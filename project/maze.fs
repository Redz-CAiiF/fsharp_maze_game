(*
*  FMaze - A .NET functional console game
*
*  (C) 2020
*  Lorenzo Donatelli  (https://github.com/whitedemond)
*  Matteo Agnoletto   (https://github.com/EPMatt)
*  Matteo Libralesso  (https://github.com/Redz-CAiiF)
*  
*  For licensing conditions related to this project, see LICENSE
*
*)

///The Core namespace includes all the related modules to logically handle a Maze game and its components.
namespace FMaze.Core

///The data structure representing a maze instance.
type MazeType = {
    ///map of cells representing the structure of the maze
    map : CellType list;
    ///number of rows the maze is made up of
    rows: int
    ///number of columns the maze is made up of
    cols: int
    ///start cell (row, column)
    start: int*int
    ///finish cell (row,column)
    finish: int*int
    }

type ExpandedMazeType = {
    ///map of cells representing the structure of the maze
    map : bool list;
    ///number of rows the maze is made up of
    rows: int
    ///number of columns the maze is made up of
    cols: int
    ///cell where the player will be first spawned at game startup.
    start: (int*int)
    ///cell that player must reach in order to succesfully end the game.
    finish: (int*int)
    }


///The data structure representing a maze solution instance.
type SolutionType = {
    ///the path from the end to the start
    path : int list;
    map_rows : int
    map_cols : int
    }

///<summary>The <c>Maze</c> module contains functions to operate on <code>MazeType</code> instances.</summary>
module Maze =
    open Utils

    ///<summary>The seed used for generating random numbers and implementing randomness in the maze generator algorhythm</summary>
    let SEED = System.Random()

    ///<summary>Defines operations on cell lists.</summary>
    module MazeMap =
        ///<summary>Generate a map and initialize cells to their default value.</summary>
        ///<param name="rows">Number of rows of the map</param>
        ///<param name="cols">Number of columns of the map</param>
        ///<returns>A new map of <code>rows</code> rows and <code>cols</code> columns whose elements are <code>CellType</code> instances initialized to default value.</returns>
        let generate_map (rows:int) (cols:int) =
            List.init (rows*cols) (fun i -> Cell.ERROR_CELL)
               
        ///<summary>Given a cell find the index of that cell in the given map.</summary>
        ///<param name="cell">The new cell to look for in the map</param>
        ///<param name="map">The cell map to search on for the cell</param>
        ///<returns>The index of the cell in the map, or <c>-1</c> if the cell is not present in the map.</returns>
        let find_cell (cell: CellType) (map:CellType list) :int  = 
            let rec find (cell:'A) (position:int) (map:'A list) = 
                match map with
                | [] -> -1
                | x::xs -> (if x <> cell then find cell (position+1) xs else position)
            find cell 0 map

        ///<summary>Remove the wall of adjacent cells.</summary>
        ///<param name="current">The current cell</param>
        ///<param name="next">The adjacent cell</param>
        ///<returns>A new map with the modified cells</returns>
        let remove_common_wall (map: CellType list) (current:int) (next:int) (cols:int) =
            let c_x,c_y = from_monodim_to_bidim current cols
            let n_x,n_y = from_monodim_to_bidim next cols
            let on_same_row = c_x = n_x
            let on_same_column = c_y = n_y
            let current_cell = map.[current]
            let next_cell = map.[next]
            if on_same_row && (c_y = (n_y-1)) then
                Utils.replace current {current_cell with walls= {current_cell.walls with right=Walls.OPEN}} (Utils.replace next {next_cell with walls= {next_cell.walls with left=Walls.OPEN}} map)
            elif on_same_row && (c_y = (n_y+1)) then
                Utils.replace current {current_cell with walls= {current_cell.walls with left=Walls.OPEN}} (Utils.replace next {next_cell with walls= {next_cell.walls with right=Walls.OPEN}} map)
            elif on_same_column && (c_x = (n_x-1)) then
                Utils.replace current {current_cell with walls= {current_cell.walls with bottom=Walls.OPEN}} (Utils.replace next {next_cell with walls= {next_cell.walls with top=Walls.OPEN}} map)
            elif on_same_column && (c_x = (n_x+1)) then
                Utils.replace current {current_cell with walls= {current_cell.walls with top=Walls.OPEN}} (Utils.replace next {next_cell with walls= {next_cell.walls with bottom=Walls.OPEN}} map)
            else
                map

    ///<summary>Gets coordinates of a random outer cell for the given maze dimensions. An outer cell is defined as a cell which is on the sides of the maze (on the map limit).</summary>
    ///<param name="rows">Number of rows the maze which the outer random coordinates must be generated is made up of.</param>
    ///<param name="cols">Number of columns the maze which the outer random coordinates must be generated is made up of.</param>
    ///<returns>A pair of int representing coordinates of a random outer cell of the given maze dimensions.</returns>
    let generate_outer_coordinate (rows:int,cols:int) : (int*int) =
        //which coordinate is fixed? 50%
        let is_row_fixed = SEED.Next(100) < 50
        //fixed coordinate: start or end of coordinate? 50%
        let fixed_coord = if SEED.Next(100) < 50 then 0 else (if is_row_fixed then rows-1 else cols-1)
        //randomize other coordinate and return
        if is_row_fixed then (fixed_coord,SEED.Next(cols)) else (SEED.Next(rows),fixed_coord)
    
    ///<summary>Gets coordinates of a random outer cell for the given maze dimensions, different from the given one. An outer cell is defined as a cell which is on the sides of the maze (on the map limit).</summary>
    ///<param name="rows">Number of rows the maze which the outer random coordinates must be generated is made up of.</param>
    ///<param name="cols">Number of columns the maze which the outer random coordinates must be generated is made up of.</param>
    ///<param name="coord">The coordinate which the newly generated coordinate must be different from.</param>
    ///<returns>A pair of int representing coordinates of a random outer cell of the given maze dimensions.</returns>
    let rec generate_different_outer_coordinate (rows:int,cols:int) (coord:int*int): (int*int) =
        let generated = generate_outer_coordinate (rows,cols)
        if generated = coord then
            generate_different_outer_coordinate (rows,cols) coord 
        else generated


    ///<summary>Gets the cell at the specified index in the given Maze. Index is specified as 1-dimensional.</summary>
    ///<param name="position">The index of the desired element in the maze</param>
    ///<param name="maze">The maze to select the element from</param>
    ///<returns>The cell at the specified index</returns>
    let get_cell (position:int) (maze:MazeType):CellType =
        maze.map.[position]

    ///<summary>Gets the cell at the specified index in the given Maze. Index is specified as 1-dimensional.</summary>
    ///<param name="position">The index of the desired element in the maze</param>
    ///<param name="maze">The maze to select the element from</param>
    ///<returns>The cell at the specified index</returns>
    let get_bi_cell (x:int) (y:int) (maze:MazeType):CellType = 
        let i= (from_bidim_to_monodim maze.rows maze.cols x y)
        in
        if i < 0 || i >= maze.cols*maze.rows then Cell.ERROR_CELL
        else maze.map.[i]
        
    ///<summary>Routines used in the maze generation process.</summary>
    module Generator =
        open MazeMap
      
        ///<summary>Given a cell map, check if the map has been entirely explored, i.e. all the cells have been visited by the generator algorhythm.</summary>
        ///<param name="map">The cell map which the check is performed on</param>
        ///<returns><c>true</c> if the map has been entirely explored, <c>false</c> if not.</returns>
        let rec is_explored (map:CellType list):bool =
            List.forall (fun (elem:CellType) -> elem.visited) map

        ///<summary>Given a maze and a cell index return a list of indexes of the cell neighbours.</summary>
        ///<param name="index">Index of the base cell</param>
        ///<param name="maze">The maze where the cell is located in</param>
        ///<returns>A list of monodimensional indexes each one representing an unvisited neighbour of the cell at the given index.</returns>
        let get_unvisited_neighbours (index: int) (maze:MazeType) : int list=
            let r,c = from_monodim_to_bidim index maze.cols
            //System.Diagnostics.Debug.WriteLine("from_monodim_to_bidim index maze.cols = {0} {1}",r,c)
            //neighbours cells
            let top    = from_bidim_to_monodim maze.rows maze.cols (r-1) c 
            let right  = from_bidim_to_monodim maze.rows maze.cols r (c+1)
            let bottom = from_bidim_to_monodim maze.rows maze.cols (r+1) c 
            let left   = from_bidim_to_monodim maze.rows maze.cols r (c-1) 
            //System.Diagnostics.Debug.WriteLine("finding unvisited neighbours of {0}",index)
            //System.Diagnostics.Debug.WriteLine( "{0} {1} {2} {3}",top,right,bottom,left)
            //the neighbours    
            List.filter (fun (el:int) -> el <> -1 && not maze.map.[el].visited ) [top;right;bottom;left]
        
        ///<summary>Given a maze and a cell index return the number of unvisited neightbours for the given cell.</summary>
        ///<param name="index">Index of the base cell</param>
        ///<param name="maze">The maze where the cell is located in</param>
        ///<returns>The number of unvisited neighbours of the cell at the given index.</returns>
        let unvisited_neighbours_number (index:int) (maze:MazeType):int =
            let neighbours = get_unvisited_neighbours index maze
            neighbours.Length
        
        ///<summary>Given a maze and a cell index check if the given cell has unvisited neighbours.</summary>
        ///<param name="index">Index of the base cell</param>
        ///<param name="maze">The maze where the cell is located in</param>
        ///<returns><c>true</c> if the given cell has unvisited neighbours, <c>false</c> if not.</returns>
        let has_unvisited_neighbours (index:int) (maze:MazeType):bool =
            let neighbours = get_unvisited_neighbours index maze
            neighbours.Length <> 0
        
        ///<summary>The recursive backtracker algorhythm used for generating the maze. See <a href="https://en.wikipedia.org/wiki/Maze_generation_algorithm#Recursive_backtracker">Maze generation on Wikipedia</a> for details about how the algorhythm.</summary>
        ///<param name="maze">An maze initialized with default cells</param>
        ///<param name="start">The index of the cell representing the entry point of the recursive backtracker algorhythm </param>
        ///<returns>The maze with randomly generated paths.</returns>
        let recursive_backtracker (maze:MazeType) (start: int):MazeType = 
            let rec aux (stack:int list) (maze:MazeType) (current:int):MazeType =
                let current_cell = Cell.set_visited (get_cell current maze) true //just visited the cell
                let map = Utils.replace (current) (current_cell) maze.map        //updated cell map
                if not (is_explored map) then
                    let unvisited_neighbours = get_unvisited_neighbours current maze //get neighbours of current cell
                    let unvisited_neighbours_number = unvisited_neighbours.Length
                    if (unvisited_neighbours_number) > 0 then      //if there are unvisited neighbours
                        let next = unvisited_neighbours.[SEED.Next(0,unvisited_neighbours_number)] //select one of the unvisited neighbours randomly
                        let removed_map = remove_common_wall map current next maze.cols //open the map
                        let new_stack = next::stack //push the neighbour in the stack
                        aux new_stack {maze with map=removed_map} next
                    else
                        let new_stack = List.tail stack
                        aux new_stack {maze with map=map} (List.head stack)
                else
                    {maze with map=map}
            aux [] maze start
     
        ///<summary>Generate a maze from a default initialized <c>Maze</c> instance.</summary>
        ///<param name="maze">A default initialized maze.</param>
        ///<returns>A generated maze using recursive backtracker as the generating algorithm</returns>
        let generate (maze:MazeType): MazeType =
            recursive_backtracker maze 0

    ///<summary>Contain routines used to expand a maze map.</summary>
    module Expand = 

        let get_outer_expanded_coord (row:int,col:int) (maze:MazeType) :(int*int)=
            if row = 0 then row, Utils.expand__coordinate_value col
            else if col = 0 then Utils.expand__coordinate_value row, col
            else if row=maze.rows-1 then (Utils.expand__coordinate_value row)+1, Utils.expand__coordinate_value col
            else Utils.expand__coordinate_value row,(Utils.expand__coordinate_value col)+1

        ///<summary>Convert a maze to its expanded form</summary>
        ///<param name="maze">A generated maze.</param>
        ///<returns>A new expanded maze made from the provided maze</returns>
        let convert_maze_to_expandedmaze (maze: MazeType):ExpandedMazeType = 

            ///<summary>Convert a maze to a bool rapresentation of that maze</summary>
            ///<param name="maze">A generated maze.</param>
            ///<returns>A boolean rappresentation of the provided maze</returns>
            let map_to_bool (maze:MazeType) = 
        
                ///<summary>It generate a default map, with all the borders as walls and all the odd coordinates as paths</summary>
                ///<param name="rows">The rows of the maze.</param>
                ///<param name="cols">The cols of the maze.</param>
                ///<returns>A boolean rappresentation of an expanded default maze with no connections between 2 paths</returns>
                let generate_default_map (rows:int) (cols:int):bool list = 
                    let mutable res = []
                    for x in 0..(rows-1) do
                        let mutable row = []:bool list
                        for y in 0..(cols-1) do
                            //se sia x che y sono dispari
                            row <- row@[(if (x |> Utils.isOdd) && (y |> Utils.isOdd) then Walls.OPEN else Walls.CLOSED)]
    
                        res <- row@res
                    res
                
                ///<summary>It fill a default expanded map with the paths of a generated maze</summary>
                ///<param name="maze">A generated maze.</param>
                ///<returns>A boolean rappresentation of a maze</returns>
                let set_maze (maze:MazeType) = 
                    let mutable res = generate_default_map (maze.rows |> Utils.expand__coordinate_value) (maze.cols |> Utils.expand__coordinate_value)
                    for index in 0..(maze.map.Length-1) do
                        //per ogni cella in map prendo gli stati dei muri destra e basso e gli cambio nella mappa dei chars
                        let current = maze.map.[index]
                        let x,y = Utils.from_monodim_to_bidim index maze.cols
    
                        let x = Utils.expand__coordinate_value x
                        let y = Utils.expand__coordinate_value y
                        //sostituisco i muri right e bottom con quelli salvati nella cella
                        let exp_index = Utils.from_bidim_to_monodim (maze.rows |> Utils.expand__coordinate_value) (maze.cols |> Utils.expand__coordinate_value)
                        let replace_cell (position:int) (cell:'A) (map:'A list): 'A list = 
                            map.[..(position-1)]@[cell]@map.[(position+1)..]
    
                        res <- replace_cell (exp_index x (y+1)) current.walls.right res
                        res <- replace_cell (exp_index (x+1) y) current.walls.bottom res
                    res
                set_maze maze
            
            let sr,sc = get_outer_expanded_coord maze.start maze
            let fr,fc = get_outer_expanded_coord maze.finish maze
            let map = map_to_bool maze;
            {
                //map: the converted bool map with the outer start cell open and the outer finish cell open
                map =  Utils.replace (from_bidim_to_monodim (expand__coordinate_value maze.rows) (expand__coordinate_value maze.cols) sr sc) Walls.OPEN (Utils.replace (from_bidim_to_monodim (expand__coordinate_value maze.rows) (expand__coordinate_value maze.cols) fr fc) Walls.OPEN map)
                rows = (maze.rows |> Utils.expand__coordinate_value);
                cols = (maze.cols |> Utils.expand__coordinate_value);
                start= sr,sc
                finish= fr,fc
            }

        ///<summary>Expand the provided maze.</summary>
        ///<returns>The expanded maze</returns>
        let expand (maze:MazeType) : ExpandedMazeType =
            convert_maze_to_expandedmaze maze

    ///<summary>Contain used to solve a maze.</summary>
    module Resolutor =
        open MazeMap

        ///<summary>Return the index of all the unvisited neighbours or the non wall parts of the maze</summary>
        ///<param name="index">The index of the choosen cell</param>
        ///<param name="maze">The maze to select the element from</param>
        ///<param name="rows">The maze rows number</param>
        ///<param name="cols">The maze columns number</param>
        ///<returns>A list with the unvisited cells connected to the current one</returns>
        let get_unvisited_neighbours (index: int) (maze:bool list) (rows:int) (cols:int) : int list=
            let r,c = from_monodim_to_bidim index cols
            //neighbours cells
            let top    = from_bidim_to_monodim rows cols (r-1) c 
            let right  = from_bidim_to_monodim rows cols r (c+1)
            let bottom = from_bidim_to_monodim rows cols (r+1) c 
            let left   = from_bidim_to_monodim rows cols r (c-1) 
            //the neighbours
            List.filter (fun (el:int) -> el <> -1 && not maze.[el] ) [top;right;bottom;left]

        ///<summary>Solve the given maze</summary>
        ///<param name="expanded_map">The maze in its expanded form that will be used to find the solution</param>
        ///<returns>A value of SolutionType type, containing the solution of the maze and the maze dimensions</returns>
        let solve (expanded_map:ExpandedMazeType):SolutionType = 
            let rec aux (path:int list) (maze:bool list) (rows:int) (cols:int) (current:int) (exit:int):int list =
                if current = exit then
                    //aggiungo current a path e ritorno il tutto
                    current::path
                else
                    //setto sulla mappa current come visitato, cioè false
                    let n_maze = Utils.replace current true maze
                    //trovo i vicini non visitati di current
                    let unvisited_neighbours = get_unvisited_neighbours current n_maze rows cols
                    //se ci sono vicini
                    if unvisited_neighbours.Length > 0 then
                        //prendo un vicino a caso
                        let next = unvisited_neighbours.[0]
                        //aggiungo current a path
                        let n_path = current::path
                        //richiamo la funzione con il nuovo path, la nuova mappa, start, il nuovo current e exit
                        aux n_path n_maze rows cols next exit
                    //se non ce ne sono
                    else
                        //tolgo un elemento da path e lo setto come current
                        let next::n_path = path
                        //e richiamo la funzione con il nuovo path, la nuova mappa, start, il nuovo current e exit
                        aux n_path n_maze rows cols next exit

            //all inizio current = start
            let start = from_bidim_to_monodim expanded_map.rows expanded_map.cols (fst expanded_map.start) (snd expanded_map.start)
            let exit = from_bidim_to_monodim expanded_map.rows expanded_map.cols (fst expanded_map.finish) (snd expanded_map.finish)
            let ind_path = aux [] expanded_map.map expanded_map.rows expanded_map.cols start exit
        
            {path = ind_path; map_rows = expanded_map.rows; map_cols = expanded_map.cols}

        
    ///<summary>Creates a new Maze from the given parameters.</summary>
    ///<param name = "rows "> the rows of the maze. </param>
    ///<param name = "cols" > the cols of the maze </param>
    ///<returns> The maze with the given parameters. </returns>
    let create (rows:int) (cols:int) : MazeType =
        let start =  generate_outer_coordinate (rows,cols)
        Generator.generate {map = (MazeMap.generate_map rows cols) ; rows = rows; cols = cols; start = start; finish= generate_different_outer_coordinate (rows,cols) start}

