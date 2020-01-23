module maze_resolutor
open FMaze.Core
open FMaze.Core.Maze.MazeMap //replace_cell
open Utils


module Resolutor =

    type SoluitonCellType = {
        ///map of cells representing the structure of the maze
        x : int;
        y : int;
        }

    type SolutionType = {
        ///map of cells representing the structure of the maze
        map : SoluitonCellType list;
        }

    ///<summary>The seed used for generating random numbers used to find a path to the solution from the starting point</summary>
    let SEED = System.Random()



    let get_unvisited_neighbours (index: int) (maze:bool list) (rows:int) (cols:int) : int list=
        let r,c = from_monodim_to_bidim index cols
        //neighbours cells
        let top    = from_bidim_to_monodim rows cols (r-1) c 
        let right  = from_bidim_to_monodim rows cols r (c+1)
        let bottom = from_bidim_to_monodim rows cols (r+1) c 
        let left   = from_bidim_to_monodim rows cols r (c-1) 
        //the neighbours    
        List.filter (fun (el:int) -> el <> -1 && maze.[el] ) [top;right;bottom;left]




    let resolve (expanded_map:ExpandedMazeType) = 
        //(expanded_map.start_row, expanded_map.start_col)
        //(expanded_map.end_row, expanded_map.end_col)

        let rec aux (path:int list) (maze:bool list) (rows:int) (cols:int) (current:int) (exit:int):int list =
            if current = exit then
                path
            else
                //setto sulla mappa current come visitato, cioè false
                let n_maze = replace_cell current false maze
                //trovo i vicini non visitati di current
                let unvisited_neighbours = get_unvisited_neighbours current n_maze rows cols
                //se ci sono vicini
                if unvisited_neighbours.Length > 0 then
                    //prendo un vicino a caso
                    let next = unvisited_neighbours.[SEED.Next(0,unvisited_neighbours.Length)]
                    //aggiungo current a path
                    let n_path = current::path
                    //richiamo la funzione con il nuovo path, la nuova mappa, start, il nuovo current e exit
                    aux n_path n_maze rows cols next exit
                //se non ce ne sono
                else
                    //tolgo un elemento da path e lo setto come current
                    let next::n_path = path
                    //e richiamo la funzione con il nuovo path, la nuova mappa, start, il nuovo current e exit
                    aux n_path maze rows cols next exit

        //all inizio current = start
        let start = from_bidim_to_monodim expanded_map.rows expanded_map.cols expanded_map.start_row expanded_map.start_col
        let exit = from_bidim_to_monodim expanded_map.rows expanded_map.cols expanded_map.end_row expanded_map.end_col
        aux [] expanded_map.map expanded_map.rows expanded_map.cols start exit

