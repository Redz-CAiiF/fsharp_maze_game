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

///The cell data structure.
type CellType = {
    ///walls for the cell
    walls:WallsType; 
    ///whether the cell was visited by generator algorhythm or not
    visited:bool
    }


///<summary>The Cell module defines common operations on cells.</summary>
///<remarks>
///A cell is the basic component of the maze. It contains info on the walls and a boolean used for various purposes throughout the application.
///Cells, organized in a list constitute a map
///</remarks>
module Cell=
    open Walls

    ///<summary>A cell representing an error in the application</summary>
    let ERROR_CELL = {walls={top=CLOSED;right=CLOSED;bottom=CLOSED;left=CLOSED};visited=false}

    ///<summary>Sets the given value of visited for the given cell.</summary>
    ///<param name="cell">The cell which the setter operates on</param>
    ///<param name="visited">The new value of visited</param>
    ///<returns>The given cell with the updated value of visited</returns>
    let set_visited (cell:CellType) (visited:bool) : CellType = 
        {cell with visited=visited}
    
    ///<summary>Create a new <c>CellType</c> instance with the given parameters.</summary>
    let create (walls:WallsType,visited:bool) = {walls = walls;visited = visited}