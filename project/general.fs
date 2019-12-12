///The Core namespace includes all the related modules to logically handle a Maze game and its components.
namespace FMaze.Core
///<summary>The Utils module contains utility functions and definitions used across the whole application</summary>
module Utils=

    ///<summary>Check if the given coordinates are valid in the given matrix dimensions.</summary>
    ///<param name="rows">Number of rows of the matrix</param>
    ///<param name="cols">Numbers of columns of the matrix</param>
    ///<param name="row">Row index to check</param>
    ///<param name="col">Column index to check</param>
    ///<returns><c>true</c> if the coordinates are vaild in the given matrix dimensions, <c>false</c> if not.</returns>
    let are_coordinates_valid (rows:int) (cols:int) (row:int) (col:int):bool  = row> -1 && col> -1 && row<rows && col<cols

    ///<summary>Convert bidimensional coordinates in a 1-dimensional index for matrix operations.</summary>
    ///<param name="rows">Number of rows of the matrix</param>
    ///<param name="cols">Numbers of columns of the matrix</param>
    ///<param name="row">Row index</param>
    ///<param name="col">Column index</param>
    ///<returns>The respective 1-dimensional index if coordinates are valid, false if not</returns>
    let from_bidim_to_monodim (rows:int) (cols:int) (row:int) (col:int) :int=
        if are_coordinates_valid rows cols row col then col + row * cols 
        else -1

    ///<summary>Convert a monodimensional index into bidimensional coordinates.</summary>
    ///<param name="index">The monodimensional index to convert to</param>
    ///<param name="cols">Numbers of columns in the matrix</param>
    ///<returns>A tuple <c>(row , col )</c> containing the coordinates corresponding the given momodimensional index.</returns>
    let from_monodim_to_bidim (index:int) (cols:int) :int*int = (index/cols, index%cols)


    ///<summary>Check if the given number is even.</summary>
    ///<param name="x">The value to check</param>
    ///<returns><c>true</c> if the number is even, <c>false</c> if not.</returns>
    let isEven (x:int):bool = (x % 2) = 0


    ///<summary>Check if the given number is odd.</summary>
    ///<param name="x">The value to check</param>
    ///<returns><c>true</c> if the number is odd, <c>false</c> if not.</returns>
    let isOdd (x:int):bool = not (isEven x)




///<summary>Defines walls for a cell. Values: OPEN/CLOSED</summary>
type WallsType = {
    ///Open/Closed status for the cell's top wall
    top:bool;
    ///Open/Closed status for the cell's right wall
    right:bool;
    ///Open/Closed status for the cell's bottom wall
    bottom:bool;
    ///Open/Closed status for the cell's left wall
    left:bool
    }

///<summary>The Walls module define consts for the <c>WallType</c> type.</summary>
module Walls= 
    ///<summary>Const value which represent a closed wall.</summary>
    let CLOSED = true
    ///<summary>Const value which represent an open wall.</summary>
    let OPEN = false

    let create (top:bool,right:bool,bottom:bool,left:bool) = {top = top;right = right;bottom = bottom;left = left}





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
