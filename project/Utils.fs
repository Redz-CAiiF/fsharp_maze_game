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

    ///<summary>Expand the given coordinate</summary>
    ///<param name="x">The coordinate to expand</param>
    ///<returns><c>true</c> the expanded coordinate</returns>
    let expand__coordinate_value (x:int):int = x*2+1
