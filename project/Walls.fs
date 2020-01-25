///The Core namespace includes all the related modules to logically handle a Maze game and its components.
namespace FMaze.Core
///<summary>Defines walls for a cell. Values: OPEN/CLOSED</summary>
type WallsType = {
    ///<summary>Open/Closed status for the cell's top wall</summary>
    top:bool;
    ///<summary>Open/Closed status for the cell's right wall</summary>
    right:bool;
    ///<summary>Open/Closed status for the cell's bottom wall</summary>
    bottom:bool;
    ///<summary>Open/Closed status for the cell's left wall</summary>
    left:bool
    }

///<summary>The Walls module define consts for the <c>WallType</c> type.</summary>
module Walls= 
    ///<summary>Const value which represent a closed wall.</summary>
    let CLOSED = true
    ///<summary>Const value which represent an open wall.</summary>
    let OPEN = false
