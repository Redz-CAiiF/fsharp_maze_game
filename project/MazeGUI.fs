﻿(*
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

///The GUI namespace includes all the related modules to handle a Maze game through a graphical user interface
namespace FMaze.GUI
open FMaze.Core
open LabProg2019.Gfx
open LabProg2019.Prelude
open LabProg2019.Engine
open LabProg2019

///<summary>The data structure representing an instance of a maze game rendered with the given Engine.<\summary>
type MazeGUIType = {
    ///the data structure representing the game logic.
    logic: MazeType
    ///the data structure representing the expanded maze
    expanded_maze: ExpandedMazeType
    ///the engine responsible for rendering the graphical user interface for the game.
    engine: engine
    ///the sprite for the maze. Contains an image with pixels representing the current maze 2D structure.
    maze_sprite: sprite
    ///the sprite for the player.
    player_sprite: sprite
    ///current player position
    player_position: (int*int)
}

///<summary>The <c>MazeGUI</c> module contains functions to operate on <code>MazeGUIType</code> instances. It exposes a constructor for generating a maze game with a graphical user interface.</summary>
module MazeGUI = 

    ///the pixel value which is used to represent walls in the graphical user interface.
    let WALL_PIXEL = pixel.create ('@',Color.White, Color.White)
    ///the pixel value which is used to highlight the correct path in the graphical user interface
    let PATH_PIXEL = pixel.create('@',Color.Green, Color.Green)
    ///an empty and transparent pixel
    let EMPTY_PIXEL = pixel.create(' ', Color.Black)
    ///a black filled pixel
    let BLACK_PIXEL = pixel.create(''', Color.Black)
    ///the image which is used to represent the player's position in the graphical user interface
    let PLAYER_IMAGE = image.rectangle(1,1,pixel.create('@',Color.Red,Color.Red))
    //offsets used for displaying the maze in the engine
    let MAZE_X_OFFSET = 6
    let MAZE_Y_OFFSET = 6

    ///<summary>Convert an ExpandedMaze to a pixel array.</summary>
    ///<param name="expanded_maze">The expanded maze that we want to convert</param>
    ///<returns>The pixel map.</returns>
    let to_pixel_array (expanded_maze: ExpandedMazeType): pixel[] =
        let arr = Array.map (fun t -> if t = Walls.CLOSED then WALL_PIXEL else EMPTY_PIXEL) (Array.ofList expanded_maze.map)
        arr
    
    ///<summary>Convert an ExpandedMaze to an image.</summary>
    ///<param name="expanded_maze">The expanded maze that we want to convert.</param>
    ///<returns>An image from the expanded maze.</returns>
    let to_image (expanded_maze: ExpandedMazeType): image =
        new image (expanded_maze.cols, expanded_maze.rows, to_pixel_array expanded_maze)
    
    ///<summary>Convert an SolutionType to image. </summary>
    ///<param name = "solution"> the Solution that we wanto convert. </param>
    ///<return> image from Solution. </returns>
    let solution_to_image (solution: SolutionType) : image =
        let pxs = Array.create (solution.map_cols*solution.map_rows) EMPTY_PIXEL
        List.iter (fun e -> pxs.[e] <- PATH_PIXEL) solution.path
        new image (solution.map_cols,solution.map_rows,pxs)

    ///<summary> Solve a maze and store the solution. </summary>
    ///<param name = "maze" > the maze we want solve. </param>
    /// <return> return a unit e render the path  </returns>
    let display_solution (maze : MazeGUIType) :unit =
        //solve maze and store the solution
        let sol =Maze.Resolutor.solve maze.expanded_maze
        //render the path
        ignore (maze.engine.create_and_register_sprite(solution_to_image (sol),MAZE_X_OFFSET,MAZE_Y_OFFSET,2)) 
        

    ///<summary>Creates a new MazeGUI from the given parameters. It generates and initializes all the necessary items to handle a graphical maze game.</summary>
    ///<param name="rows">The rows of the mazegui</param>
    ///<param name="cols">The cols of the mazegui</param>
    ///<returns>The MazeGUI instance created with the given parameters.</returns>
    let create (rows: int) (cols : int): MazeGUIType =
        let logic =Maze.create rows cols
        let expanded_maze = Maze.Expand.expand logic
        let engine= new engine(expanded_maze.cols+2*MAZE_X_OFFSET, expanded_maze.rows+2*MAZE_Y_OFFSET)
        {
            logic=logic;
            expanded_maze= expanded_maze;
            engine=engine;
            maze_sprite=engine.create_and_register_sprite(to_image expanded_maze, MAZE_X_OFFSET,MAZE_Y_OFFSET,1);
            player_sprite=engine.create_and_register_sprite(PLAYER_IMAGE, snd expanded_maze.start+MAZE_X_OFFSET, fst expanded_maze.start+MAZE_Y_OFFSET ,5);
            player_position = expanded_maze.start
        }

    /// <summary> Create a new MazeGui from the difficulty given.</summary>
    /// <param name = "difficulty" > the difficulty of the maze</param>
    /// <returns>return a MazeGui instance from the difficulty given. </returns>
    let create_with_difficulty (difficulty : int) : MazeGUIType=
        create (fst Config.MAZE_DIMENSIONS.[difficulty]) (snd Config.MAZE_DIMENSIONS.[difficulty])