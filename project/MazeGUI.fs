namespace FMaze.GUI
open FMaze.Core
open LabProg2019.Gfx
open LabProg2019.Prelude
open LabProg2019.Engine

///<summary>The data structure representing an instance of a maze game rendered with the given Engine.<\summary>
type MazeGUIType = {
    ///<summary>the data structure representing the game logic.<\summary>
    logic: MazeType
    ///<summary>the data structure representing the expanded maze<\summary>
    expanded_maze: ExpandedMazeType
    ///<summary>the engine responsible for rendering the graphical user interface for the game.<\summary>
    engine: engine
    ///<summary>the sprite for the maze. Contains an image with pixels representing the current maze 2D structure.<\summary>
    maze_sprite: sprite
    ///<summary>the sprite for the player.<\summary>
    player_sprite: sprite
    
}

///<summary>The <c>MazeGUI</c> module contains functions to operate on <code>MazeGUIType</code> instances. It exposes a constructor for generating a maze game with a graphical user interface.</summary>
module MazeGUI = 

    ///<summary>the pixel value which is used to represent walls in the graphical user interface.<\summary>
    let WALL_PIXEL = pixel.create ('@',Color.White, Color.White)
    ///<summary>the pixel value which is used to represent the open path in the graphical user interface.<\summary>
    let PATH_PIXEL = pixel.create(' ',Color.Blue)
    ///<summary>the image which is used to represent the player's position in the graphical user interface.<\summary>
    let PLAYER_IMAGE = image.rectangle(1,1,pixel.create('@',Color.Yellow))

    ///<summary>Convert an ExpandedMaze to a pixel array.</summary>
    ///<param name="expanded_maze">The expanded maze that we want to convert</param>
    ///<returns>The pixel map.</returns>
    let to_pixel_array (expanded_maze:ExpandedMazeType): pixel[] =
        Array.map (fun t -> if t = Walls.CLOSED then WALL_PIXEL else PATH_PIXEL) (Array.ofList expanded_maze.map)//(Utils.to_array expanded_maze.map)

    ///<summary>Convert an ExpandedMaze to an image.</summary>
    ///<param name="expanded_maze">The expanded maze that we want to convert</param>
    ///<returns>An image from the expanded maze.</returns>
    let to_image (expanded_maze:ExpandedMazeType): image =
        new image (expanded_maze.cols, expanded_maze.rows, to_pixel_array expanded_maze)
    
    
    ///<summary>Creates a new MazeGUI from the given parameters. It generates and initializes all the necessary items to handle a graphical maze game.</summary>
    ///<param name="rows">The rows of the mazegui</param>
    ///<param name="cols">The cols of the mazegui</param>
    ///<returns>The MazeGUI instance created with the given parameters.</returns>
    let create (rows: int) (cols:int): MazeGUIType =
        let logic =Maze.create rows cols
        let expanded_maze = Maze.Expand.expand logic
        let engine= new engine(expanded_maze.cols+10, expanded_maze.rows+10)
        {
            logic=logic;
            expanded_maze= expanded_maze;
            engine=engine;
            maze_sprite=engine.create_and_register_sprite(to_image expanded_maze, 5,5,0);
            player_sprite=engine.create_and_register_sprite(PLAYER_IMAGE, snd expanded_maze.start+5, fst expanded_maze.start+5 ,0);
        }
