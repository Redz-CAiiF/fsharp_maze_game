namespace FMaze.GUI
open FMaze.Core
open LabProg2019.Gfx
open LabProg2019.Prelude
open LabProg2019.Engine

///The data structure representing an instance of a maze game rendered with the given Engine.
type MazeGUIType = {
    ///the data structure representing the game logic.
    logic: MazeType
    ///the engine responsible for rendering the graphical user interface for the game.
    engine: engine
    ///the sprite for the maze. Contains an image with pixels representing the current maze 2D structure.
    maze_sprite: sprite
    ///the sprite for the player.
    player_sprite: sprite
    ///pixel where the player will be first spawned at game startup.
    start: (int*int)
    ///pixel that player must reach in order to succesfully end the game.
    finish: (int*int)
}

///<summary>The <c>MazeGUI</c> module contains functions to operate on <code>MazeGUIType</code> instances. It exposes a constructor for generating a maze game with a graphical user interface.</summary>
module MazeGUI = 

    ///the pixel value which is used to represent walls in the graphical user interface
    let WALL_PIXEL = pixel.create ('@',Color.White, Color.White)
    ///the pixel value which is used to represent the open path in the graphical user interface
    let PATH_PIXEL = pixel.create(' ',Color.Blue)
    ///the image which is used to represent the player's position in the graphical user interface
    let PLAYER_IMAGE = image.rectangle(1,1,pixel.create('@',Color.Yellow))

    let get_outer_expanded_coord (row:int,col:int) (maze:MazeType) :(int*int)=
           if row = 0 then row, Utils.expand__coordinate_value col
           else if col = 0 then Utils.expand__coordinate_value row, col
           else if row=maze.rows-1 then (Utils.expand__coordinate_value row)+1, Utils.expand__coordinate_value col
           else Utils.expand__coordinate_value row,(Utils.expand__coordinate_value col)+1

    let expand (maze:MazeType) : bool[] = 
               let r = Array.create ((Utils.expand__coordinate_value maze.rows) * (Utils.expand__coordinate_value maze.cols)) (false)        
               let generate_default_map (rows:int) (cols:int):bool list = 
                   let mutable res = []
                   for x in 0..(rows-1) do
                       let mutable row = []:bool list
                       for y in 0..(cols-1) do
                           //se sia x che y sono dispari
                           row <- row@[(if (x |> Utils.isOdd) && (y |> Utils.isOdd) then Walls.OPEN else Walls.CLOSED)]

                       res <- row@res
                   res

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
               let lst=set_maze maze
               for i=0  to (Array.length(r))-1 do 
                    r.[i]<- lst.[i]
               r

    let to_pixel_array (maze: MazeType): pixel[] =
        let arr = Array.map (fun t -> if t = Walls.CLOSED then WALL_PIXEL else PATH_PIXEL) (expand maze)
        //open start cell which lays on the external wall
        let sr,sc=(get_outer_expanded_coord maze.start maze)
        arr.[Utils.from_bidim_to_monodim (Utils.expand__coordinate_value maze.rows) (Utils.expand__coordinate_value maze.cols) sr sc]<-PATH_PIXEL
        //open end cell which lays on the external wall
        let fr,fc=(get_outer_expanded_coord maze.finish maze)
        arr.[Utils.from_bidim_to_monodim (Utils.expand__coordinate_value maze.rows) (Utils.expand__coordinate_value maze.cols) fr fc]<-PATH_PIXEL
        arr

    let to_image (maze: MazeType): image =
        new image ((Utils.expand__coordinate_value maze.cols), (Utils.expand__coordinate_value maze.rows), to_pixel_array maze)
    
    
    ///<summary>Creates a new MazeGUI from the given parameters. It generates and initializes all the necessary items to handle a graphical maze game.</summary>
    ///<returns>The MazeGUI instance created with the given parameters.</returns>
    let create (rows: int) (cols:int): MazeGUIType =
        let logic =Maze.create rows cols
        let expanded_rows = Utils.expand__coordinate_value rows
        let expanded_cols = Utils.expand__coordinate_value cols
        let engine= new engine(expanded_cols+10, expanded_rows+10)
        let start = get_outer_expanded_coord logic.start logic
        {
            logic=logic;
            engine=engine;
            maze_sprite=engine.create_and_register_sprite(to_image logic, 5,5,0);
            player_sprite=engine.create_and_register_sprite(PLAYER_IMAGE, snd start+5, fst start+5 ,0);
            start=start
            finish=get_outer_expanded_coord logic.finish logic
        }


   
