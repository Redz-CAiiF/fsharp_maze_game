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

(*
* LabProg2019 - Progetto di Programmazione a.a. 2019-20
* Program.fs - game entry point
* (C) 2019 Alvise Spano' @ Universita' Ca' Foscari di Venezia
*)

module FMaze.Program

open FMaze.Core.Utils
open FMaze.Core
open System
open System.Diagnostics
open LabProg2019.Globals
open LabProg2019
open LabProg2019.Gfx
open LabProg2019.Config
open System.IO.Pipes
open System.IO
open FMaze.GUI

let render_menu () : Engine.engine =
    let engine = new Engine.engine (MENU_WIDTH, MENU_HEIGHT)
    let menu = engine.create_and_register_sprite (image.rectangle (MENU_WIDTH,MENU_HEIGHT, MazeGUI.BLACK_PIXEL),0, 0, 0)
    menu.draw_text("oooooooooooo ooo        ooooo", TITLE_X , TITLE_Y, Color.White)
    menu.draw_text(" `888'     `8 `88.       .888'", TITLE_X , TITLE_Y+1, Color.White)
    menu.draw_text("  888          888b     d'888   .oooo.     oooooooo  .ooooo.", TITLE_X , TITLE_Y+2, Color.White)
    menu.draw_text("  888oooo8     8 Y88. .P  888  `P  )88b   d'''7d8P  d88' `88b" , TITLE_X , TITLE_Y+3, Color.White)
    menu.draw_text("  888    '     8  `888'   888   .oP'888     .d8P'   888ooo888", TITLE_X , TITLE_Y+4, Color.White)
    menu.draw_text("  888          8    Y     888  d8(  888   .d8P'  .P 888    .o" , TITLE_X , TITLE_Y+5, Color.White)
    menu.draw_text(" o888o        o8o        o888o `Y888''8o d8888888P  `Y8bod8P'", TITLE_X , TITLE_Y+6, Color.White)
    menu.draw_text("A .NET functional console game", TITLE_X+16 , TITLE_Y+8, Color.White)
    menu.draw_text("Game modes :\n", TITLE_X ,  TITLE_Y+12, Color.Yellow)
    menu.draw_text("1 : Interactive", TITLE_X ,  TITLE_Y+14, Color.Cyan)
    menu.draw_text("2 : Automatic resolution", TITLE_X ,  TITLE_Y+15, Color.Cyan)
    menu.draw_text("3 : Dark Labyrinth", TITLE_X,  TITLE_Y+16, Color.Cyan)
    menu.draw_text("Q : uscita", TITLE_X , TITLE_Y+18, Color.Yellow)
    menu.draw_text(COPYRIGHT_NOTICE, TITLE_X , TITLE_Y+21, Color.White)
    engine


///prompt user to select a difficulty for the required game mode, and start the game
let select_difficulty (engine:Engine.engine) (start_game: int -> unit) : unit =
    let diff_banner = GUI.Utils.render_banner engine ["Select game difficulty";"";"1: Easy";"2: Medium";"3: Difficult";"4: Impossible"]
    let handle_menu_selection (key : ConsoleKeyInfo) (screen : wronly_raster) (st : bool) =
        let st =
            //handle menu selection: run the corresponding routine to start a different game mode
            match key.KeyChar with 
                 '1' | '2' | '3' | '4' -> start_game (int key.KeyChar - int '0'-1) //start game with desired difficulty: inline char to int conversion
                                          ignore (engine.unregister_sprite diff_banner)
                                          true
                | _   ->                  false                           //Default: invalid selection
        st, st

    //loop on menu selection
    engine.loop_on_key handle_menu_selection false
    ()

let winner (engine:Engine.engine) (maze_rows:int) (maze_cols:int) :unit =
     (GUI.Utils.render_banner engine ["Victory!";"";"Press Q to go back to main menu"]) |> ignore
     ()

///initialize an interactive game with the given difficulty
let mode_interactive (difficulty:int) =
    let gui = MazeGUI.create_with_difficulty (difficulty)
    let upper_text = gui.engine.create_and_register_sprite (image.rectangle (MENU_WIDTH,MENU_HEIGHT, MazeGUI.EMPTY_PIXEL),1, 1, 1)
    upper_text.draw_text("Interactive Mode:",0,0,Color.White)
    upper_text.draw_text("You're the red dot. Exit the maze!",0,1,Color.White)
    upper_text.draw_text("W: up , A: left , S: down , D: right",0,2,Color.White)
    upper_text.draw_text("R: solve maze , Q: main menu",0,3,Color.White)

    let handle_user_interaction (key : ConsoleKeyInfo) (screen : wronly_raster) ((state : MazeGUIType), (lock_input : bool)) =
        let dx, dy =
            match key.KeyChar,lock_input with 
                _ , true -> 0,0 //don't move player if input is locked
              | 'w',_ -> 0, -1
              | 'a',_ ->  -1, 0
              | 'd',_ ->  1, 0
              | 's',_ ->  0, 1
              | 'r',_ ->  ignore (MazeGUI.display_solution (state)) //display solution on the screen
                          upper_text.draw_text("Q: main menu                            ",0,2,Color.White)
                          upper_text.draw_text("                                        ",0,3,Color.White)
                          0, 0
              | k, _ when k = QUIT_KEY ->
                          state.engine.clear () //clear the screen before exiting
                          0, 0
              | _ ->      0, 0
        //define new player position: move only if new coordinates are valid
        let new_player_position =
            if are_coordinates_valid state.expanded_maze.rows state.expanded_maze.cols ((fst state.player_position)+dy) ((snd state.player_position)+dx) && state.expanded_maze.map.[from_bidim_to_monodim state.expanded_maze.rows state.expanded_maze.cols ((fst state.player_position)+dy) ((snd state.player_position)+dx)] = Walls.OPEN then
                state.player_sprite.move_by(dx,dy) 
                (fst state.player_position)+dy,(snd state.player_position)+dx
            else state.player_position
        //check if reached the end
        if state.expanded_maze.finish = state.player_position  && not lock_input then
            winner state.engine state.expanded_maze.rows state.expanded_maze.cols //show victory banner
            ({state with player_position = new_player_position},true), key.KeyChar = QUIT_KEY    //return with lock_input enabled: do not accept further commands
        elif key.KeyChar = 'r' then
            ({state with player_position = new_player_position},true), key.KeyChar = QUIT_KEY    //return with lock_input enabled: do not accept further commands
        else
            ({state with player_position = new_player_position},lock_input || false), key.KeyChar = QUIT_KEY  //continue playing
    gui.engine.loop_on_key handle_user_interaction (gui,false)
    ()

///given the current game instance, uncover an area of the maze by the given range, by emptying pixels in the given mask sprite
let uncover_mask (mask:sprite) (maze: MazeGUIType) (range:int) :sprite =
    let player_row, player_column = maze.player_position
    let new_pxs = mask.pixels
    for i=player_row-range to player_row+range do
        for j = player_column-range to player_column + range do
            if are_coordinates_valid maze.expanded_maze.rows maze.expanded_maze.cols i j then
               new_pxs.[from_bidim_to_monodim maze.expanded_maze.rows maze.expanded_maze.cols i j] <- MazeGUI.EMPTY_PIXEL
    let new_mask = maze.engine.create_and_register_sprite (new image(maze.expanded_maze.cols, maze.expanded_maze.rows, new_pxs), MazeGUI.MAZE_X_OFFSET, MazeGUI.MAZE_Y_OFFSET, 3)
    //unregister old mask
    maze.engine.unregister_sprite mask
    //return new mask
    new_mask

///completely uncover the given mask.
let uncover_all (mask:sprite) (maze: MazeGUIType) :sprite =
    //create a new mask from scratch, completely empty
    let new_mask = maze.engine.create_and_register_sprite (new image(maze.expanded_maze.cols, maze.expanded_maze.rows, MazeGUI.EMPTY_PIXEL), MazeGUI.MAZE_X_OFFSET, MazeGUI.MAZE_Y_OFFSET, 3)
    //unregister old mask
    maze.engine.unregister_sprite mask
    //return new mask
    new_mask

///initialize an automatic solver demonstration with the given difficulty
let mode_automatic_resolution (difficulty:int) =
    let gui = MazeGUI.create_with_difficulty difficulty
    let upper_text = gui.engine.create_and_register_sprite (image.rectangle (MENU_WIDTH,MENU_HEIGHT, MazeGUI.EMPTY_PIXEL),1, 1, 1)
    upper_text.draw_text("Automatic Mode:",0,0,Color.White)
    upper_text.draw_text("Demonstration of the solver algorithm",0,1,Color.White)
    upper_text.draw_text("S: solve maze , Q: main menu",0,2,Color.White)
    let handle_user_interaction (key : ConsoleKeyInfo) (screen : wronly_raster) ((state : MazeGUIType) , (lock_input : bool)) =
        let lock_next_input =
            match key.KeyChar, lock_input with
                's', false -> ignore (MazeGUI.display_solution (state)) //display solution on the screen
                              upper_text.draw_text("Q: main menu                            ",0,2,Color.White)
                              true
              | k, _ when k = QUIT_KEY ->
                              state.engine.clear () //clear the screen
                              true
              | _ ->          lock_input
        (state, lock_next_input), key.KeyChar = QUIT_KEY
    gui.engine.loop_on_key handle_user_interaction (gui,false)
    ()

///initialize a dark labyrinth game with the given difficulty
let mode_dark_labyrinth (difficulty:int) =
    let gui = MazeGUI.create_with_difficulty (difficulty)
    let mutable mask = gui.engine.create_and_register_sprite (image.rectangle (gui.expanded_maze.cols, gui.expanded_maze.rows, MazeGUI.BLACK_PIXEL,MazeGUI.BLACK_PIXEL),MazeGUI.MAZE_X_OFFSET, MazeGUI.MAZE_Y_OFFSET, 3)
    mask <- uncover_mask mask gui PLAYER_VISIBILITY_RANGE
    let upper_text = gui.engine.create_and_register_sprite (image.rectangle (MENU_WIDTH,MENU_HEIGHT, MazeGUI.EMPTY_PIXEL),1, 1, 1)
    upper_text.draw_text("Dark Labyrinth Mode:",0,0,Color.White)
    upper_text.draw_text("You're the red dot. Exit the maze!",0,1,Color.White)
    upper_text.draw_text("W: up , A: left , S: down , D: right",0,2,Color.White)
    upper_text.draw_text("R: solve maze , Q: main menu",0,3,Color.White)

    let handle_user_interaction (key : ConsoleKeyInfo) (screen : wronly_raster) ((state : MazeGUIType), (lock_input : bool)) =
        let dx, dy =
            match key.KeyChar,lock_input with 
                _ , true -> 0,0 //don't move player if input is locked
              | 'w',_ -> 0, -1
              | 'a',_ ->  -1, 0
              | 'd',_ ->  1, 0
              | 's',_ ->  0, 1
              | 'r',_ ->  
                          ignore (MazeGUI.display_solution (state)) //display solution on the screen
                          mask <- uncover_all mask state      //remove the mask and show the whole maze
                          upper_text.draw_text("Q: main menu                            ",0,2,Color.White)
                          upper_text.draw_text("                                        ",0,3,Color.White)
                          0, 0
              | k, _ when k = QUIT_KEY ->
                          state.engine.clear () //clear the screen before exiting
                          0, 0
              | _ ->      0, 0
        //define new player position: move only if new coordinates are valid
        let new_player_position =
            if are_coordinates_valid state.expanded_maze.rows state.expanded_maze.cols ((fst state.player_position)+dy) ((snd state.player_position)+dx) && state.expanded_maze.map.[from_bidim_to_monodim state.expanded_maze.rows state.expanded_maze.cols ((fst state.player_position)+dy) ((snd state.player_position)+dx)] = Walls.OPEN then
                state.player_sprite.move_by(dx,dy)
                mask <- uncover_mask mask {gui with player_position= ((fst state.player_position)+dy,(snd state.player_position)+dx)} PLAYER_VISIBILITY_RANGE
                (fst state.player_position)+dy,(snd state.player_position)+dx
            else state.player_position
        //check if reached the end
        if state.expanded_maze.finish = state.player_position  && not lock_input then
            winner state.engine state.expanded_maze.rows state.expanded_maze.cols //show victory banner
            ({state with player_position = new_player_position},true), key.KeyChar = QUIT_KEY    //return with lock_input enabled: do not accept further commands
        elif key.KeyChar = 'r' then
            ({state with player_position = new_player_position},true), key.KeyChar = QUIT_KEY    //return with lock_input enabled: do not accept further commands
        else
            ({state with player_position = new_player_position},lock_input || false), key.KeyChar = QUIT_KEY  //continue playing
    gui.engine.loop_on_key handle_user_interaction (gui,false)
    ()
    


let start_menu () =     
    //render the menu
    let engine = render_menu ()
    let handle_menu_selection (key : ConsoleKeyInfo) (screen : wronly_raster) (st : int) =
        let st =
            //handle menu selection: run the corresponding routine to start a different game mode
            match key.KeyChar with 
            | '1' -> select_difficulty engine mode_interactive    //Interactive
                     ignore (render_menu ())
                     1
            | '2' -> select_difficulty engine mode_automatic_resolution  //Automatic Resolution
                     ignore (render_menu ())
                     2                          
            | '3' -> select_difficulty engine mode_dark_labyrinth         //Dark Labyrinth
                     ignore (render_menu ())
                     3                      
            | _   -> 0                              //Default: invalid selection
        st, key.KeyChar = QUIT_KEY

    //loop on menu selection
    engine.loop_on_key handle_menu_selection 0

// game mode (client)
//

let main_game () =
    use p = new Process ()
    p.StartInfo.UseShellExecute <- true
    p.StartInfo.CreateNoWindow <- false
    p.StartInfo.Arguments <- "1"
    p.StartInfo.FileName <- Process.GetCurrentProcess().MainModule.FileName
    ignore <| p.Start ()

    use client = new NamedPipeClientStream (".", Config.log_pipe_name, PipeDirection.Out)
    client.Connect ()
    Log <- new remote_logger (client)
    start_menu ()   //render the menu
    0

// log mode (server)
//

let main_log_server () =
    Log.msg "log server process started"
    Console.Title <- Config.log_console_title
    let server = new NamedPipeServerStream (Config.log_pipe_name, PipeDirection.In)
    Log.msg "waiting for incoming connection..."
    server.WaitForConnection ()
    Log.msg "client connected"
    use r = new StreamReader (server)
    while not r.EndOfStream do
        try
            let fg = r.ReadLine ()
            let parse c = Enum.Parse (typeof<Color>, c) :?> Color
            let s = r.ReadLine().Replace (Config.log_pipe_translate_eol, '\n')
            Console.ForegroundColor <- parse fg
            Console.WriteLine s
            Console.ResetColor ()
        with e -> Log.error "exception caught:%s\nstack trace: %O" e.Message e

    Log.warn "EOF reached: quitting."
    0

// main 
//

[<EntryPoint>]
let main argv = 
    #if TEST
    Test.main ()
    printfn "\nPress any key to quit..."
    Console.ReadKey () |> ignore
    0
    #else
    if argv.Length > 0 then 
        main_log_server ()
    else
        let code = main_game ()
        printfn "\nPress any key to quit..."
        Console.ReadKey () |> ignore
        code
    #endif


