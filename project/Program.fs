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
    let b = pixel.create(''', Color.Black)
    let menu = engine.create_and_register_sprite (image.rectangle (MENU_WIDTH,MENU_HEIGHT, b),0, 0, 0)
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


let mode_interactive (difficulty:int) =
    let gui = MazeGUI.create_with_difficulty (difficulty)
    let handle_user_interaction (key : ConsoleKeyInfo) (screen : wronly_raster) ((state : MazeGUIType), (lock_input : bool)) =
        let dx, dy =
            match key.KeyChar,lock_input with 
                _ , true -> 0,0 //don't move player if input is locked
              | 'w',_ -> 0, -1
              | 'a',_ ->  -1, 0
              | 'd',_ ->  1, 0
              | 's',_ ->  0, 1
              | k, _ when k = QUIT_KEY ->
                          state.engine.clear () //clear the screen before exiting
                          0, 0
              | _ ->   0, 0
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
        else
            ({state with player_position = new_player_position},false), key.KeyChar = QUIT_KEY  //continue playing
    gui.engine.loop_on_key handle_user_interaction (gui,false)
    ()

let mode_automatic_resolution (difficulty:int) =
    let gui = MazeGUI.create_with_difficulty difficulty
    let handle_user_interaction (key : ConsoleKeyInfo) (screen : wronly_raster) ((state : MazeGUIType) , (lock_input : bool)) =
        let lock_next_input =
            match key.KeyChar, lock_input with
                's', false -> ignore (MazeGUI.display_solution (state)) //display solution on the screen
                              true
              | k, _ when k = QUIT_KEY ->
                              state.engine.clear () //clear the screen
                              true
              | _ ->          false
        (state, lock_next_input), key.KeyChar = QUIT_KEY
    gui.engine.loop_on_key handle_user_interaction (gui,false)
    ()

let mode_dark_labyrinth (difficulty:int) = //TODO: Implement dark labyrinth
    let gui = MazeGUI.create_with_difficulty difficulty
    let handle_user_interaction (key : ConsoleKeyInfo) (screen : wronly_raster) ((state : MazeGUIType), (lock_input : bool)) =
        let dx, dy =
            match key.KeyChar,lock_input with 
                _ , true -> 0,0 //don't move player if input is locked
              | 'w',_ -> 0, -1
              | 'a',_ ->  -1, 0
              | 'd',_ ->  1, 0
              | 's',_ ->  0, 1
              | k, _ when k = QUIT_KEY ->
                          state.engine.clear () //clear the screen before exiting
                          0, 0
              | _ ->   0, 0
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
        else
            ({state with player_position = new_player_position},false), key.KeyChar = QUIT_KEY  //continue playing
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


