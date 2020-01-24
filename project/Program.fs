(*
* LabProg2019 - Progetto di Programmazione a.a. 2019-20
* Program.fs - game entry point
* (C) 2019 Alvise Spano' @ Universita' Ca' Foscari di Venezia
*)

module FMaze.Program

open System
open System.Diagnostics
open LabProg2019.Globals
open LabProg2019
open LabProg2019.Gfx
open System.IO.Pipes
open System.IO
open FMaze.GUI

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

    let main ()=
        let gui = MazeGUI.create 10 30
        let my_update (key : ConsoleKeyInfo) (screen : wronly_raster) (state : MazeGUIType) =
            // TODO: move player, handle moving in the maze and finish the game
            state, false //TODO: add condition to end the game
        gui.engine.loop_on_key my_update gui

    main ()
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


