(*
* LabProg2019 - Progetto di Programmazione a.a. 2019-20
* Engine.fs: game engine
* (C) 2019 Alvise Spano' @ Universita' Ca' Foscari di Venezia
*)

module LabProg2019.Engine

open System
open Gfx
open Globals
open System.Threading

// buffer type hierarchy
//

type private buffer (con : system_console_raster, ras : wronly_raster) =
    member this.lock_and_commit f = 
        lock this <| fun () ->
            let r = f ras
            lock con <| fun () -> ras.commit
            r

type private console_buffer (con, num) =
    inherit buffer (con, con)
    override __.ToString () = sprintf "console_buffer#%d" num

type private image_buffer (con, num) =
    inherit buffer (con, new image (con.width, con.height))
    override __.ToString () = sprintf "image_buffer#%d" num


// engine
//

type private 'st loop_data = { frame_cnt : int; elapsed : TimeSpan; now : DateTime; state : 'st; quit : bool }

/// This class provides the 2D ASCII engine.
/// Constructors picks width and height of the output console and optionally the frame-rate cap and the flip queue length.
/// A flip queue length of 0 implies no buffering and direct rendering on the system console.
/// The system console windows is automatically created when this class gets instantiated.
/// Calling one of the loop methods make the engine start its render loop.
/// Sprites can be registered for making the engine draw them each frame automatically in ascending z-order.
/// The order of operations for each frame are: clear buffer; render sprites; call user update function; commit frame; switch buffer.
type engine (w : int, h : int, ?fps_cap : int, ?flip_queue) =
    let fps_cap = defaultArg fps_cap Config.default_fps_cap
    let flip_queue = defaultArg flip_queue Config.default_flip_queue
    let con = new system_console_raster (w, h)
    let buffers : buffer[] =
        if flip_queue = 0 then [| new console_buffer (con, 0) |]
        else [| for i = 1 to flip_queue do yield new image_buffer (con, i - 1) |]
    let mutable sprites : sprite list = []
    
    do
        assert (flip_queue >= 0)
        Log.msg "initializing engine:\n\twidth=%d height=%d fps=%d\n\tbuffers=%d type=%s" w h fps_cap flip_queue (buffers.[0].GetType().Name)
        
    /// Get the width of the output console.
    member val screen_width = w
    /// Get the height of the output console.
    member val screen_height = h 

    /// Register the given sprite so that the engine knows it and renders it each frame according to its z value.
    member __.register_sprite (spr : sprite) =
        let len = lock sprites <| fun () ->
            sprites <- List.sortBy (fun spr -> spr.z) (spr :: sprites)  // sprites are always sorted in ascending order by z
            List.length sprites
        Log.msg "registered sprite #%d: x=%g y=%g z=%d width=%d height=%d" len spr.x spr.y spr.z spr.width spr.height

    /// Shortcut for creating and registering a sprite in one operation with a given image.
    member this.create_and_register_sprite (img, x, y, z) = let r = new sprite (img, x, y, z) in this.register_sprite r; r
    /// Shortcut for creating and registering a sprite in one operation with an empty image.
    member this.create_and_register_sprite (w, h, x, y, z) = this.create_and_register_sprite (new image (w, h), x, y, z)

    /// Flag for enabling/disabling automatic clearing of each frame (default = true).
    member val auto_clear = true with get, set
    /// Flag for enabling/disabling automatic rendering of sprites at each frame (default = true).
    member val show_sprites = true with get, set
    /// Flag for enabling/disabling fps and frame time information overlay (default = true in Debug and false in Release).
    member val show_fps = 
        #if DEBUG
        true
        #else
        false
        #endif
        with get, set

    member private this.shoot update (data : _ loop_data) =
        let buff = buffers.[data.frame_cnt % Array.length buffers]
        let (st', quit'), ts = stopwatch_quiet <| fun () ->
            buff.lock_and_commit <| fun wr ->
                if this.auto_clear then wr.clear
                let st', quit' = update wr data.state
                if this.show_sprites then lock sprites <| fun () -> for spr in sprites do spr.draw wr
                if this.show_fps then
                    let dt = DateTime.Now - data.now
                    let hd = sprintf "frame count: %d\nframe time: %.1f ms (%.1f fps)" data.frame_cnt data.elapsed.TotalMilliseconds (float data.frame_cnt / dt.TotalSeconds)
                    wr.draw_text (hd, 0, 0, Color.Yellow, Color.Blue)
                st', quit'
        { data with state = st'; frame_cnt = data.frame_cnt + 1; quit = quit'; elapsed = ts }

    static member private log_key_pressed (k : ConsoleKeyInfo) =
        Log.debug "engine: key pressed: %s" (let c = k.KeyChar in if Char.IsLetterOrDigit c then sprintf "'%c'" c else sprintf "ASCII: %d" (int c))

    /// Start the engine loop given a custom update function and an initial state.
    /// The update function is called every time a key is pressed on the system console; the key is passed as argument to the update function, together with the output wronly_raster and the state.
    /// Each call to the update function produces a boolean and a new state: the latter is passed to the subsequent call; the former tells the engine whether to quit or not the big loop.
    member this.loop_on_key update st0 =
        Log.msg "entering engine on-key loop..."
        let mutable data = { state = st0; frame_cnt = 0; quit = false; elapsed = new TimeSpan (); now = DateTime.Now }
        data <- this.shoot (fun _ st -> st, false) data
        while not data.quit do
            let k = Console.ReadKey true
            engine.log_key_pressed k
            data <- this.shoot (update k) data
        Log.msg "exiting engine loop-on-key."

    /// Start the engine loop given a custom update function and an initial state.
    /// The update function is called every I milliseconds, where I = 1000 / fps_cap, on a different thread, allowing advanced concurrent rendering strategies.
    /// A key may either have or not have been pressed, therefore a value of type 'ConsoleKeyInfo option' is passed to the update function, together with the output wronly_raster and the state.
    /// Each call to the update function produces a boolean and a new state: the latter is passed to the subsequent call; the former tells the engine whether to quit or not the big loop.
    member this.loop update st0 =
        let interval = 1000. / float fps_cap
        Log.msg "entering engine loop: timer interval=%g ms" interval
        use timer = new Timers.Timer (Interval = interval, Enabled = true, AutoReset = true)
        let now0 = DateTime.Now
        let data = new synced<_> { state = st0; frame_cnt = 0; quit = false; elapsed = new TimeSpan (); now = now0 }

        let h _ (args : Timers.ElapsedEventArgs) =
            data.apply_and_set <| fun r ->
                let ko =
                    if Console.KeyAvailable then
                        let k = Console.ReadKey true
                        engine.log_key_pressed k
                        Some k
                    else None
                let r = this.shoot (update ko) r
                if r.quit then timer.Stop ()
                r

        let handler = new Timers.ElapsedEventHandler (h)
        timer.Elapsed.AddHandler handler
        while data.apply (fun r -> not r.quit) do Thread.Sleep 500
        timer.Elapsed.RemoveHandler handler
        Log.msg "exiting engine loop."
        



        


