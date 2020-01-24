(*
* LabProg2019 - Progetto di Programmazione a.a. 2019-20
* Demo1.fs: sample usage of engine
* (C) 2019 Alvise Spano' @ Universita' Ca' Foscari di Venezia
*)

module LabProg2019.menu

open System
open Engine
open Gfx
    

[< NoEquality; NoComparison >]
type state = {
    //player : sprite
    home : sprite
    titolo : sprite
    }

let W = 60
let H = 30
let head= "Gruppo formato da: "
let Matteo = "Matteo Agnoletto" 
let matteo2 = "Matteo D "
let lorenzo = "Lorenzo Donatelli "

let s = head + "\n"+ Matteo + "\n"+matteo2+"\n"+lorenzo


let main () =       
    let engine = new engine (W, H)

    let my_update (key : ConsoleKeyInfo) (screen : wronly_raster) (st : state) =
        // move player
        let dx, dy =
            match key.KeyChar with 
            | '1' -> 0.,0.
            | _   -> 0., 0.
        // TODO: check bounds
       // st.player.move_by (dx, dy)
        st, key.KeyChar = 'q'

    let a = pixel.create(''', Color.Black, Color.Black)
    
    let b = pixel.create(''', Color.Black, Color.Black )
    // create simple backgroud and player
    (*ignore <|*)  
    let home = engine.create_and_register_sprite (image.rectangle (W, H, a,a), 0, 0, 0)
    //let player = engine.create_and_register_sprite (image.circle (2, pixel.filled Color.White, pixel.filled Color.Gray), W / 2, H / 2, 1)
    let titolo = engine.create_and_register_sprite (image.rectangle (W,H, b,b),3, 2, 1)
    titolo.draw_text("Maze labiritn ricorsivo  ", 7 , 5, Color.Red,Color.Blue)
    titolo.draw_text("Modalità di gioco :\n", 3 , 11, Color.White,Color.Green)
    titolo.draw_text("premere 1 : ricorsivo", 7 , 12, Color.Cyan,Color.Blue)
    titolo.draw_text("premere q : uscire", 7 , 13, Color.Gray ,Color.Blue)
    
    titolo.draw_text(s, 10 , 20, Color.White,Color.Blue)
     
    // initialize state
    let st0 = { 
       // player = player
       home = home
       titolo = titolo
           }
    // start engine
    engine.loop_on_key my_update st0

