(*
* LabProg2019 - Progetto di Programmazione a.a. 2019-20
* Prelude.fs: misc stuff
* (C) 2019 Alvise Spano' @ Universita' Ca' Foscari di Venezia
*)

[< AutoOpen >]
module LabProg2019.Prelude

open System
open Printf
open System.Drawing


/// Convert an angle a in degrees into radians.
let deg2rad a = a * Math.PI / 180. 

/// Access a shared value in a thread-safe synchronized way.
type synced<'a> (v_ : 'a) =
    let mutable v = v_
    /// Apply the given function f to the synchronized value atomically.
    member this.apply f = lock this <| fun () -> f v
    /// Apply the given function f to the synchronized value and modify it atomically.
    member this.apply_and_set f = this.apply <| fun x -> v <- f x

/// Calculates the intersection between the two given rectangluar regions.
let clamp (x0, y0, w0, h0) (x1, y1, w1, h1) =
    let r1 = new Rectangle (x0, y0, w0, h0)
    let r2 = new Rectangle (x1, y1, w1, h1)
    r1.Intersect r2
    r1.Left, r1.Top, r1.Width, r1.Height

type ConsoleColor with
    /// Computes the dark version of a given Color.
    member col.darken =
        try
            let colenumty = col.GetType ()
            in
                ConsoleColor.Parse (colenumty, "dark" + ConsoleColor.GetName (colenumty, col), true) :?> ConsoleColor
        with _ -> if col = ConsoleColor.Black then ConsoleColor.DarkGray else ConsoleColor.Black

    /// Calculate a pair of Colors (foreground, backgroud) given a reference color and an integer representing the brightness in the range [0, 3].
    member col.shade_by_brightness n =
        let darkcol = col.darken
        let fg, bg =
            match n with
                | 0 -> darkcol, ConsoleColor.Black // darkest
                | 1 -> col, ConsoleColor.Black
                | 2 -> col, darkcol
                | 3 -> darkcol, col
                | _ -> ConsoleColor.White, darkcol  
        in
            fg, if fg = bg then ConsoleColor.Gray else bg

    /// Calculates the nearest ConsoleColor to the given RGB triple.
    static member of_rgb (r, g, b) = 
        let rec R delta z = function
            | [] -> z
            | cc :: ccs ->
                let n = Enum.GetName (typeof<ConsoleColor>, cc)
                let c = System.Drawing.Color.FromName (if n = "DarkYellow" then "Orange" else n)
                let t = Math.Pow (float (c.R - r), 2.0) + Math.Pow (float (c.G - g), 2.0) + Math.Pow (float (c.B - b), 2.0)
                in
                    if t = 0.0 then cc
                    elif t < delta then R t cc ccs
                    else R delta z ccs
        in
            R Double.MaxValue (new ConsoleColor ()) (Enum.GetValues typeof<ConsoleColor> |> Seq.cast |> Seq.toList)

    /// Convert a 4-bit nibble in the system console format to a ConsoleColor.
    static member color_of_nibble n =
        match n &&& 0x000fs with
        | 0b0000s -> ConsoleColor.Black
        | 0b0001s -> ConsoleColor.DarkBlue
        | 0b0010s -> ConsoleColor.DarkGreen
        | 0b0100s -> ConsoleColor.DarkRed
        | 0b0011s -> ConsoleColor.DarkCyan
        | 0b0110s -> ConsoleColor.DarkYellow
        | 0b0101s -> ConsoleColor.DarkMagenta
        | 0b0111s -> ConsoleColor.Gray
        | 0b1000s -> ConsoleColor.DarkGray
        | 0b1001s -> ConsoleColor.Blue
        | 0b1010s -> ConsoleColor.Green
        | 0b1100s -> ConsoleColor.Red
        | 0b1011s -> ConsoleColor.Cyan
        | 0b1110s -> ConsoleColor.Yellow
        | 0b1101s -> ConsoleColor.Magenta
        | 0b1111s -> ConsoleColor.White
        | x -> failwithf "unexpected color bit string: %x" x

    /// Convert a ConsoleColor to a 4-bit nibble in the system console format.
    static member nibble_of_color col =
        match col with
        | ConsoleColor.Black -> 0b0000s 
        | ConsoleColor.DarkBlue -> 0b0001s 
        | ConsoleColor.DarkGreen -> 0b0010s 
        | ConsoleColor.DarkRed -> 0b0100s 
        | ConsoleColor.DarkCyan -> 0b0011s 
        | ConsoleColor.DarkYellow -> 0b0110s 
        | ConsoleColor.DarkMagenta -> 0b0101s 
        | ConsoleColor.Gray -> 0b0111s 
        | ConsoleColor.DarkGray -> 0b1000s 
        | ConsoleColor.Blue -> 0b1001s 
        | ConsoleColor.Green -> 0b1010s 
        | ConsoleColor.Red -> 0b1100s 
        | ConsoleColor.Cyan -> 0b1011s 
        | ConsoleColor.Yellow -> 0b1110s 
        | ConsoleColor.Magenta -> 0b1101s 
        | ConsoleColor.White -> 0b1111s 
        | x -> failwithf "unexpected color: %O" x

/// Type alias for ConsoleColor.
type Color = ConsoleColor
    