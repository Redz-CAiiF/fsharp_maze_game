(*
* LabProg2019 - Progetto di Programmazione a.a. 2019-20
* Gfx.fs: graphics stuff
* (C) 2019 Alvise Spano' @ Universita' Ca' Foscari di Venezia
*)

module LabProg2019.Gfx

open System
open Globals
open System.Drawing
open Prelude
open External
open System.IO
open System.Text

// pixel type
//

type CharInfo with
    /// Create a new CharInfo given a character, a foreground color and a background color. Background color argument is optional and defaults to black.
    static member create (c : char, fg : Color, ?bg : Color) = 
        let bg = defaultArg bg Color.Black
        let mutable ci = new CharInfo ()
        ci.Char.UnicodeChar <- c
        ci.fg <- fg
        ci.bg <- bg
        ci

    /// Shortcut for creating a filled cell given the foreground and background colors.
    static member filled (fg : Color, ?bg : Color) = CharInfo.create (Config.filled_pixel_char, fg, ?bg = bg) 
    /// Shortcut for creating an empty cell given the foreground and background colors.
    static member empty = CharInfo.create (Config.empty_pixel_char, Color.White) 
    /// Tests whether this is an empty character cell.
    member this.is_empty = this.Char.UnicodeChar = Config.empty_pixel_char

/// Alias for the pixel type.
type pixel = CharInfo


       
/// Class representing write-only rasters providing basic plotting, drawing and blitting primitives. No read operations are available.
/// Width and height are given as constructor arguments.
[< AbstractClass; NoEquality; NoComparison; Diagnostics.DebuggerDisplay("{ToString()}") >]
type wronly_raster (w, h) =
    /// The width of this raster.
    abstract member width : int
    /// The height of this raster.
    abstract member height : int
    default __.width = w
    default __.height = h

    /// Low-level unsafe plot primitive. Unsafe means no boundary check is performed, thus failing at runtime when coordinates are out of boundaries.
    abstract member unsafe_plot : int * int * pixel -> unit

    /// Item setter is a shortcut to unsafe_plot. No getter available.
    member inline this.Item 
        with set (x, y) px = this.unsafe_plot (x, y, px)

    /// Clear this raster setting all pixels to empty.
    abstract member clear : unit
    default this.clear =
        for y = 0 to this.height - 1 do
            for x = 0 to this.width - 1 do
                this.[x, y] <- pixel.empty

    /// Commit this raster to the system console.
    abstract member commit : unit

    /// Check whether the given point (x, y) is contained within the boundaries of this raster.
    member this.is_inside (x, y) =
        let f v m = v >= 0 && v < m
        in
            f x this.width && f y this.height

    /// Check whether the given rectangular region (x, y, w, h) is contained within the boundaries of this raster.
    member this.is_inside (x, y, w, h) = this.is_inside (x, y) && this.is_inside (x + w - 1, y + h - 1)

    /// Plot the given pixel at the given coordinates. If coordinates are out of boundaries then nothing is plotted.
    member this.plot (x, y, px) =
        if this.is_inside (x, y) then 
            this.[x, y] <- px

    /// Draw the given text at the given coordinates with the given colors. The font is fixed size and OS-dependant.
    abstract draw_text : string * int * int * Color * ?bg:Color -> unit
    default this.draw_text (s, x, y, fg, ?bg) =
        let mutable dx = 0
        let mutable dy = 0
        for i = 0 to s.Length - 1 do
            match s.Chars i with
            | '\n'  -> dy <- dy + 1
                       dx <- 0
            | c     -> this.plot (x + dx, y + dy, pixel.create (c, fg, ?bg = bg))
                       dx <- dx + 1
      
    /// Draw a line with the Bresenham algorithm plotting each dot with the given pixel. The line starts at (x0, y0) and ends at (x1, y1).
    member this.draw_line (x0, y0, x1, y1, px) =
        let plot x y = this.plot (x, y, px)
        let steep = abs (y1 - y0) > abs (x1 - x0)
        let x0, y0, x1, y1 = if steep then y0, x0, y1, x1 else x0, y0, x1, y1
        let x0, y0, x1, y1 = if x0 > x1 then x1, y1, x0, y0 else x0, y0, x1, y1
        let dx, dy = x1 - x0, abs (y1 - y0)
        let s = if y0 < y1 then 1 else -1
        let rec R e x y =
            if x <= x1 then
                if steep then plot y x else plot x y
                if e < dy then R (e - dy + dx) (x + 1) (y + s)
                else R (e - dy) (x + 1) y
        in
            R (dx / 2) x0 y0

    /// Draw a rectangle with (x0, y0) as top-left corner and (x0 + w - 1, y0 + h - 1) as bottom-right corner.
    member this.draw_rectangle (x0, y0, w, h, px) =
        let x1, y1 = x0 + w - 1, y0 + h - 1
        this.draw_line (x0, y0, x1, y0, px)
        this.draw_line (x0, y0, x0, y1, px)
        this.draw_line (x1, y1, x1, y0, px)
        this.draw_line (x1, y1, x0, y1, px)
              
    /// Draw a circle with (x0, y0) as center and r as ray, using px as pixel.
    member this.draw_circle (x0, y0, r, px) =
        let plot x y = this.plot (x, y, px)
        let x = 0
        let y = r
        let m = 5 - 4 * r
        let rec loop x y m =
            plot (x0 + x) (y0 + y)
            plot (x0 + y) (y0 + x)
            plot (x0 - x) (y0 + y)
            plot (x0 - y) (y0 + x)
            plot (x0 + x) (y0 - y)
            plot (x0 + y) (y0 - x)
            plot (x0 - x) (y0 - y)
            plot (x0 - y) (y0 - x)
            let y, m =
                if m > 0 then (y - 1), (m - 8 * y)
                else y, m
            if x <= y then
              let x = x + 1 in
              let m = m + 8 * x + 4 in
              loop x y m      
        loop x y m

    override this.ToString () = sprintf "wronly_raster (%d, %d)" this.width this.height
        

/// Subclass extending wronly_raster with read operations.
/// Width and height are given as constructor arguments.
[< AbstractClass; Diagnostics.DebuggerDisplay("{ToString()}") >]
 type raster (w, h) =
    inherit wronly_raster (w, h)

    /// Low level unsafe read access. Unsafe means no boundary check is performed, thus failing at runtime when coordinates are out of boundaries.
    abstract member unsafe_get : int * int -> pixel

    /// Item getter and setter are shortcuts to unsafe_get and unsafe_plot respectively. 
    member inline this.Item 
        with get (x, y) = this.unsafe_get (x, y)
        and set (x, y) px = this.unsafe_plot (x, y, px)  // setter must be reimplemented in subclass because the whole property definition shadows the inherited one

    /// Fill algorithm starting from the given point (x, y) and flooding the region consisting of contiguous empty pixels with the given argument fill_px.
    member this.flood_fill (x, y, fill_px) =
        if x < 0 || x >= this.width then ()
        elif y < 0 || y >= this.height then ()
        elif this.[x, y].is_empty then 
            this.[x, y] <- fill_px
            this.flood_fill (x + 1, y, fill_px)
            this.flood_fill (x, y + 1, fill_px)
            this.flood_fill (x - 1, y, fill_px)
            this.flood_fill (x, y - 1, fill_px)       

    /// Reads a pixel value at the given coordinates. If coordinates are out of boundaries then nothing is plotted.
    member this.get (x, y) =
        if this.is_inside (x, y) then this.[x, y]
        else pixel.empty

    /// Blit a rectangular region of pixels from this source to the destination raster.
    /// Expression src.unsafe_blit (x0, y0, w, h, dst, x1, y1) blits from source raster src to destination wronly_raster dst, copying the rectangular region with top-left corner at (x0, y0) and bottom-right corner at (x0 + w - 1, y0 + h -1) to destination coordinates (x1, y1).
    /// This method does not check boundaries.
    abstract unsafe_blit : int * int * int * int * wronly_raster * int * int -> unit
    default src.unsafe_blit (x0, y0, w, h, dst, x1, y1) =
        let inline p xi yi dx dy =
            let px = src.[x0 + xi + dx, y0 + yi + dy]
            if not px.is_empty then dst.[x1 + xi, y1 + yi] <- px 
        if y1 <= y0 || x1 <= x0 then
            for yi = 0 to h - 1 do
                for xi = 0 to w - 1 do
                    p xi yi (src.width - w) (src.height - h)
        else
            for yi = 0 to h - 1 do
                for xi = 0 to w - 1 do                    
                    p xi yi 0 0

    /// Blit the whole source raster src to the destination wronly_raster dst at given point (x1, y1). Clamping is performed and a sub-region of the specified area may be actually blitted.
    member src.blit (dst : wronly_raster, x1, y1) = src.blit (0, 0, src.width, src.height, dst, x1, y1)

    /// Blit the given region (x0 + w - 1, y0 + h -1) of the source raster src to the destination wronly_raster dst at given point (x1, y1). Clamping is performed and a sub-region of the specified area may be actually blitted.
    member src.blit (x0, y0, w, h, dst, x1, y1) =
        let x0', y0', w0, h0 = clamp (0, 0, src.width, src.height) (x0, y0, w, h)
        let x1', y1', w1, h1 = clamp (0, 0, dst.width, dst.height) (x1, y1, w, h)
        let w', h' = min w0 w1, min h0 h1
        src.unsafe_blit (x0', y0', w', h', dst, x1', y1')
    
    override src.commit =
        src.commit_from [|
            for y = 0 to src.height - 1 do
                for x = 0 to src.width - 1 do
                    yield src.[x, y]
            |]

    /// Commit the given buffer to the system console.
    member internal __.commit_from buff =
        let mutable rect = new SmallRect (0s, 0s, int16 w, int16 h)
        let handle = CreateFile ("CONOUT$", 0x40000000u, 2u, IntPtr.Zero, FileMode.Open, 0, IntPtr.Zero)
        ignore <| WriteConsoleOutput (handle, buff, new Coord (int16 w, int16 h), new Coord (0s, 0s), &rect)

    override this.ToString () =
        let sb = new StringBuilder ()
        for y = 0 to this.height - 1 do
            for x = 0 to this.width - 1 do
                ignore <| sb.Append (this.[x, y].ToString ())
        sb.ToString ()


/// Class representing the system console as a wronly_raster.
/// Width and height are given as constructor arguments.
type system_console_raster (w, h) =
    inherit wronly_raster (w, h)
    let w = min w Console.LargestWindowWidth
    let h = min h (Console.LargestWindowHeight - 1)
    do
        Console.Title <- sprintf "%s (%d x %d)" Config.game_console_title w h
        Console.CursorVisible <- false
        Console.OutputEncoding <- Text.Encoding.Unicode
        Console.SetWindowSize (w, h + 1)    // +1 on both to prevent console scrolling down after commit
        Console.SetBufferSize (w, h + 1)
        Log.msg "Console info:\n\tBufferWidth = %d\n\tBufferHeight = %d\n\tWindowWidth = %d\n\tWindowHeight = %d\n"
            Console.BufferWidth Console.BufferHeight Console.WindowWidth Console.WindowHeight

    /// Set the cursor position at coordinates (x, y) and set the current colors.
    static member internal at (x, y, fg, ?bg) =
        Console.SetCursorPosition (x, y)
        Console.ForegroundColor <- fg
        Option.iter (fun bg -> Console.BackgroundColor <- bg) bg

    override __.clear = Console.Clear ()        

    override __.unsafe_plot (x, y, px) =
        system_console_raster.at (x, y, px.fg, px.bg)
        Console.Write px.Char.UnicodeChar

    override __.draw_text (s, x, y, fg, ?bg) =
        system_console_raster.at (x, y, fg, ?bg = bg)
        Console.Write s

    override __.commit = ()


/// Class representing images as a subclass of raster.
/// Constructor arguments are width, height and the 1-dimensional array of pixels with the image data.
[< Diagnostics.DebuggerDisplay("{ToString()}") >]
type image (w, h, pixels : pixel[]) =
    inherit raster (w, h)

    do assert (w * h = pixels.Length)

    /// Alternate constructor creating a new pixel array initialized with empty pixels.
    new (w, h, ?px) = new image (w, h, Array.create (w * h) (defaultArg px pixel.empty))

    /// Retrieve the pixels as a 1-dimensional array.
    member val internal pixels = pixels

    override __.unsafe_get (x, y) = pixels.[y * w + x]
    override __.unsafe_plot (x, y, px) = pixels.[y * w + x] <- px
            
    override this.commit = this.commit_from pixels

    /// Create a new image object with a circle of ray r drawn at the center of it. The resulting image is a square of size r * 2 + 1.
    /// The border of the circle is drawn using argument px; if the optional argument filled_px is passed as well, the circle gets flood-filled with that.
    static member circle (r, px : pixel, ?filled_px) =
        let d = r * 2 + 1
        let c = d / 2
        let i = new image (d, d)
        i.draw_circle (c, c, r, px)
        Option.iter (fun px -> i.flood_fill (c, c, px)) filled_px
        i
        
    /// Create a new image object with a rectangle drawn in it. The resulting image is w * h pixels in size.
    /// The border of the rectangle is drawn using argument px; if the optional argument filled_px is passed as well, the rectangle gets flood-filled with that.
    static member rectangle (w, h, px, ?filled_px) =
        let i = new image (w, h)
        i.draw_rectangle (0, 0, w, h, px)
        Option.iter (fun px -> i.flood_fill (i.width / 2, i.height / 2, px)) filled_px
        i


/// Subclass of image representing sprites. Sprites are images that can have a location and can be moved.
/// Constructor parameters are the image, coordinates x, y and an integer z that is the order by which sprites are rendered, in ascending order (lower z means more behind, higher z means more in front).
/// Coordinates x and y are stored as floats, allowing fine precision movement of sprites.
type sprite (img : image, x_ : int, y_ : int, z_ : int) =    
    inherit image (img.width, img.height, img.pixels)

    /// Get or set the x coordinate of this sprite as a float.
    member val x : float = float x_ with get, set
    /// Get or set the y coordinate of this sprite as a float.
    member val y : float = float y_ with get, set
    /// Get or set the z value of this sprite.
    member val z = z_ with get, set

    /// Recalculate the x and y coordinates of this sprite given a pair of floats (dx, dy) representing the horizontal and vertical offsets.
    member this.move_by (dx, dy) =
        this.x <- this.x + dx
        this.y <- this.y + dy

    /// Recalculate the x and y coordinates of this sprite given a pair of integers (dx, dy) representing the horizontal and vertical offsets.
    member this.move_by (dx, dy) = this.move_by (float dx, float dy)
    
    /// Draw this sprite onto the given wronly_raster. Clamping may take place.
    member spr.draw wr = spr.blit (wr, int spr.x, int spr.y)


/// Class respresenting a subregion of the parent raster.
/// Constructor arguments are the parent raster and the subregion coordinates and size relative to the parent coordinates.
type region (parent : raster, x0, y0, w, h) =
    inherit raster (w, h)
    
    override __.unsafe_get (x, y) = parent.[x0 + x, y0 + y]
    override __.unsafe_plot (x, y, px) = parent.[x0 + x, y0 + y] <- px

    /// Get the parent raster.
    member val parent = parent

type raster with
    /// Create a region object from this raster as parent raster given the area coordinates.
    member this.region (x, y, w, h) = new region (this, x, y, w, h)
