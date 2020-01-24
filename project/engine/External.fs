(*
* LabProg2019 - Progetto di Programmazione a.a. 2019-20
* External.fs: external calls
* (C) 2019 Alvise Spano' @ Universita' Ca' Foscari di Venezia
*)

/// External types directly stubbing structs defined in Windows low-level API (kernel32.dll).
module LabProg2019.External

open System
open System.Runtime.InteropServices
open Microsoft.Win32.SafeHandles
open System.IO

#nowarn "9"
[< DllImport("kernel32.dll", SetLastError = true, CharSet = CharSet.Auto) >]
extern SafeFileHandle CreateFile(
    string fileName,
    [< MarshalAs(UnmanagedType.U4) >] uint32 fileAccess,
    [< MarshalAs(UnmanagedType.U4) >] uint32 fileShare,
    IntPtr securityAttributes,
    [< MarshalAs(UnmanagedType.U4) >] FileMode creationDisposition,
    [< MarshalAs(UnmanagedType.U4) >] int flags,
    IntPtr template)

/// This struct is actually a C-like union type representing either a Unicode 16-bit charachter or an ASCII 8-bit character on the same memory address.
[< StructLayout(LayoutKind.Explicit) >]
type CharUnion =
    struct
        [< FieldOffset(0) >] val mutable UnicodeChar : char
        [< FieldOffset(0) >] val mutable AsciiChar : byte
    end      

/// This struct represents a character cell with its character and colors. It directly stubs the CharInfo struct defined in Windows low-level API (kernel32.dll).
[< StructLayout(LayoutKind.Explicit); Diagnostics.DebuggerDisplay("{ToString()}") >]
type CharInfo =
  struct
    /// The character field of type CharUnion.
    [< FieldOffset(0) >] val mutable Char : CharUnion
    /// Extra attributes for additional informations: colors are encoded here.
    [< FieldOffset(2) >] val mutable Attributes : int16

    /// Get or set the foreground color.
    member this.fg
        with get () = Color.color_of_nibble this.Attributes
        and set c = this.Attributes <- (this.Attributes &&& 0xfff0s) ||| Color.nibble_of_color c

    /// Get or set the background color.
    member this.bg
        with get () = Color.color_of_nibble (this.Attributes >>> 4)
        and set c = this.Attributes <- (this.Attributes &&& 0xff0fs) ||| (Color.nibble_of_color c <<< 4)

    override this.ToString () = string this.Char.UnicodeChar

  end 

/// This struct represents screen areas as rectangles. Used by low-level console writing primitives for committing screen regions.
[< StructLayout(LayoutKind.Sequential) >]
type SmallRect =
    struct 
        val Left : int16
        val Top : int16
        val Right : int16
        val Bottom : int16
        new (l, t, r, b) = { Left = l; Top = t; Right = r; Bottom = b }
    end

/// This struct represents points as pairs of 16-bit integers. Used by low-level console writing primitives for committing screen regions.
[< StructLayout(LayoutKind.Sequential) >]
type Coord =
    struct
        val X : int16
        val Y : int16
        new (x, y) = { X = x; Y = y }
    end

/// This is the stub for the main console writing primitive. It commits a rectangular region of CharInfo on the system console using a low-latency high-performance algorithm.
[< DllImport("kernel32.dll", SetLastError = true) >]
extern bool WriteConsoleOutput(
    SafeFileHandle hConsoleOutput, 
    CharInfo[] lpBuffer, 
    Coord dwBufferSize, 
    Coord dwBufferCoord, 
    SmallRect& lpWriteRegion)


                        