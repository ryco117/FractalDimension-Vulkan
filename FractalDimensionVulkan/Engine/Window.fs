module EngineWindow

open System.Diagnostics
open System.Windows.Forms
open System.Runtime.InteropServices;

open Vulkan
open Vulkan.Windows

[<DllImport("kernel32.dll", CallingConvention = CallingConvention.Cdecl)>]
extern nativeint GetConsoleWindow ()

[<DllImport("user32.dll", CallingConvention = CallingConvention.Cdecl)>]
extern bool ShowWindow (nativeint hWnd, int nCmdShow)

let SW_HIDE = 0
let SW_SHOW = 5

type EngineWindow (width: int, height: int, title: string) as self =
    inherit Form (Text = title, Size = System.Drawing.Size (width, height), FormBorderStyle = FormBorderStyle.Sizable)

    // Ensure user-defined paint-events and set opaque fill
    do self.SetStyle (ControlStyles.UserPaint + ControlStyles.Opaque, true)
    do self.UpdateStyles ()
    
    // Define configurable draw function
    let mutable (drawFunction: (unit -> unit) option) = None

    let mutable fullscreen = false

    let primaryScreenDimensions () =
        let screen = Screen.PrimaryScreen.Bounds
        screen.Width, screen.Height

    // TODO: Hide/Show cursor on a timer
    do Cursor.Hide ()

    let mutable disposed = false
    let cleanup () =
        if not disposed then
            disposed <- true
            Cursor.Show ()

    member this.CreateWindowSurface (instance: Instance) =
        let info = new Windows.Win32SurfaceCreateInfoKhr  (Hwnd = this.Handle, Hinstance = Process.GetCurrentProcess().Handle)
        instance.CreateWin32SurfaceKHR info

    member _.Extent
        with get () = let s = self.ClientSize in Extent2D (Width = uint32 s.Width, Height = uint32 s.Height)
        and set (extent: Extent2D) =
            self.ClientSize <- System.Drawing.Size (int extent.Width, int extent.Height)

    member _.DrawFunction
        with get () = drawFunction
        and set func = drawFunction <- func

    member _.ToggleFullscreen () =
        fullscreen <- not fullscreen
        if fullscreen then
            let width, height = primaryScreenDimensions ()
            self.FormBorderStyle <- FormBorderStyle.None
            self.WindowState <- FormWindowState.Normal
            self.Extent <- Extent2D (Width = uint32 width, Height = uint32 height)
            self.Bounds <- Screen.PrimaryScreen.Bounds
        else
            self.FormBorderStyle <- FormBorderStyle.Sizable
            self.Extent <- Extent2D (Width = uint32 width, Height = uint32 height)

    override _.OnPaintBackground _ = ()

    override _.OnPaint _args =
        if self.Visible then
            match drawFunction with
            | Some drawFunc -> drawFunc ()
            | None -> ()

    interface System.IDisposable with override _.Dispose () = cleanup ()
    override _.Finalize () = cleanup ()