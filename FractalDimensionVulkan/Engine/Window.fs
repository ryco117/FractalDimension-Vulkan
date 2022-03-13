module EngineWindow

open System.Diagnostics
open System.Windows.Forms
open System.Runtime.InteropServices;

open Vulkan
open Vulkan.Windows

// Allow Win32 API to show hide the default application console
module NativeConsole =
    [<DllImport("kernel32.dll", CallingConvention = CallingConvention.Cdecl)>]
    extern nativeint GetConsoleWindow ()

    [<DllImport("user32.dll", CallingConvention = CallingConvention.Cdecl)>]
    extern bool ShowWindow (nativeint hWnd, int nCmdShow)

    let SW_HIDE = 0
    let SW_SHOW = 5

type EngineWindow (defaultWidth: int, defaultHeight: int, title: string) as self =
    inherit Form (Text = title, Size = System.Drawing.Size (defaultWidth, defaultHeight), FormBorderStyle = FormBorderStyle.Sizable)

    // Ensure user-defined paint-events and set opaque fill
    do self.SetStyle (ControlStyles.UserPaint + ControlStyles.Opaque, true)
    do self.UpdateStyles ()
    
    // Define configurable draw function
    let mutable (drawFunction: (unit -> unit) option) = None

    // Get properties of display containing majority of window
    let getWindowScreen () = Screen.GetBounds self

    // Hide cursor on window construction
    do Cursor.Hide ()

    let mutable disposed = false
    let cleanup () =
        if not disposed then
            disposed <- true
            Cursor.Show ()  // Probably not necessary, but API says to call in pairs

    // Allow for easy toggling of borderless-fullscreen
    let mutable fullscreen = false
    member _.IsFullscreen = fullscreen
    member _.ToggleFullscreen () =
        fullscreen <- not fullscreen
        if fullscreen then
            let displayBounds = getWindowScreen ()
            let width = uint32 displayBounds.Width
            let height = uint32 displayBounds.Height
            self.FormBorderStyle <- FormBorderStyle.None
            self.Extent <- Extent2D (Width = width, Height = height)
            self.Bounds <- displayBounds
        else
            self.FormBorderStyle <- FormBorderStyle.Sizable
            self.Extent <- Extent2D (Width = uint32 defaultWidth, Height = uint32 defaultHeight)

    // Create a Vulkan surface from the WinForm HWND and process HINSTANCE
    member this.CreateWindowSurface (instance: Instance) =
        let info = new Windows.Win32SurfaceCreateInfoKhr  (Hwnd = this.Handle, Hinstance = Process.GetCurrentProcess().Handle)
        instance.CreateWin32SurfaceKHR info

    member _.Extent
        with get () = let s = self.ClientSize in Extent2D (Width = uint32 s.Width, Height = uint32 s.Height)
        and set (extent: Extent2D) = self.ClientSize <- System.Drawing.Size (int extent.Width, int extent.Height)

    member _.DrawFunction
        with get () = drawFunction
        and set func = drawFunction <- func

    override _.OnPaintBackground _ = ()

    override _.OnPaint _args =
        match drawFunction with
        | Some drawFunc -> drawFunc ()
        | None -> ()

    interface System.IDisposable with override _.Dispose () = cleanup ()
    override _.Finalize () = cleanup ()