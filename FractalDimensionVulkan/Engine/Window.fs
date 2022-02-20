module EngineWindow

open System.Diagnostics
open System.Windows.Forms
open Vulkan
open Vulkan.Windows

type EngineWindow (width: int, height: int, title: string) as self =
    inherit Form (Text = title, Size = System.Drawing.Size (width, height), FormBorderStyle = FormBorderStyle.Sizable)

    // Ensure user-defined paint-events and set opaque fill
    do self.SetStyle (ControlStyles.UserPaint + ControlStyles.Opaque, true)
    do self.UpdateStyles ()
    
    // Define optional event handlers
    let mutable (drawFunction: (unit -> unit) option) = None
    let mutable (tockFunction: (unit -> unit) option) = None

    let mutable fullscreen = false

    let primaryScreenDimensions () =
        let screen = Screen.PrimaryScreen.Bounds
        screen.Width, screen.Height

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

    member _.TockFunction
        with get () = tockFunction
        and set func = tockFunction <- func

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
        match tockFunction with
        | None -> ()
        | Some tockFunc -> tockFunc ()
        if self.Visible then
            match drawFunction with
            | Some drawFunc -> drawFunc ()
            | None -> ()