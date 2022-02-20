open System.Windows.Forms
open NAudio

open EngineWindow
open EngineDevice
open EngineRenderer

open RenderSystem

let DEBUG = true

[<EntryPoint>]
let main _ =
    use window = new EngineWindow (600, 400, "Volcano")
    use device = new EngineDevice (window)
    use renderer = new EngineRenderer (window, device)
    let renderSystem = new RenderSystem (device, renderer.SwapchainRenderPass)
    
    let mutable state = AppState.newDefaultState ()

    // Create and set Update function
    let updateStateFunc () =
        if DEBUG then
            state <- {state with lastFrameTime = state.upTime.Elapsed.TotalSeconds}
        else
            // TODO: Put NAudio handling here?
            ()
    window.TockFunction <- Some updateStateFunc

    // Create and set Draw function
    let drawFunc () =
        match renderer.BeginFrame () with
        | Some commandBuffer ->
            renderer.BeginSwapchainRenderPass commandBuffer
            renderSystem.RenderGameObjects commandBuffer state renderer.AspectRatio
            renderer.EndSwapchainRenderPass commandBuffer
            renderer.EndFrame ()
        | None -> ()

        window.Invalidate ()    // Windows.Forms method to request another redraw
    window.DrawFunction <- Some drawFunc

    let handleKeyDown (args: KeyEventArgs) =
        match args.KeyCode with
        | Keys.Escape -> exit 0
        | Keys.F11 -> window.ToggleFullscreen ()
        | _ -> ()
    window.HandleKeyDown <- Some handleKeyDown

    // Execute the run-loop
    Application.Run window

    // When window is closed, wait for Vulkan to be ready for cleanup
    device.Device.WaitIdle ()

    0