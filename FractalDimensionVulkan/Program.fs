(*
FractalDimension - Experimental ray-marching based audio visualizer
Copyright (C) 2022  Ryan Andersen

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program. If not, see <https://www.gnu.org/licenses/>.
*)

open System.Numerics 
open System.Windows.Forms

open EngineWindow
open EngineDevice
open EngineRenderer

open AppConfig
open RenderSystem

let DEBUG = true

// Define type for storing a note
type NoteRange = Bass | Mids | High
type Note = {freq: float32; mag: float32}

// Define type for kaleidoscope animation state
type KaleidoscopeAnimationState = 
    | Inanimate
    | Animating of bool
    | Complete of bool

[<EntryPoint>]
let main args =
    let config = defaultConfigWithArgs args

    use window = new EngineWindow (800, 450, "FractalDimension")
    use device = new EngineDevice (window)
    use renderer = new EngineRenderer (window, device)

    (*use globalUboBuffer =
        new EngineBuffer.EngineBuffer (
        device, sizeof<AppState.GlobalUniformBufferObject>, EngineSwapchain.maxFramesInFlight, 
        Vulkan.BufferUsageFlags.UniformBuffer, Vulkan.MemoryPropertyFlags.HostVisible, 
        device.Properties.Limits.MinUniformBufferOffsetAlignment)*)

    let renderSystem = new RenderSystem (device, renderer.SwapchainRenderPass)
    
    let atomicState = AppState.AtomicState ()

    use audioOutCapture =
        // Pre-process audio with scaling factor
        let scaleSource (complex: NAudio.Dsp.Complex[]) =
            if config.volumeScale <> 1.f then
                Array.map (fun (z: NAudio.Dsp.Complex) ->
                    let mutable c = NAudio.Dsp.Complex ()
                    c.X <- config.volumeScale * z.X
                    c.Y <- config.volumeScale * z.Y
                    c) complex
            else
                complex

        let onDataAvail samplingRate (complex: NAudio.Dsp.Complex[]) =
            if complex.Length > 0 then
                fun (state: AppState.State) ->
                    let audio = atomicState.AudioOnlyState

                    // Apply pre-process function
                    let complex = scaleSource complex

                    // Define processing helpers
                    let mag (c: NAudio.Dsp.Complex) = sqrt (c.X*c.X + c.Y*c.Y)
                    let freqResolution = samplingRate / float complex.Length
                    let getStrongest maxCount delta (input: NAudio.Dsp.Complex[]) =
                        let fLen = float32 input.Length
                        let arr = Array.init input.Length (fun i -> {freq = (float32 i) / fLen; mag = mag input[i]})
                        let sorted = Array.sortWith (fun {freq = _; mag = a} {freq = _; mag = b} -> sign (b - a)) arr
                        let rec getList acc size (arr: Note[]) =
                            if arr.Length = 0  || size = maxCount then
                                acc
                            else
                                let t = arr[0].freq
                                let remaining, friends = Array.partition (fun {freq = s; mag = _} -> abs (t - s) > delta) arr
                                let m = Array.fold (fun acc {freq = _; mag = m} -> acc + m) 0.f friends
                                getList ({freq = t; mag = m}::acc) (size + 1) remaining
                        List.toArray (List.rev (getList [] 0 sorted))

                    // Convert frequency ranges to array indices
                    let frequencyToIndex f = int (round (f / freqResolution))
                    let bassStart = frequencyToIndex config.bassStartFreq
                    let bassEnd = frequencyToIndex config.bassEndFreq
                    let midsStart = frequencyToIndex config.midsStartFreq
                    let midsEnd = frequencyToIndex config.midsEndFreq
                    let highStart = frequencyToIndex config.highStartFreq
                    let highEnd = frequencyToIndex config.highEndFreq

                    // Determine strongest bins from each frequency range (bass/mids/high)
                    let bassArray = Array.sub complex bassStart (bassEnd - bassStart)
                    let bassNotes =
                        bassArray
                        |> getStrongest 2 0.125f
                    let midsNotes =
                        Array.sub complex midsStart (midsEnd - midsStart)
                        |> getStrongest 3 0.1f
                    let highNotes =
                        Array.sub complex highStart (highEnd - highStart)
                        |> getStrongest 3 0.1f
                    let bassVolume, midsVolume, highVolume = 
                        let summer = Array.sumBy (fun n -> n.mag)
                        summer bassNotes, summer midsNotes, summer highNotes
                    let volume = bassVolume + midsVolume + highVolume

                    // Resolve isolated notes to locations in 3D space
                    let toWorldSpace {freq = f; mag = _} (range: NoteRange) =
                        let x =
                            let ff = float f
                            match range with
                            | Bass -> System.Math.Pow (ff, 0.9)
                            | Mids -> System.Math.Pow (ff, 0.8)
                            | High -> System.Math.Pow (ff, 0.6)
                        CubeFillingCurve.curveToCube x
                    let pointFromNotes (notes: Note[]) (minimum: float32) (defaultPoint: Vector3) (range: NoteRange) volume =
                        if volume = 0.f then
                            defaultPoint
                        else
                            let summer note =
                                if note.mag > minimum then
                                    (note.mag / volume) * toWorldSpace note range
                                else
                                    (note.mag / volume) * defaultPoint
                            Array.sumBy summer notes
                    let targetBass = pointFromNotes bassNotes config.minimumBass state.targetBass Bass bassVolume
                    let targetMids = pointFromNotes midsNotes config.minimumMids state.targetMids Mids midsVolume
                    let targetHigh = pointFromNotes highNotes config.minimumHigh state.targetHigh High highVolume

                    // Update angular velocity on kick
                    let angularVelocity, lastChange =
                        let avgLastBassMag x =
                            let mutable s = 0.f
                            for i = 0 to audio.previousBass.Length - 1 do
                                s <- s +
                                    if audio.previousBass[i].Length = 0 then
                                        0.f
                                    else
                                        let j =
                                            let j = int (round (x * float32 audio.previousBass[i].Length))
                                            if j >= audio.previousBass[i].Length then audio.previousBass[i].Length - 1 else j
                                        audio.previousBass[i][j] |> mag
                            s / float32 audio.previousBass.Length
                        match Array.tryFind (fun note ->
                            note.mag > config.minimumBassForJerk &&
                            let span = (System.DateTime.UtcNow - audio.lastAngularChange) in span.TotalSeconds > 8. * float(config.minimumBassForJerk / note.mag) &&
                            note.mag > 5.f * avgLastBassMag note.freq) bassNotes with
                        | Some note ->
                            let p =
                                toWorldSpace note Bass
                                |> Vector3.Normalize
                            Vector4 (p.X, p.Y, p.Z, (sqrt volume) * config.autoOrbitJerk), System.DateTime.UtcNow
                        | None -> state.angularVelocity, audio.lastAngularChange

                    atomicState.AudioOnlyState <-
                        {audio with 
                            lastAngularChange = lastChange
                            previousBass = Array.mapi (fun i arr -> if i = audio.previousBassIndex then bassArray else arr) audio.previousBass
                            previousBassIndex = (audio.previousBassIndex + 1) % audio.previousBass.Length}

                    // Return updated state
                    {state with angularVelocity = angularVelocity; volume = volume; targetBass = targetBass; targetMids = targetMids; targetHigh = targetHigh}
                |> atomicState.SetState

        let onClose () =
            fun (state: AppState.State) ->
                {state with volume = 0.0001f}
            |> atomicState.SetState
        new EzSound.AudioOutStreamer (onDataAvail, onClose)

    // Create and set Update function
    let updateStateFunc () =
        let userState = atomicState.UserInterfaceOnlyState

        fun (state: AppState.State) ->
            let time = state.upTime.Elapsed.TotalSeconds
            let deltaTime =
                time - state.previousFrameTime
                |> float32

            let playTime = state.pushConstants.time + System.MathF.Pow (state.volume, 0.75f) * deltaTime

            // Update the rotation amd angular momentum of the camera
            let cameraQuaternion, angularVelocity =
                let omega = state.angularVelocity
                let w = omega.W
                let r =
                    Vector4 (omega.X, omega.Y, omega.Z, w * deltaTime)
                    |> EngineMaths.buildQuaternion
                Vector4.Normalize (EngineMaths.quaternionMultiply state.pushConstants.cameraQuaternion r), Vector4 (omega.X, omega.Y, omega.Z, w + (AppState.autoOrbitSpeed - w) * (1.f - exp (-deltaTime/2.75f)))

            // Update position of note-vectors
            let interpolateNotePoints (scale: float32) (source: Vector3) (target: Vector3) =
                let smooth = (1.f - exp (deltaTime / -scale))
                source + (target - source) * smooth
            let interpolateReactives = interpolateNotePoints 2.25f
            let reactiveBass = interpolateReactives state.pushConstants.reactiveBass state.targetBass
            let reactiveMids = interpolateReactives state.pushConstants.reactiveMids state.targetMids
            let reactiveHigh = interpolateReactives state.pushConstants.reactiveHigh state.targetHigh
            let interpolateSmooths = interpolateNotePoints 18.5f
            let smoothBass = interpolateSmooths state.pushConstants.smoothBass state.targetBass
            let smoothMids = interpolateSmooths state.pushConstants.smoothMids state.targetMids
            let smoothHigh = interpolateSmooths state.pushConstants.smoothHigh state.targetHigh

            // Update animation for kaleidoscope effect
            let kaleido, kaleidoAnimationState =
                let omega = config.kaleidoscopeSpeed
                match userState.kaleidoscope with
                | None, true -> 1.f, Inanimate
                | None, false -> 0.f, Inanimate
                | Some k, true ->
                    let tmp =
                        k + omega * deltaTime * System.MathF.Pow (state.volume, 0.7f)
                        |> min 1.f
                    tmp, if tmp = 1.f then Complete true else Animating true
                | Some k, false ->
                    let tmp =
                        k - omega * deltaTime * System.MathF.Pow (state.volume, 0.7f)
                        |> max 0.f
                    tmp, if tmp = 0.f then Complete false else Animating false

            // Update kaleidoscope state with new calculation
            let newKaleidoscope =
                match kaleidoAnimationState with
                | Complete dir -> None, dir
                | Animating dir -> Some kaleido, dir
                | Inanimate -> userState.kaleidoscope

            // Update mouse visibility state
            let newLastMove =
                match userState.lastMouseMovement with
                | x, y, lastMove, false ->
                    if (System.DateTime.UtcNow - lastMove).TotalSeconds > 2. then
                        Cursor.Hide ()
                        x, y, lastMove, true
                    else
                        x, y, lastMove, false
                | lastMouseMovement -> lastMouseMovement

            atomicState.UserInterfaceOnlyState <- {userState with kaleidoscope = newKaleidoscope; lastMouseMovement = newLastMove}

            // Update push constants
            let pushConstants = {
                state.pushConstants with 
                    cameraQuaternion = cameraQuaternion; time = playTime; aspectRatio = renderer.AspectRatio
                    deIntType = userState.distanceEstimate.ToInt (); kaleido = System.MathF.Pow (kaleido, 0.675f)
                    reactiveBass = reactiveBass; reactiveMids = reactiveMids; reactiveHigh = reactiveHigh
                    smoothBass = smoothBass; smoothMids = smoothMids; smoothHigh = smoothHigh}

            {state with pushConstants = pushConstants; angularVelocity = angularVelocity; previousFrameTime = time}
        |> atomicState.SetState

        // Check if audio capture has finished
        if atomicState.UserInterfaceOnlyState.audioResponsive && audioOutCapture.Stopped () then
            audioOutCapture.StartCapturing ()

    // Create and set Draw function
    let drawFunc () =
        match renderer.BeginFrame () with
        | Some commandBuffer ->
            updateStateFunc ()

            renderer.BeginSwapchainRenderPass commandBuffer

            // Use state in 
            renderSystem.RenderGameObjects commandBuffer
            |> atomicState.UseState

            // Finish render pass
            renderer.EndSwapchainRenderPass commandBuffer
            renderer.EndFrame ()
        | None -> ()

        window.Invalidate ()    // Windows.Forms method to request another redraw
    window.DrawFunction <- Some drawFunc

    // Handle key events for user input
    fun (args: KeyEventArgs) ->
        let userState = atomicState.UserInterfaceOnlyState
        match args.KeyCode with
        | Keys.Escape -> exit 0
        | Keys.F11 -> window.ToggleFullscreen ()
        | Keys.R ->
            let audio = userState.audioResponsive
            if audio then
                if audioOutCapture.Capturing () then
                    audioOutCapture.StopCapturing ()
            else
                audioOutCapture.Reset ()

            atomicState.UserInterfaceOnlyState <- {userState with audioResponsive = not audio}
        | Keys.D0 ->
            atomicState.UserInterfaceOnlyState <- {userState with distanceEstimate = AppState.InfiniteDistance}
        | Keys.D1 ->
            atomicState.UserInterfaceOnlyState <- {userState with distanceEstimate = AppState.Mandelbox}
        | Keys.D2 ->
            atomicState.UserInterfaceOnlyState <- {userState with distanceEstimate = AppState.Mandelbulb}
        | Keys.D3 ->
            atomicState.UserInterfaceOnlyState <- {userState with distanceEstimate = AppState.Klienian}
        | Keys.D4 ->
            atomicState.UserInterfaceOnlyState <- {userState with distanceEstimate = AppState.Menger}
        | Keys.D5 ->
            atomicState.UserInterfaceOnlyState <- {userState with distanceEstimate = AppState.IFS}
        | Keys.Space ->
            atomicState.UserInterfaceOnlyState <-
                {userState with kaleidoscope = 
                                match userState.kaleidoscope with
                                | None, dir -> Some (if dir then 1.f else 0.f), not dir
                                | Some k, dir  -> Some k, not dir}
        | _ -> ()
    |> window.KeyDown.Add

    // Handle mouse movements to accurately show/hide cursor with user activity
    fun (args: MouseEventArgs) ->
        let userState = atomicState.UserInterfaceOnlyState
        let x, y, _, hidden = userState.lastMouseMovement
        if hidden && (args.X <> x || args.Y <> y) then
            atomicState.UserInterfaceOnlyState <- {userState with lastMouseMovement = args.X, args.Y, System.DateTime.UtcNow, false}
            Cursor.Show ()
    |> window.MouseMove.Add

    // If made it here without error, hide the console
    let consoleHwnd = NativeConsole.GetConsoleWindow ()
    NativeConsole.ShowWindow (consoleHwnd, NativeConsole.SW_HIDE) |> ignore

    // Execute the run-loop
    Application.Run window

    // Reshow console just because?
    NativeConsole.ShowWindow (consoleHwnd, NativeConsole.SW_SHOW) |> ignore

    // When window is closed, wait for Vulkan to be ready for cleanup
    device.Device.WaitIdle ()
    0