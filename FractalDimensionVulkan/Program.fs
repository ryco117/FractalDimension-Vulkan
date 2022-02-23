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
    
    //TODO: !! Need to figure out a better way to ensure safety and performance between audio and graphics workers
    use stateMutex = new System.Threading.Mutex ()
    let mutable state = AppState.newDefaultState ()

    // UI Only
    let mutable distanceEstimate = AppState.IFS

    use audioOutCapture =
        // Audio local-state variables
        let mutable lastAngularChange = System.DateTime.UtcNow
        let previousBass: NAudio.Dsp.Complex[][] = Array.create 5 Array.empty
        let mutable previousBassIndex = 0

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
                    |> getStrongest 1 0.1f
                let midsNotes =
                    Array.sub complex midsStart (midsEnd - midsStart)
                    |> getStrongest 1 0.1f
                let highNotes =
                    Array.sub complex highStart (highEnd - highStart)
                    |> getStrongest 1 0.15f
                let volume = 
                    let summer a = Array.sumBy (fun n -> n.mag) a
                    summer bassNotes + summer midsNotes + summer highNotes

                // Resolve isolated notes to locations in 3D space
                let toWorldSpace {freq = f; mag = _} (range: NoteRange) =
                    let x =
                        let ff = float f
                        match range with
                        | Bass -> System.Math.Pow (ff, 0.8)
                        | Mids -> System.Math.Pow (ff, 0.65)
                        | High -> System.Math.Pow (ff, 0.4)
                    CubeFillingCurve.curveToCube x
                let pointFromNotes (notes: Note[]) (minimum: float32) (defaultPoint: Vector3) (range: NoteRange) =
                    match Array.tryFind (fun note -> note.mag > minimum) notes with
                    | Some note -> ((1.f - minimum / note.mag) / (1.f - minimum)) * toWorldSpace note range
                    | None -> defaultPoint
                let targetBass = pointFromNotes bassNotes config.minimumBass state.targetBass Bass
                let targetMids = pointFromNotes midsNotes config.minimumMids state.targetMids Mids
                let targetHigh = pointFromNotes highNotes config.minimumHigh state.targetHigh High

                // Update angular velocity on kick
                let angularVelocity =
                    let avgLastBassMag x =
                        let mutable s = 0.f
                        for i = 0 to previousBass.Length - 1 do
                            s <- s +
                                if previousBass[i].Length = 0 then
                                    0.f
                                else
                                    let j =
                                        let j = int (round (x * float32 previousBass[i].Length))
                                        if j >= previousBass[i].Length then previousBass[i].Length - 1 else j
                                    previousBass[i][j] |> mag
                        s / float32 previousBass.Length
                    match Array.tryFind (fun note ->
                        note.mag > config.minimumBassForJerk &&
                        let span = (System.DateTime.UtcNow - lastAngularChange) in span.TotalSeconds > 8. * float(config.minimumBassForJerk / note.mag) &&
                        note.mag > 5.f * avgLastBassMag note.freq) bassNotes with
                    | Some note ->
                        lastAngularChange <- System.DateTime.UtcNow
                        let p =
                            toWorldSpace note Bass
                            |> Vector3.Normalize
                        Vector4 (p.X, p.Y, p.Z, (sqrt volume) * config.autoOrbitJerk)
                    | None -> state.angularVelocity

                previousBass[previousBassIndex] <- bassArray
                previousBassIndex <- (previousBassIndex + 1) % previousBass.Length

                // Update state
                stateMutex.WaitOne () |> ignore
                state <- {state with angularVelocity = angularVelocity; volume = volume; targetBass = targetBass; targetMids = targetMids; targetHigh = targetHigh}
                stateMutex.ReleaseMutex ()

        let onClose () =
            stateMutex.WaitOne () |> ignore
            state <- {state with volume = 0.0001f}
            stateMutex.ReleaseMutex ()
        new EzSound.AudioOutStreamer (onDataAvail, onClose)

    // Create and set Update function
    let updateStateFunc () =
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
        let interpolateSmooths = interpolateNotePoints 17.f
        let smoothBass = interpolateSmooths state.pushConstants.smoothBass state.targetBass
        let smoothMids = interpolateSmooths state.pushConstants.smoothMids state.targetMids
        let smoothHigh = interpolateSmooths state.pushConstants.smoothHigh state.targetHigh

        // Update push constants
        let pushConstants = {
            state.pushConstants with 
                cameraQuaternion = cameraQuaternion; time = playTime; aspectRatio = renderer.AspectRatio; deIntType = distanceEstimate.ToInt ()
                reactiveBass = reactiveBass; reactiveMids = reactiveMids; reactiveHigh = reactiveHigh
                smoothBass = smoothBass; smoothMids = smoothMids; smoothHigh = smoothHigh}

        stateMutex.WaitOne () |> ignore
        state <- {state with pushConstants = pushConstants; angularVelocity = angularVelocity; previousFrameTime = time}
        stateMutex.ReleaseMutex ()

        // Check if audio capture has finished
        if state.audioResponsive && audioOutCapture.Stopped () then
            audioOutCapture.StartCapturing ()

    // Create and set Draw function
    let drawFunc () =
        match renderer.BeginFrame () with
        | Some commandBuffer ->
            updateStateFunc ()

            renderer.BeginSwapchainRenderPass commandBuffer
            renderSystem.RenderGameObjects commandBuffer state 
            renderer.EndSwapchainRenderPass commandBuffer
            renderer.EndFrame ()
        | None -> ()

        window.Invalidate ()    // Windows.Forms method to request another redraw
    window.DrawFunction <- Some drawFunc

    let handleKeyDown (args: KeyEventArgs) =
        match args.KeyCode with
        | Keys.Escape -> exit 0
        | Keys.F11 -> window.ToggleFullscreen ()
        | Keys.R ->
            let audio = state.audioResponsive
            if audio then
                if audioOutCapture.Capturing () then
                    audioOutCapture.StopCapturing ()
            else
                audioOutCapture.Reset ()

            stateMutex.WaitOne () |> ignore
            state <- {state with audioResponsive = not audio}
            stateMutex.ReleaseMutex ()
        | Keys.D0 ->
            distanceEstimate <- AppState.None
        | Keys.D1 ->
                distanceEstimate <- AppState.Mandelbox
        | Keys.D2 ->
            distanceEstimate <- AppState.Mandelbulb
        | Keys.D3 ->
            distanceEstimate <- AppState.Klienian
        | Keys.D4 ->
            distanceEstimate <- AppState.Menger
        | Keys.D5 ->
            distanceEstimate <- AppState.IFS
        | _ -> ()
    window.KeyDown.Add handleKeyDown

    // Execute the run-loop
    Application.Run window

    // When window is closed, wait for Vulkan to be ready for cleanup
    device.Device.WaitIdle ()
    0