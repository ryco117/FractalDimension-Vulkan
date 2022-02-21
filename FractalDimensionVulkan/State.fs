(*
This file is part of FractalDimension

FractalDimension is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

FractalDimension is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with FractalDimension. If not, see <https://www.gnu.org/licenses/>.
*)

module AppState

open System.Numerics

// Fractal Types?!?!
type DistanceEstimate =
    None | Mandelbox | Mandelbulb | Klienian | Menger | IFS with
    member self.ToInt (): int32 =
        match self with
        | None -> 0
        | Mandelbox -> 1
        | Mandelbulb -> 2
        | Klienian -> 3
        | Menger -> 4
        | IFS -> 5

let autoOrbitSpeed = 0.05f
let defaultAngularVelocity = Vector4 (0.f, 1.f, 0.f, autoOrbitSpeed)

[<Struct>]
[<System.Runtime.InteropServices.StructLayout (System.Runtime.InteropServices.LayoutKind.Explicit)>]
type PushConstantData = {
    [<System.Runtime.InteropServices.FieldOffset 0>]
    cameraQuaternion: Vector4

    [<System.Runtime.InteropServices.FieldOffset 16>]
    time: float32

    [<System.Runtime.InteropServices.FieldOffset 20>]
    aspectRatio: float32

    [<System.Runtime.InteropServices.FieldOffset 24>]
    deIntType: int32

    // Reactive locations
    [<System.Runtime.InteropServices.FieldOffset 32>]
    reactiveBass: Vector3

    [<System.Runtime.InteropServices.FieldOffset 48>]
    reactiveMids: Vector3

    [<System.Runtime.InteropServices.FieldOffset 64>]
    reactiveHigh: Vector3

    // Smooth locations
    [<System.Runtime.InteropServices.FieldOffset 80>]
    smoothBass: Vector3

    [<System.Runtime.InteropServices.FieldOffset 96>]
    smoothMids: Vector3

    [<System.Runtime.InteropServices.FieldOffset 112>]
    smoothHigh: Vector3}
let internal pushConstantSize = uint32 sizeof<PushConstantData>

let internal defaultPushConstants = {
    cameraQuaternion = EngineMaths.unitQuaternion
    time = 0.f
    aspectRatio = 16.f/9.f
    deIntType = DistanceEstimate.IFS.ToInt ()
    
    reactiveBass = Vector3.UnitX
    reactiveMids = Vector3.UnitX
    reactiveHigh = Vector3.UnitX
    
    smoothBass = Vector3.UnitX
    smoothMids = Vector3.UnitX
    smoothHigh = Vector3.UnitX}

(*[<Struct>]
[<System.Runtime.InteropServices.StructLayout (System.Runtime.InteropServices.LayoutKind.Explicit)>]
type GlobalUniformBufferObject = {
    // Reactive locations
    [<System.Runtime.InteropServices.FieldOffset 0>]
    mutable reactiveBass: Vector3

    [<System.Runtime.InteropServices.FieldOffset 16>]
    mutable reactiveMids: Vector3

    [<System.Runtime.InteropServices.FieldOffset 32>]
    mutable reactiveHigh: Vector3

    // Smooth locations
    [<System.Runtime.InteropServices.FieldOffset 48>]
    mutable smoothBass: Vector3

    [<System.Runtime.InteropServices.FieldOffset 64>]
    mutable smoothMids: Vector3

    [<System.Runtime.InteropServices.FieldOffset 80>]
    mutable smoothHigh: Vector3}
    
let internal newDefaultUniformBuffer () = {
    reactiveBass = Vector3.Zero
    reactiveMids = Vector3.Zero
    reactiveHigh = Vector3.Zero
    
    smoothBass = Vector3.Zero
    smoothMids = Vector3.Zero
    smoothHigh = Vector3.Zero}*)

type State = {
    // GPU Data
    pushConstants: PushConstantData

    // Camera rotation velocity
    angularVelocity: Vector4

    // Timing
    upTime: System.Diagnostics.Stopwatch
    previousFrameTime: float
    
    // Audio
    audioResponsive: bool
    volume: float32
    targetBass: Vector3
    targetMids: Vector3
    targetHigh: Vector3}

let newDefaultState () = {
    pushConstants = defaultPushConstants

    angularVelocity = defaultAngularVelocity

    upTime = System.Diagnostics.Stopwatch.StartNew ()
    previousFrameTime = 0.

    audioResponsive = true
    volume = 0.0001f
    targetBass = Vector3.Zero
    targetMids = Vector3.Zero
    targetHigh = Vector3.Zero}