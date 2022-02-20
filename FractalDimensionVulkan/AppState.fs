module AppState

type State = {
    lastFrameTime: float
    cameraPosition: System.Numerics.Vector3
    cameraQuaternion: EngineMaths.Vector4
    angularVelocity: EngineMaths.Vector4
    upTime: System.Diagnostics.Stopwatch}

let demoCameraPosition time =
    let delta = time / 16.f;
    System.Numerics.Vector3 (-2.25f * sin delta, 0.125f, -2.25f * cos delta)

let demoCameraQuaternion time =
    let delta = time / 32.f
    EngineMaths.Vector4 (0.f, sin delta, 0.f, cos delta)

let initialPosition = System.Numerics.Vector3 (0.f, 0.125f, -2.f)

let newDefaultState () = {
    lastFrameTime = 0.
    cameraPosition = initialPosition
    cameraQuaternion = EngineMaths.Vector4.UnitQuaternion
    angularVelocity = EngineMaths.Vector4.UnitQuaternion
    upTime = System.Diagnostics.Stopwatch.StartNew ()}