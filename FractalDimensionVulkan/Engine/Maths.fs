module EngineMaths

open System.Numerics

// Constant for unit quaternion for Vector4 for simplicity
let unitQuaternion = Vector4.UnitW

// Angular velocity xyz must be normalized and define axis of rotation
let buildQuaternion (angularVelocity: Vector4) =
    let ratio = sin angularVelocity.W
    Vector4 (ratio * angularVelocity.X, ratio * angularVelocity.Y, ratio * angularVelocity.Z, cos angularVelocity.W)

// Implementation of quaternion/point multiplication for Vector4 for simplicity
let rotateVectorAsQuaternion (q: Vector4) (p: Vector3) =
    let q' = Vector3 (q.X, q.Y, q.Z)
    p + 2.f*Vector3.Cross(q', Vector3.Cross(q', p) + q.W * p)

// Implementation of quaternion multiplication for Vector4 for simplicity
let quaternionMultiply (p: Vector4) (q: Vector4) =
    let p' = Vector3 (p.X, p.Y, p.Z)
    let q' = Vector3 (q.X, q.Y, q.Z)
    let v = Vector3.Cross (p', q') + p.W * q' + q.W * p'
    Vector4 (v.X, v.Y, v.Z, p.W * q.W - Vector3.Dot (p', q'))