module EngineMaths

[<System.Runtime.InteropServices.StructLayout (System.Runtime.InteropServices.LayoutKind.Explicit)>]
type Vector4 =
    struct
        [<System.Runtime.InteropServices.FieldOffset 0>]
        val mutable x: float32
        [<System.Runtime.InteropServices.FieldOffset 4>]
        val mutable y: float32
        [<System.Runtime.InteropServices.FieldOffset 8>]
        val mutable z: float32
        [<System.Runtime.InteropServices.FieldOffset 12>]
        val mutable w: float32
        new (x', y', z', w') = {x = x'; y = y'; z = z'; w = w'}
        new (t) = {x = t; y = t; z = t; w = t}
        static member (+) (u: Vector4, v: Vector4) = Vector4 (u.x + v.x, u.y + v.y, u.z + v.z, u.w + v.w)
        static member (*) (t: float32, v: Vector4) = Vector4 (t * v.x, t * v.y, t * v.z, t * v.w)
        static member (*) (v: Vector4, t: float32) = Vector4 (t * v.x, t * v.y, t * v.z, t * v.w)
        static member (/) (v: Vector4, t: float32) = Vector4 (v.x / t, v.y / t, v.z / t, v.w / t)
        static member Zero = Vector4 (0.f)
        static member UnitQuaternion = Vector4 (0.f, 0.f, 0.f, 1.f)
        static member BuildQuaternion (v: System.Numerics.Vector3) (theta: float32) =
            let v = sin theta * System.Numerics.Vector3.Normalize v
            Vector4 (v.X, v.Y, v.Z, cos theta)
        member self.AddInPlace (v: Vector4) =
            self.x <- self.x + v.x
            self.y <- self.y + v.y
            self.z <- self.z + v.z
            self.w <- self.w + v.w
        member self.RotateVectorAsQuaternion (p: System.Numerics.Vector3) =
            let q = System.Numerics.Vector3 (self.x, self.y, self.z)
            p + 2.f*System.Numerics.Vector3.Cross(q, System.Numerics.Vector3.Cross(q, p) + self.w * p)
        member self.QuaternionMultiply (quaternion: Vector4) =
            let q = System.Numerics.Vector3 (self.x, self.y, self.z)
            let q' = System.Numerics.Vector3 (quaternion.x, quaternion.y, quaternion.z)
            let v = System.Numerics.Vector3.Cross (q, q') + self.w * q' + quaternion.w * q
            Vector4 (v.X, v.Y, v.Z, self.w * quaternion.w - System.Numerics.Vector3.Dot (q, q'))
    end