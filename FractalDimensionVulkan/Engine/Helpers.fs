module Helpers

open System.Runtime.InteropServices

open Vulkan

let deviceSizeZero = DeviceSize.op_Implicit 0
let deviceWholeSize = DeviceSize.op_Implicit (~~~0UL)

let rectFromFourNumbers x y width height =
    Rect2D (Offset = Offset2D (X = x, Y = y), Extent = Extent2D (Width = width, Height = height))

let MarshalStruct (item: 'a) (pointer: nativeint) = Marshal.StructureToPtr (item, pointer, false)

let MarshalArrayOfStruct (array: 'a[]) (pointer: nativeint) =
    let elementSize = sizeof<'a>
    let mutable offset = 0
    for element in array do
        MarshalStruct element (System.IntPtr.Add (pointer, offset))
        offset <- offset + elementSize