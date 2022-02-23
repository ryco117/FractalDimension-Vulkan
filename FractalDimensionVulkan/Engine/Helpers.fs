module Helpers

open System.Runtime.InteropServices

open Vulkan

let deviceSizeZero = DeviceSize.op_Implicit 0
let deviceWholeSize = DeviceSize.op_Implicit (~~~0UL)

let rectFromFourNumbers x y width height =
    Rect2D (Offset = Offset2D (X = x, Y = y), Extent = Extent2D (Width = width, Height = height))

let marshalStruct (item: 'a) (pointer: nativeint) = Marshal.StructureToPtr (item, pointer, false)

let marshalArrayOfStruct (elementSize: int option) (array: 'a[]) (pointer: nativeint) =
    let size = Option.defaultWith (fun () -> sizeof<'a>) elementSize
    let mutable offset = 0
    for element in array do
        marshalStruct element (System.IntPtr.Add (pointer, offset))
        offset <- offset + size

let assertMessage (message: string) (successCondition: bool) =
    if not successCondition then
        System.Exception message |> raise