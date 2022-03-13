module Helpers

open System.Runtime.InteropServices

open Vulkan

// VulkanSharp constants
let deviceSizeZero = DeviceSize.op_Implicit 0
let deviceWholeSize = DeviceSize.op_Implicit (~~~0UL)

// VulkanSharp helper
let rectFromFourNumbers x y width height =
    Rect2D (Offset = Offset2D (X = x, Y = y), Extent = Extent2D (Width = width, Height = height))

// Marshal a Struct to an unmanaged address
let marshalStruct (item: 'a) (pointer: nativeint) = Marshal.StructureToPtr (item, pointer, false)

// Marshal an array of Structs to an unmanaged address, with an optional explicit element size (for packing alignment, etc.)
let marshalArrayOfStruct (elementSize: int option) (array: 'a[]) (pointer: nativeint) =
    let size = Option.defaultWith (fun () -> sizeof<'a>) elementSize
    let mutable offset = 0
    for element in array do
        marshalStruct element (System.IntPtr.Add (pointer, offset))
        offset <- offset + size

// Raise given message exception if not success condition
let assertMessage (message: string) (successCondition: bool) =
    if not successCondition then
        failwith message