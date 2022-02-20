module EngineBuffer

open Vulkan

open EngineDevice

type EngineBuffer (device: EngineDevice, instanceSize: int, instanceCount: int, usageFlags: BufferUsageFlags, memoryPropertyFlags: MemoryPropertyFlags, ?minOffsetAlignmentOpt: int) =
    let alignmentSize =
        let alignment = Option.defaultValue 1 minOffsetAlignmentOpt
        if alignment > 1 then
            // Ensure size requested is smallest multiple of alignment 
            // that is greater than the instance size.
            let adjust = alignment - (instanceSize % alignment)
            if adjust = alignment then
                instanceSize
            else
                instanceSize + adjust
        else
            instanceSize
    let bufferSize = alignmentSize * instanceCount
    let buffer, memory =
        device.CreateBuffer (DeviceSize.op_Implicit bufferSize) usageFlags memoryPropertyFlags

    let zeroPointer = System.IntPtr.Zero
    let mutable mappedPtr = zeroPointer
    let unmap () =
        if mappedPtr <> zeroPointer then
            device.Device.UnmapMemory memory
            mappedPtr <- zeroPointer

    let mutable disposed = false
    let cleanup () =
        if not disposed then
            disposed <- true
            unmap ()
            device.Device.DestroyBuffer buffer
            device.Device.FreeMemory memory

    member _.Map (?sizeOpt: int, ?offsetOpt: int) =
        let size =
            match sizeOpt with
            | Some size -> DeviceSize.op_Implicit size
            | None -> Helpers.deviceWholeSize
        let offset =
            match offsetOpt with
            | Some offset -> DeviceSize.op_Implicit offset
            | None -> Helpers.deviceSizeZero
        mappedPtr <- device.Device.MapMemory (memory, offset, size)

    member _.Unmap = unmap

    member _.WriteToBuffer (transferToPtr: nativeint*int -> unit, ?offsetOpt: int) =
        let memOffset = System.IntPtr.Add (mappedPtr, Option.defaultValue 0 offsetOpt)
        transferToPtr (memOffset, alignmentSize)

    member _.Flush (?sizeOpt: int, ?offsetOpt: int) =
        let size =
            match sizeOpt with
            | Some size -> DeviceSize.op_Implicit size
            | None -> Helpers.deviceWholeSize
        let offset =
            match offsetOpt with
            | Some offset -> DeviceSize.op_Implicit offset
            | None -> Helpers.deviceSizeZero
        new MappedMemoryRange (Memory = memory, Offset = offset, Size = size)
        |> device.Device.FlushMappedMemoryRange

    member _.Invalidate (?sizeOpt: int, ?offsetOpt: int) =
        let size =
            match sizeOpt with
            | Some size -> DeviceSize.op_Implicit size
            | None -> Helpers.deviceWholeSize
        let offset =
            match offsetOpt with
            | Some offset -> DeviceSize.op_Implicit offset
            | None -> Helpers.deviceSizeZero
        new MappedMemoryRange (Memory = memory, Offset = offset, Size = size)
        |> device.Device.InvalidateMappedMemoryRange

    member _.DescriptorInfo (?sizeOpt: int, ?offsetOpt: int) =
        let size =
            match sizeOpt with
            | Some size -> DeviceSize.op_Implicit size
            | None -> Helpers.deviceWholeSize
        let offset =
            match offsetOpt with
            | Some offset -> DeviceSize.op_Implicit offset
            | None -> Helpers.deviceSizeZero
        new DescriptorBufferInfo (Buffer = buffer, Offset = offset, Range = size)

    member self.WriteToIndex (transferToPtr: nativeint*int -> unit) (index: int) = self.WriteToBuffer (transferToPtr, index * alignmentSize)
    member self.FlushIndex (index: int) = self.Flush (alignmentSize, index * alignmentSize)
    member self.InvalidateIndex (index: int) = self.Invalidate (alignmentSize, index * alignmentSize)
    member self.DescriptorInfoForIndex (index: int) = self.DescriptorInfo (alignmentSize, index * alignmentSize)

    member _.Buffer = buffer
    member _.BufferSize = bufferSize
    member _.MappedMemory = mappedPtr
    member _.InstanceCount = instanceCount
    member _.InstanceSize = instanceSize
    member _.AlignmentSize = alignmentSize
    member _.UsageFlags = usageFlags
    member _.MemoryPropertyFlags = memoryPropertyFlags

    interface System.IDisposable with override _.Dispose () = cleanup ()
    override self.Finalize () = cleanup ()