module EngineModel

open Vulkan

open EngineDevice
open EngineBuffer

module EngineVertex =
    let bindingDescriptions =
        [|VertexInputBindingDescription (Binding = 0u, Stride = 2u * (uint32 sizeof<float32>), InputRate = VertexInputRate.Vertex)|]

    let attributeDescriptions =
        [|VertexInputAttributeDescription (Binding = 0u, Location = 0u, Format = Format.R32G32Sfloat, Offset = 0u)|]

    [<System.Runtime.InteropServices.StructLayout (System.Runtime.InteropServices.LayoutKind.Explicit)>]
    type Vertex =
        struct
            [<System.Runtime.InteropServices.FieldOffset 0>]
            val mutable X: float32
            [<System.Runtime.InteropServices.FieldOffset 4>]
            val mutable Y: float32
            [<System.Runtime.InteropServices.FieldOffset 8>]
            val mutable Z: float32
            [<System.Runtime.InteropServices.FieldOffset 16>]
            val mutable W: float32
        end

type EngineModel (device: EngineDevice, vertices: EngineVertex.Vertex[]) =
    let vertexBuffer =
        Helpers.assertMessage "A model requires at least 3 vertices" (vertices.Length >= 3)
        let vertexSize = sizeof<EngineVertex.Vertex>
        use stagingBuffer =
            new EngineBuffer (device, vertexSize, vertices.Length, BufferUsageFlags.TransferSrc, MemoryPropertyFlags.HostVisible + MemoryPropertyFlags.HostCoherent)

        stagingBuffer.Map ()
        Helpers.assertMessage "The size of the aligned instance and vertex struct are different" (stagingBuffer.AlignmentSize = sizeof<EngineVertex.Vertex>)
        stagingBuffer.WriteToBuffer (Helpers.marshalArrayOfStruct (Some stagingBuffer.AlignmentSize) vertices)

        let buffer = new EngineBuffer (device, vertexSize, vertices.Length, BufferUsageFlags.VertexBuffer + BufferUsageFlags.TransferDst, MemoryPropertyFlags.DeviceLocal)
        Helpers.assertMessage "Vertex and staging buffers are different sizes" (stagingBuffer.BufferSize = buffer.BufferSize)
        device.CopyBuffer stagingBuffer.Buffer buffer.Buffer (DeviceSize.op_Implicit buffer.BufferSize)
        buffer

    // TODO: Implement indicies

    let mutable disposed = false
    let cleanup () =
        if not disposed then
            disposed <- true
            (vertexBuffer : System.IDisposable).Dispose ()

    member _.Bind (commandBuffer: CommandBuffer) =
        commandBuffer.CmdBindVertexBuffer (0u, vertexBuffer.Buffer, Helpers.deviceSizeZero)

    member _.Draw (commandBuffer: CommandBuffer) =
        commandBuffer.CmdDraw (uint32 vertices.Length, 1u, 0u, 0u)

    interface System.IDisposable with override _.Dispose () = cleanup ()
    override self.Finalize () = cleanup ()