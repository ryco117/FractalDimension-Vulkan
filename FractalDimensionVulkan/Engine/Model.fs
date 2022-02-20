module EngineModel

open System.Runtime.InteropServices
open Vulkan

open EngineDevice
open EngineBuffer

module Vertex =
    let bindingDescriptions =
        [|VertexInputBindingDescription (Binding = 0u, Stride = 2u * (uint32 sizeof<float32>), InputRate = VertexInputRate.Vertex)|]

    let attributeDescriptions =
        [|VertexInputAttributeDescription (Binding = 0u, Location = 0u, Format = Format.R32G32Sfloat, Offset = 0u)|]

type EngineModel (device: EngineDevice, vertices: float32[]) =
    (*let vertexBuffer, vertexBufferMemory =
        let count = vertices.Length
        assert (count >= 3)
        let buffSize = DeviceSize.op_Implicit (sizeof<float32> * count)
        let transferToPtr (memPtr: nativeint) = Marshal.Copy (vertices, 0, memPtr, vertices.Length)
        device.CreateLocalBufferWithTransfer buffSize BufferUsageFlags.VertexBuffer transferToPtr*)
    let stagingBuffer =
        assert (vertices.Length >= 3)
        let vertexSize = sizeof<float32>
        let buffSize = vertexSize * vertices.Length
        new EngineBuffer (device, vertexSize, vertices.Length, BufferUsageFlags.TransferSrc, MemoryPropertyFlags.HostVisible + MemoryPropertyFlags.HostCoherent)
   (* do
        stagingBuffer.Map ()
        let transferToPtr (memPtr: nativeint, alignmentSize: int) = Marshal.Copy (vertices, 0, memPtr, vertices.Length)
        stagingBuffer.WriteToBuffer (transferToPtr, vertices)*)

    let mutable disposed = false
    let cleanup () =
        if not disposed then
            disposed <- true
            //(vertexBuffer : System.IDisposable).Dispose ()

    member _.Bind (commandBuffer: CommandBuffer) = ()
        //commandBuffer.CmdBindVertexBuffer (0u, vertexBuffer, Helpers.deviceSizeZero)

    member _.Draw (commandBuffer: CommandBuffer) =
        commandBuffer.CmdDraw (uint32 vertices.Length, 1u, 0u, 0u)

    interface System.IDisposable with override _.Dispose () = cleanup ()
    override self.Finalize () = cleanup ()