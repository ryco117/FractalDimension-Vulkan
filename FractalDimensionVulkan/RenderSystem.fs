module RenderSystem

open Vulkan

open EngineDevice
open EnginePipeline

open AppState

[<System.Runtime.InteropServices.StructLayout (System.Runtime.InteropServices.LayoutKind.Explicit)>]
type PushConstantData =
    struct
        [<System.Runtime.InteropServices.FieldOffset 0>]
        val cameraPosition: System.Numerics.Vector3
        [<System.Runtime.InteropServices.FieldOffset 12>]
        val time: float32
        [<System.Runtime.InteropServices.FieldOffset 16>]
        val cameraQuaternion: EngineMaths.Vector4
        [<System.Runtime.InteropServices.FieldOffset 32>]
        val aspectRatio: float32
        new (time', aspectRatio') = {
            cameraPosition = demoCameraPosition time'
            time = time'
            cameraQuaternion = demoCameraQuaternion time'
            aspectRatio = aspectRatio'}
        new (cameraPosition', cameraQuaternion', time', aspectRatio') = {
            cameraPosition = cameraPosition'
            time = time'
            cameraQuaternion = cameraQuaternion'
            aspectRatio = aspectRatio'}
    end
let internal pushConstantSize = sizeof<PushConstantData>

let private DEBUG = true

type RenderSystem (device: EngineDevice, initialRenderPass: RenderPass) =
    (*let descriptorPool =
        use poolInfo =
            (*TODO: Allow more than one descriptor set*)
            new DescriptorPoolCreateInfo (
                PoolSizes = [|DescriptorPoolSize (Type = DescriptorType.StorageBuffer, DescriptorCount = 1u)|],
                MaxSets = 1u)
        device.Device.CreateDescriptorPool poolInfo

    let descriptorSetLayout =
        use voxelOctreeBinding =
            new DescriptorSetLayoutBinding (
                Binding = 0u,
                DescriptorCount = 1u,
                DescriptorType = DescriptorType.StorageBuffer,
                StageFlags = ShaderStageFlags.Fragment)
        use setLayoutInfo =
            new DescriptorSetLayoutCreateInfo (
                BindingCount = 1u,
                Bindings = [|voxelOctreeBinding|])
        device.Device.CreateDescriptorSetLayout setLayoutInfo

    let descriptorSet =
        use allocInfo =
            new DescriptorSetAllocateInfo (
                DescriptorPool = descriptorPool,
                DescriptorSetCount = 1u,
                SetLayouts = [|descriptorSetLayout|])
        match device.Device.AllocateDescriptorSets allocInfo with
        | [|descriptorSet|] -> descriptorSet
        | _ -> raise (System.Exception "Exactly one descriptor set is expected to be created")

    let updateDescriptorSets () =
        use buffInfo =
            new DescriptorBufferInfo (
                Buffer = voxelBuffer,
                Offset = DeviceSize.op_Implicit 0,
                Range = voxelBufferDeviceSize)
        use setWrite =
            new WriteDescriptorSet (
                DstBinding = 0u,
                DstSet = descriptorSet,
                DescriptorType = DescriptorType.StorageBuffer,
                BufferInfo = [|buffInfo|])
        device.Device.UpdateDescriptorSets ([|setWrite|], null)
    do updateDescriptorSets ()*)

    let pipelineLayout =
        assert (device.Properties.Limits.MaxPushConstantsSize >= uint32 pushConstantSize)
        let pushConstantRange = PushConstantRange (StageFlags = ShaderStageFlags.Fragment, Offset = 0u, Size = uint32 pushConstantSize)
        let pipelineCreateInfo =
            new PipelineLayoutCreateInfo (
                PushConstantRanges = [|pushConstantRange|],
                SetLayouts = Array.empty)
                //SetLayouts = [|descriptorSetLayout|])
        device.Device.CreatePipelineLayout pipelineCreateInfo

    let createPipeline renderPass =
        let config = {defaultPipelineConfig () with renderPass = renderPass; pipelineLayout = pipelineLayout}
        new EnginePipeline (device, "shaders/quad.vert.spv", "shaders/ray_march.frag.spv", config)
    let mutable pipeline = createPipeline initialRenderPass

    let mutable disposed = false
    let cleanup () =
        if not disposed then
            disposed <- true
            (*device.Device.DestroyBuffer voxelBuffer
            device.Device.FreeMemory voxelBufferMemory
            device.Device.DestroyDescriptorPool descriptorPool*)
            device.Device.DestroyPipelineLayout pipelineLayout

    member _.RenderGameObjects buffer (state: State) aspectRatio =
        pipeline.Bind buffer
        //buffer.CmdBindDescriptorSet (PipelineBindPoint.Graphics, pipelineLayout, 0u, descriptorSet, System.Nullable ())
        let mutable structure =
            let time = float32 state.upTime.Elapsed.TotalSeconds
            if DEBUG then
                PushConstantData (time, aspectRatio)
            else
                PushConstantData (state.cameraPosition, state.cameraQuaternion, time, aspectRatio)
        buffer.CmdPushConstants (pipelineLayout, ShaderStageFlags.Fragment, 0u, uint32 pushConstantSize, NativeInterop.NativePtr.toNativeInt &&structure)
        buffer.CmdDraw (4u, 1u, 0u, 0u)
        (*model.Bind buffer
        model.Draw buffer*)

    interface System.IDisposable with override _.Dispose () = cleanup ()
    override self.Finalize () = cleanup ()