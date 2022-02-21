(*
This file is part of FractalDimension

FractalDimension is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

FractalDimension is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with FractalDimension. If not, see <https://www.gnu.org/licenses/>.
*)

module RenderSystem

open Vulkan

open EngineDevice
open EnginePipeline

open AppState

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
        assert (device.Properties.Limits.MaxPushConstantsSize >= pushConstantSize)
        let pushConstantRange = PushConstantRange (StageFlags = ShaderStageFlags.Fragment, Offset = 0u, Size = pushConstantSize)
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

    member _.RenderGameObjects (buffer: CommandBuffer) (state: State) =
        pipeline.Bind buffer
        //buffer.CmdBindDescriptorSet (PipelineBindPoint.Graphics, pipelineLayout, 0u, descriptorSet, System.Nullable ())
        buffer.CmdPushConstants (pipelineLayout, ShaderStageFlags.Fragment, 0u, pushConstantSize, NativeInterop.NativePtr.toNativeInt &&state.pushConstants)
        buffer.CmdDraw (4u, 1u, 0u, 0u)
        (*model.Bind buffer
        model.Draw buffer*)

    interface System.IDisposable with override _.Dispose () = cleanup ()
    override self.Finalize () = cleanup ()