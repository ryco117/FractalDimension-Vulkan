module EnginePipeline

open System.IO
open Vulkan

open EngineDevice

type PipelineConfig = {
    viewportInfo: PipelineViewportStateCreateInfo
    inputAssemblyInfo: PipelineInputAssemblyStateCreateInfo
    rasterizationInfo: PipelineRasterizationStateCreateInfo
    multisampleInfo: PipelineMultisampleStateCreateInfo
    colorBlendAttachment: PipelineColorBlendAttachmentState
    colorBlendInfo: PipelineColorBlendStateCreateInfo
    depthStencilInfo: PipelineDepthStencilStateCreateInfo
    dynamicStateEnables: DynamicState[]
    dynamicStateInfo: PipelineDynamicStateCreateInfo
    pipelineLayout: PipelineLayout
    renderPass: RenderPass
    subpass: uint32
}

let defaultPipelineConfig () =
    let colorBlendAttachment =
        PipelineColorBlendAttachmentState (
            ColorWriteMask = ColorComponentFlags.R + ColorComponentFlags.G + ColorComponentFlags.B + ColorComponentFlags.A,
            BlendEnable = Bool32.op_Implicit false,
            SrcColorBlendFactor = BlendFactor.One,
            DstColorBlendFactor = BlendFactor.Zero,
            ColorBlendOp = BlendOp.Add,
            SrcAlphaBlendFactor = BlendFactor.One,
            DstAlphaBlendFactor = BlendFactor.Zero,
            AlphaBlendOp = BlendOp.Add)
    let dynamicStates = [|DynamicState.Viewport; DynamicState.Scissor|]
    {
        inputAssemblyInfo =
            new PipelineInputAssemblyStateCreateInfo (
                Topology = PrimitiveTopology.TriangleStrip, PrimitiveRestartEnable = false)
        viewportInfo =
            new PipelineViewportStateCreateInfo (
                ViewportCount = 1u,
                ScissorCount = 1u)
        rasterizationInfo = 
            new PipelineRasterizationStateCreateInfo (
                DepthClampEnable = false,
                RasterizerDiscardEnable = false,
                PolygonMode = PolygonMode.Fill,
                LineWidth = 1.f,
                CullMode = CullModeFlags.None,
                FrontFace = FrontFace.CounterClockwise,
                DepthBiasEnable = false,
                // Optional extras?
                DepthBiasConstantFactor = 0.f,
                DepthBiasClamp = 0.f,
                DepthBiasSlopeFactor = 0.f)
        multisampleInfo =
            new PipelineMultisampleStateCreateInfo (
                SampleShadingEnable = false,
                RasterizationSamples = SampleCountFlags.Count1)
        colorBlendAttachment = colorBlendAttachment
        colorBlendInfo =
            new PipelineColorBlendStateCreateInfo (
                LogicOpEnable = false,
                LogicOp = LogicOp.Copy, // Optional?
                Attachments = [|colorBlendAttachment|],
                // Optional extras?
                BlendConstants = Array.zeroCreate<float32> 4)
        depthStencilInfo =
            new PipelineDepthStencilStateCreateInfo (
                DepthTestEnable = true,
                DepthWriteEnable = true,
                DepthCompareOp = CompareOp.Less,
                DepthBoundsTestEnable = false,
                MinDepthBounds = 0.f,   // Optional?
                MaxDepthBounds = 1.f,   // Optional?
                StencilTestEnable = false,
                Front = StencilOpState (),
                Back = StencilOpState ())
        dynamicStateEnables = dynamicStates
        dynamicStateInfo = new PipelineDynamicStateCreateInfo (DynamicStates = dynamicStates)
        pipelineLayout = null
        renderPass = null
        subpass = 0u
    }

type EnginePipeline(device: EngineDevice, vertPath: string, fragPath: string, config: PipelineConfig) =
    let vertModule = device.Device.CreateShaderModule (File.ReadAllBytes vertPath)
    let fragModule = device.Device.CreateShaderModule (File.ReadAllBytes fragPath)

    let graphicsPipeline =
        let shaderStages = [|
            new PipelineShaderStageCreateInfo (Stage = ShaderStageFlags.Vertex, Module = vertModule, Name = "main");
            new PipelineShaderStageCreateInfo (Stage = ShaderStageFlags.Fragment, Module = fragModule, Name = "main")
        |]

        let pipelineInfo =
            new GraphicsPipelineCreateInfo (
                Stages = shaderStages,
                //VertexInputState = new PipelineVertexInputStateCreateInfo (VertexAttributeDescriptions = EngineModel.Vertex.attributeDescriptions, VertexBindingDescriptions = EngineModel.Vertex.bindingDescriptions),
                VertexInputState = new PipelineVertexInputStateCreateInfo (VertexAttributeDescriptions = Array.empty, VertexBindingDescriptions = Array.empty),
                InputAssemblyState = config.inputAssemblyInfo,
                ViewportState = config.viewportInfo,
                RasterizationState = config.rasterizationInfo,
                MultisampleState = config.multisampleInfo,
                ColorBlendState = config.colorBlendInfo,
                DepthStencilState = config.depthStencilInfo,
                DynamicState = config.dynamicStateInfo,

                Layout = config.pipelineLayout,
                RenderPass = config.renderPass,
                Subpass = config.subpass)
        match device.Device.CreateGraphicsPipelines (null, [|pipelineInfo|]) with
        | [|pipeline|] -> pipeline
        | _ -> System.Exception "Should exist exactly one graphics pipeline, as requested" |> raise

    let mutable disposed = false
    let cleanup () =
        if not disposed then
            disposed <- true
            device.Device.DestroyShaderModule vertModule
            device.Device.DestroyShaderModule fragModule
            device.Device.DestroyPipeline graphicsPipeline

    member _.Bind (commandBuffer: CommandBuffer) = commandBuffer.CmdBindPipeline (PipelineBindPoint.Graphics, graphicsPipeline)

    interface System.IDisposable with override _.Dispose () = cleanup ()
    override self.Finalize () = cleanup ()