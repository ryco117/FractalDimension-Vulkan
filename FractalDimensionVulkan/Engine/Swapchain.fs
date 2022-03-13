module EngineSwapchain

open Vulkan

open EngineDevice

let internal maxFramesInFlight = 3

type EngineSwapchain (device: EngineDevice, ?desiredPresentMode: PresentModeKhr, ?oldSwapchain: EngineSwapchain) =
    // Create Swapchain first
    let swapchain, swapchainImageFormat, swapchainExtent =
        let formats, modes, capabilities = device.GetSwapchainSupport ()
        let surfaceFormat = Array.find (fun (format: SurfaceFormatKhr) ->
            format.Format = Format.B8G8R8A8Srgb && format.ColorSpace = ColorSpaceKhr.SrgbNonlinear) formats
        let presentMode =
            let desiredMode = defaultArg desiredPresentMode PresentModeKhr.Fifo
            match Array.tryFind (fun mode -> mode = desiredMode) modes with
            | Some mode -> mode
            | None -> printfn "Defaulting to V-Sync present mode (FIFO)"; PresentModeKhr.Fifo
        let extent = capabilities.CurrentExtent
        let imageCount =
            let minimum = capabilities.MinImageCount + 1u
            if capabilities.MaxImageCount > 0u then
                min minimum capabilities.MaxImageCount
            else
                minimum
        let createInfo =
            let graphics, present = device.FindPhysicalQueueFamilies ()
            let sharing, indices =
                if graphics = present then
                    SharingMode.Exclusive, null
                else
                    SharingMode.Concurrent, [|graphics; present|]
            new SwapchainCreateInfoKhr (
                Surface = device.Surface,

                // Image config
                MinImageCount = imageCount,
                ImageFormat = surfaceFormat.Format,
                ImageColorSpace = surfaceFormat.ColorSpace,
                ImageExtent = extent,
                ImageArrayLayers = 1u,
                ImageUsage = ImageUsageFlags.ColorAttachment,

                // Differing families?
                ImageSharingMode = sharing,
                QueueFamilyIndices = indices,

                PreTransform = capabilities.CurrentTransform,
                CompositeAlpha = CompositeAlphaFlagsKhr.Opaque,

                PresentMode = presentMode,
                Clipped = true,
                OldSwapchain = match oldSwapchain with Some chain -> chain.Swapchain | None -> null)
        (device.Device.CreateSwapchainKHR createInfo, surfaceFormat.Format, extent)

    // Step 2: Create Image Views using new swapchain
    let swapchainImages = device.Device.GetSwapchainImagesKHR swapchain
    let swapchainImageViews = Array.map (fun img ->
        let createInfo =
            new ImageViewCreateInfo (
                Image = img,
                ViewType = ImageViewType.View2D,
                Format = swapchainImageFormat,
                SubresourceRange = ImageSubresourceRange (AspectMask = ImageAspectFlags.Color, BaseMipLevel = 0u, LevelCount = 1u, BaseArrayLayer = 0u, LayerCount = 1u))
        device.Device.CreateImageView createInfo) swapchainImages

    let imageCount = swapchainImages.Length
    
    let swapchainDepthFormat =
        device.FindSupportedFormat
            [|Format.D32Sfloat; Format.D32SfloatS8Uint; Format.D24UnormS8Uint|]
            ImageTiling.Optimal
            FormatFeatureFlags.DepthStencilAttachment

    let subpassExternal = System.UInt32.MaxValue
    let accessNone = AccessFlags.Parse "0"
    let imageCreateNone = ImageCreateFlags.Parse "0"

    // Step 3: Create Render Pass
    let renderPass =
        let depthAttachment =
            AttachmentDescription (
                Format = swapchainDepthFormat,
                Samples = SampleCountFlags.Count1,
                LoadOp = AttachmentLoadOp.Clear,
                StoreOp = AttachmentStoreOp.DontCare,
                StencilLoadOp = AttachmentLoadOp.DontCare,
                StencilStoreOp = AttachmentStoreOp.DontCare,
                InitialLayout = ImageLayout.Undefined,
                FinalLayout = ImageLayout.DepthStencilAttachmentOptimal)
        let depthRef =
            AttachmentReference (
                Attachment = 1u,
                Layout = ImageLayout.DepthStencilAttachmentOptimal)
        let colourAttachment =
            AttachmentDescription (
                Format = swapchainImageFormat,
                Samples = SampleCountFlags.Count1,
                LoadOp = AttachmentLoadOp.Clear,
                StoreOp = AttachmentStoreOp.Store,
                StencilLoadOp = AttachmentLoadOp.DontCare,
                StencilStoreOp = AttachmentStoreOp.DontCare,
                InitialLayout = ImageLayout.Undefined,
                FinalLayout = ImageLayout.PresentSrcKhr)
        let colourRef =
            AttachmentReference (
                Attachment = 0u,
                Layout = ImageLayout.ColorAttachmentOptimal)
        let subpass =
            new SubpassDescription (
                PipelineBindPoint = PipelineBindPoint.Graphics,
                ColorAttachments = [|colourRef|],
                DepthStencilAttachment = depthRef)
        let dependency =
            SubpassDependency (
                DstSubpass = 0u,
                DstAccessMask = AccessFlags.ColorAttachmentWrite + AccessFlags.DepthStencilAttachmentWrite,
                DstStageMask = PipelineStageFlags.ColorAttachmentOutput + PipelineStageFlags.EarlyFragmentTests,
                SrcSubpass = subpassExternal,
                SrcAccessMask = accessNone,
                SrcStageMask = PipelineStageFlags.ColorAttachmentOutput + PipelineStageFlags.EarlyFragmentTests)
        let attachments = [|colourAttachment; depthAttachment|]
        let renderPassInfo =
            new RenderPassCreateInfo (
                Attachments = attachments,
                Subpasses = [|subpass|],
                Dependencies = [|dependency|])
        device.Device.CreateRenderPass renderPassInfo

    // Step 4: Create Depth Resources
    let depthImages, depthImageMemories =
        let imageInfo =
            let extent =
                Extent3D (
                    Width = swapchainExtent.Width,
                    Height = swapchainExtent.Height,
                    Depth = 1u)
            new ImageCreateInfo (
                ImageType = ImageType.Image2D,
                Extent = extent,
                MipLevels = 1u,
                ArrayLayers = 1u,
                Format = swapchainDepthFormat,
                Tiling = ImageTiling.Optimal,
                InitialLayout = ImageLayout.Undefined,
                Usage = ImageUsageFlags.DepthStencilAttachment,
                Samples = SampleCountFlags.Count1,
                SharingMode = SharingMode.Exclusive,
                Flags = imageCreateNone)
        Array.init imageCount (fun _ -> device.CreateImageWithInfo imageInfo MemoryPropertyFlags.DeviceLocal) |> Array.unzip
    let depthImageViews =
        Array.map (fun img ->
            let viewInfo =
                new ImageViewCreateInfo (
                    Image = img,
                    ViewType = ImageViewType.View2D,
                    Format = swapchainDepthFormat,
                    SubresourceRange =
                        ImageSubresourceRange (
                            AspectMask = ImageAspectFlags.Depth,
                            BaseMipLevel = 0u,
                            LevelCount = 1u,
                            BaseArrayLayer = 0u,
                            LayerCount = 1u))
            device.Device.CreateImageView viewInfo) depthImages

    // Step 5: Create Frame Buffers
    let swapchainFramebuffers = Array.init imageCount (fun i ->
        let attachments = [|swapchainImageViews[i]; depthImageViews[i]|]
        let info =
            new FramebufferCreateInfo (
                RenderPass = renderPass,
                Attachments = attachments,
                Width = swapchainExtent.Width,
                Height = swapchainExtent.Height,
                Layers = 1u)
        device.Device.CreateFramebuffer info)

    // Step 6: Create Sync Objects
    let imagesInFlight = Array.zeroCreate<Fence> imageCount
    let imageAvailableSemaphores, renderFinishedSemaphores, inFlightFences =
        let semaphoreInfo = new SemaphoreCreateInfo ()
        let fenceInfo = new FenceCreateInfo (Flags = FenceCreateFlags.Signaled)
        Array.init maxFramesInFlight (fun _ ->
            device.Device.CreateSemaphore semaphoreInfo,
            device.Device.CreateSemaphore semaphoreInfo,
            device.Device.CreateFence fenceInfo)
        |> Array.unzip3

    // Cleanup previous swapchain
    do match oldSwapchain with
        | Some chain -> (chain :> System.IDisposable).Dispose ()
        | None -> ()

    let mutable currentFrame = 0
    let timeout = System.UInt64.MaxValue
    let aspectRatio = (float32 swapchainExtent.Width) / (float32 swapchainExtent.Height)

    let waitForFence fence = device.Device.WaitForFences ([|fence|], Bool32.op_Implicit true, timeout)

    let mutable disposed = false
    let cleanup () =
        if not disposed then
            disposed <- true
            let d = device.Device
            Array.iter (fun view -> d.DestroyImageView view) swapchainImageViews

            d.DestroySwapchainKHR swapchain

            Array.iteri (fun i img ->
                d.DestroyImageView depthImageViews[i]
                d.DestroyImage img
                d.FreeMemory depthImageMemories[i]) depthImages

            Array.iter (fun fb -> d.DestroyFramebuffer fb) swapchainFramebuffers

            d.DestroyRenderPass renderPass

            Array.iteri (fun i fence ->
                d.DestroySemaphore renderFinishedSemaphores[i]
                d.DestroySemaphore imageAvailableSemaphores[i]
                d.DestroyFence fence) inFlightFences

    member _.RenderPass = renderPass

    member _.Swapchain = swapchain

    member _.GetFramebuffer index = swapchainFramebuffers[index]

    member _.Extent = swapchainExtent

    member _.ExtentAspectRatio = aspectRatio

    member _.ImageCount = imageCount

    member _.AcquireNextImageAsIndex () =
        waitForFence inFlightFences[currentFrame]
        int (device.Device.AcquireNextImageKHR (swapchain, timeout, imageAvailableSemaphores[currentFrame]))

    member _.SubmitCommandBuffers buffer imageIndex =
        if imagesInFlight[imageIndex] <> Unchecked.defaultof<Fence> then
            waitForFence imagesInFlight[imageIndex]

        imagesInFlight[imageIndex] <- inFlightFences[currentFrame]
        let waitSemaphores = [|imageAvailableSemaphores[currentFrame]|]
        let signalSemaphores = [|renderFinishedSemaphores[currentFrame]|]
        let submitInfo =
            new SubmitInfo (
                WaitSemaphores = waitSemaphores,
                WaitDstStageMask = [|PipelineStageFlags.ColorAttachmentOutput|],
                CommandBuffers = [|buffer|],
                SignalSemaphores = signalSemaphores)
        device.Device.ResetFence inFlightFences[currentFrame]
        device.GraphicsQueue.Submit ([|submitInfo|], inFlightFences[currentFrame])
        new PresentInfoKhr (
            WaitSemaphores = signalSemaphores,
            Swapchains = [|swapchain|],
            ImageIndices = [|uint32 imageIndex|])
        |> device.PresentQueue.PresentKHR
        currentFrame <- (currentFrame + 1) % maxFramesInFlight

    member _.DepthFormat = swapchainDepthFormat
    member _.ImageFormat = swapchainImageFormat
    member _.CompareSwapFormats (swapchain': EngineSwapchain) =
        // TODO: Is possible for more subtle differences to occur, but given current construction of render pass this will suffice
        swapchain'.DepthFormat = swapchainDepthFormat && swapchain'.ImageFormat = swapchainImageFormat

    interface System.IDisposable with override _.Dispose () = cleanup ()
    override self.Finalize () = cleanup ()