module EngineDevice

open System.Runtime.InteropServices
open Vulkan

open EngineWindow

let private DEBUG = true
let internal validationLayers = if DEBUG then [|"VK_LAYER_KHRONOS_validation"|] else Array.empty
let internal deviceExtensions = [|"VK_KHR_swapchain"|]
let internal requiredInstanceExtensions =
    let ext = [|"VK_KHR_surface"; "VK_KHR_win32_surface"|]
    if DEBUG then
        Array.append [|"VK_EXT_debug_report"|] ext
    else
        ext

let internal debugCallback (severity: DebugReportFlagsExt) (msgType: DebugReportObjectTypeExt) (objectHandle: uint64) (location: nativeint) (messageCode: int) (layerPrefix: nativeint) (message: nativeint) (userData: nativeint) =
    let str = $"%s{severity.ToString()}: %s{Marshal.PtrToStringAnsi message}"
    Printf.eprintf "%s" str
    System.Diagnostics.Debug.WriteLine str
    Bool32.op_Implicit true

type EngineDevice (window: EngineWindow) =
    let checkValidationLayerSupport () =
        let availableLayers = Commands.EnumerateInstanceLayerProperties ()
        let containsMatch func arr =
            match Array.tryFind func arr with Some _ -> true | None -> false
        containsMatch (fun (layerName: string) -> containsMatch (fun (props: LayerProperties) -> layerName = props.LayerName) availableLayers) validationLayers

    // Create Instance first
    let instance =
        if DEBUG && not (checkValidationLayerSupport ()) then
            raise (System.Exception "Validation layers requested, but not available!")
        use appInfo =
            let version = Version.Make (1u, 1u, 0u)
            new ApplicationInfo (
                ApplicationName = "Voxel-Flight-Simulator",
                ApplicationVersion = version,
                EngineName = "No Engine",
                EngineVersion = version,
                ApiVersion = version
            )
        let createInfo =
            new InstanceCreateInfo (
                ApplicationInfo = appInfo,
                EnabledExtensionNames = requiredInstanceExtensions,
                EnabledLayerNames = validationLayers)
        new Instance (createInfo)

    let debugMessenger = Instance.DebugReportCallback (debugCallback)
    do instance.EnableDebug debugMessenger

    // Create Surface second
    let surface = window.CreateWindowSurface instance

    let findQueueFamilies (device: PhysicalDevice) =
        let queueFamilies = device.GetQueueFamilyProperties ()
        let graphicsCheck (prop: QueueFamilyProperties) = prop.QueueCount > 0u && prop.QueueFlags.HasFlag QueueFlags.Graphics
        let graphicsFamily = Array.tryFindIndex graphicsCheck queueFamilies
        let presentFamily = Array.tryFindIndex (fun (i: int, prop: QueueFamilyProperties) ->
            prop.QueueCount > 0u && Bool32.op_Implicit (device.GetSurfaceSupportKHR (uint32 i, surface))) (Array.mapi (fun i prop -> i, prop) queueFamilies)
        let uintOpt = function
        | Some i -> Some (uint32 i)
        | None -> None
        uintOpt (if Option.isSome presentFamily && graphicsCheck queueFamilies[Option.get presentFamily] then presentFamily else graphicsFamily), uintOpt presentFamily

    let querySwapchainSupport (device: PhysicalDevice) =
        (device.GetSurfaceFormatsKHR surface), (device.GetSurfacePresentModesKHR surface), (device.GetSurfaceCapabilitiesKHR surface)

    // Determine a Physical Device third
    let device =
        let devices = instance.EnumeratePhysicalDevices ()
        if devices.Length = 0 then
            raise (System.Exception "Failed to find GPUs with Vulkan support!")
        let requiredExts = Set.ofArray deviceExtensions
        let isDeviceSuitable (device: PhysicalDevice) =
            match findQueueFamilies device with
            | Some _, Some _ ->
                let availableExtensions = Array.map (fun (prop: ExtensionProperties) -> prop.ExtensionName) (device.EnumerateDeviceExtensionProperties "")
                if DEBUG || true then
                    printfn $"Avaliable extensions for %s{device.GetProperties().DeviceName}"
                    Array.iter (fun ext -> printfn $"%s{ext}") availableExtensions
                (Set.intersect requiredExts (Set.ofArray availableExtensions)).Count = requiredExts.Count &&
                    (let formats, presentModes, _ = querySwapchainSupport device in formats.Length > 0 && presentModes.Length > 0) &&
                    (let features = device.GetFeatures () in Bool32.op_Implicit features.SamplerAnisotropy)
            | _ -> false
        match Array.tryFind isDeviceSuitable devices with
        | Some physicalDevice -> physicalDevice
        | None -> raise (System.Exception "Failed to find a suitable GPU!")

    let findPhysicalQueueFamilies () =
        match findQueueFamilies device with
        | Some graphicsIndex, Some presentIndex -> graphicsIndex, presentIndex
        | _ -> raise (System.Exception "Selected GPU should have graphics and present queue families before this code")

    // Determine a Logical Device fourth
    let logicalDevice, graphicsQueue, presentQueue =
        let graphicsIndex, presentIndex = findPhysicalQueueFamilies ()
        let createInfoFromIndex index = new DeviceQueueCreateInfo (QueueFamilyIndex = index, QueuePriorities = [|1.f|])
        use graphicsInfo = createInfoFromIndex graphicsIndex
        let deviceQueueCreateInfos =
            if graphicsIndex <> presentIndex then
                [|graphicsInfo; createInfoFromIndex presentIndex|]
            else
                [|graphicsInfo|]
        let deviceFeatures = PhysicalDeviceFeatures (SamplerAnisotropy = Bool32.op_Implicit true)
        let logic =
            new DeviceCreateInfo (
                QueueCreateInfos = deviceQueueCreateInfos,
                EnabledFeatures = deviceFeatures,
                EnabledExtensionNames = deviceExtensions)
            |> device.CreateDevice
        logic, logic.GetQueue (graphicsIndex, 0u), logic.GetQueue (presentIndex, 0u)

    // Create Command Pool fifth
    let commandPool =
        let graphicsIndex, _presentIndex = findPhysicalQueueFamilies ()
        new CommandPoolCreateInfo (QueueFamilyIndex = graphicsIndex, Flags = CommandPoolCreateFlags.Transient + CommandPoolCreateFlags.ResetCommandBuffer)
        |> logicalDevice.CreateCommandPool

    let findMemoryType (typeFilter: uint32) properties =
        let memProps = device.GetMemoryProperties ()
        let mutable bit = 1u
        Array.findIndex (fun (memType: MemoryType) ->
            let r = (typeFilter &&& bit) > 0u && memType.PropertyFlags.HasFlag properties
            bit <- bit <<< 1
            r) memProps.MemoryTypes

    let beginSingleTimeCommands () =
        let commandBuffer =
            let buffers =
                new CommandBufferAllocateInfo (Level = CommandBufferLevel.Primary, CommandPool = commandPool, CommandBufferCount = 1u)
                |> logicalDevice.AllocateCommandBuffers
            buffers[0]
        new CommandBufferBeginInfo (Flags = CommandBufferUsageFlags.OneTimeSubmit)
        |> commandBuffer.Begin
        commandBuffer

    let endSingleTimeCommands (commandBuffer: CommandBuffer) =
        commandBuffer.End ()
        new SubmitInfo (CommandBuffers = [|commandBuffer|])
        |> graphicsQueue.Submit
        graphicsQueue.WaitIdle ()
        logicalDevice.FreeCommandBuffer (commandPool, commandBuffer)

    let mutable disposed = false
    let cleanup () =
        if not disposed then
            disposed <- true
            logicalDevice.DestroyCommandPool commandPool
            logicalDevice.Destroy ()

            instance.DestroySurfaceKHR surface
            instance.Destroy ()

    member _.Surface = surface

    member _.Device = logicalDevice

    member _.Properties = device.GetProperties ()

    member _.GraphicsQueue = graphicsQueue

    member _.PresentQueue = presentQueue

    member _.GetSwapchainSupport () = querySwapchainSupport device

    member _.FindPhysicalQueueFamilies () = findPhysicalQueueFamilies ()

    member _.FindSupportedFormat candidates tiling features =
        Array.find (fun format -> 
            let props = device.GetFormatProperties format
            (tiling = ImageTiling.Linear && props.LinearTilingFeatures.HasFlag features) || (tiling = ImageTiling.Optimal && props.OptimalTilingFeatures.HasFlag features)) candidates

    member _.CreateImageWithInfo imageInfo properties =
        let img = logicalDevice.CreateImage imageInfo
        let memRequirements = logicalDevice.GetImageMemoryRequirements img
        let allocInfo =
            new MemoryAllocateInfo (
                AllocationSize = memRequirements.Size,
                MemoryTypeIndex = uint32 (findMemoryType memRequirements.MemoryTypeBits properties))
        let imgMemory = logicalDevice.AllocateMemory allocInfo
        logicalDevice.BindImageMemory (img, imgMemory, Helpers.deviceSizeZero)
        img, imgMemory

    member _.CommandPool = commandPool

    member _.CreateBuffer (size: DeviceSize) (usage: BufferUsageFlags) (properties: MemoryPropertyFlags) =
        use bufferInfo =
            new BufferCreateInfo (
                Size = size,
                Usage = usage,
                SharingMode = SharingMode.Exclusive,
                QueueFamilyIndices = Array.empty)
        let buffer = logicalDevice.CreateBuffer bufferInfo
        let memRequirements = logicalDevice.GetBufferMemoryRequirements buffer
        use allocInfo =
            new MemoryAllocateInfo (
                AllocationSize = memRequirements.Size,
                MemoryTypeIndex = uint32 (findMemoryType memRequirements.MemoryTypeBits properties))
        let bufferMemory = logicalDevice.AllocateMemory allocInfo
        logicalDevice.BindBufferMemory (buffer, bufferMemory, Helpers.deviceSizeZero)
        buffer, bufferMemory

    member _.CopyBuffer srcBuffer dstBuffer size =
        let commandBuffer = beginSingleTimeCommands ()
        let copyRegion = BufferCopy (Size = size)
        commandBuffer.CmdCopyBuffer (srcBuffer, dstBuffer, copyRegion)
        endSingleTimeCommands commandBuffer

    member self.CreateLocalBufferWithTransfer (buffSize: DeviceSize) (usage: BufferUsageFlags) (transferToPtr: nativeint -> unit) =
        let stagingBuffer, stagingBufferMemory =
            self.CreateBuffer buffSize BufferUsageFlags.TransferSrc (MemoryPropertyFlags.HostVisible + MemoryPropertyFlags.HostCoherent)
        let memPtr = self.Device.MapMemory (stagingBufferMemory, Helpers.deviceSizeZero, buffSize)
        transferToPtr memPtr
        self.Device.UnmapMemory stagingBufferMemory
        let buffer, memory = self.CreateBuffer buffSize (usage + BufferUsageFlags.TransferDst) MemoryPropertyFlags.DeviceLocal
        self.CopyBuffer stagingBuffer buffer buffSize
        self.Device.DestroyBuffer stagingBuffer
        self.Device.FreeMemory stagingBufferMemory
        buffer, memory

    interface System.IDisposable with override _.Dispose () = cleanup ()
    override self.Finalize () = cleanup ()