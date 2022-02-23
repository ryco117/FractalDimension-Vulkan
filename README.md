![FractalDimension](FractalDimensionVulkan/FractalDimensionLogo.ico)
# FractalDimension

### About the project
An experimental audio visualizer written in F# and using the library [VulkanSharp](https://github.com/mono/VulkanSharp) as a wrapper for the Vulkan API.
The fractals are rendered using a technique called [ray-marching](http://blog.hvidtfeldts.net/index.php/2011/06/distance-estimated-3d-fractals-part-i/).
I chose the open source library [NAudio](https://github.com/naudio/NAudio) to retrieve the audio stream.

This project is a Vulkan re-implementation of the previous OpenGL based [FractalDimension](https://github.com/ryco117/FractalDimension).

### Visualizer Technical Description
FractalDimension uses the [WASAPI loopback](https://docs.microsoft.com/en-us/windows/win32/coreaudio/loopback-recording) 
feature to capture the current audio out stream. The audio is then read in chunks as they become available 
and a fast fourier transform is applied to the sampled audio stream. The resulting frequency domain is partitioned into three unequal parts, 
`bass`, `mids`, and `high`. For each of these sub domains, the discrete frequency bins with the largest magnitudes are determined 
(ie. the tones most present in the sound wave are determined). 
Then, based on the frequency and magnitude of the detected notes, various parameters of the environment are altered.