@echo off
for /f %%v in ('dir shaders\*.vert /B') do (C:\VulkanSDK\1.2.189.2\Bin\glslc.exe shaders\%%v -o shaders\%%v.spv || goto FINISH)
for /f %%f in ('dir shaders\*.frag /B') do (C:\VulkanSDK\1.2.189.2\Bin\glslc.exe shaders\%%f -o shaders\%%f.spv || goto FINISH)
robocopy shaders\ bin\Release\net6.0-windows\shaders\ *.spv /XO /IS
robocopy shaders\ bin\Debug\net6.0-windows\shaders\ *.spv /XO /IS
:FINISH
pause