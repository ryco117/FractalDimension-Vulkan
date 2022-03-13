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

module AppConfig

open CommandLine

type Config = {
    // Visual Properties
    launchFullscreen: bool
    autoOrbitJerk: float32
    kaleidoscopeSpeed: float32

    //Audio scaling
    volumeScale: float32

    // Frequency ranges
    bassStartFreq: float
    bassEndFreq: float
    midsStartFreq: float
    midsEndFreq: float
    highStartFreq: float
    highEndFreq: float
    
    // Minimum volumes
    minimumBass: float32
    minimumMids: float32
    minimumHigh: float32
    minimumBassForJerk: float32}
    
[<Literal>]
let defaultVolumeScale = 1.f

[<Literal>]
let defaultLaunchFullscreen = false

let defaultConfig = {
    launchFullscreen = defaultLaunchFullscreen
    autoOrbitJerk = 0.1675f
    kaleidoscopeSpeed = 0.9f

    volumeScale = defaultVolumeScale

    bassStartFreq = 30.
    bassEndFreq = 125.
    midsStartFreq = 125.
    midsEndFreq = 1_100.
    highStartFreq = 1_100.
    highEndFreq = 15_000.
    
    minimumBass = 0.01f
    minimumMids = 0.0075f
    minimumHigh = 0.001f
    minimumBassForJerk = 0.05f}

type CommandLineOptions = {
    [<Option(shortName = 'v', longName = "volumeScale", Default = defaultVolumeScale, HelpText = "Factor to multiply the incoming audio signal by.")>]
    volumeScale: float32
    
    [<Option(shortName = 'f', longName = "launch-fullscreen", Default = defaultLaunchFullscreen, HelpText = "Toggle whether application should launch in fullscreen.")>]
    launchFullscreen: bool}

let (|Success|Fail|) (result : ParserResult<'a>) =
    match result with
    | :? Parsed<'a> as parsed -> Success(parsed.Value)
    | :? NotParsed<'a> as notParsed -> Fail(notParsed.Errors)
    | _ -> failwith "invalid parser result"

let defaultConfigWithArgs (args: string[]) =
    let result = Parser.Default.ParseArguments<CommandLineOptions> args
    match result with
    | Success opts -> {defaultConfig with volumeScale = opts.volumeScale; launchFullscreen = opts.launchFullscreen}
    | Fail errs -> failwith $"Invalid: %A{args}, Errors: %u{Seq.length errs}"