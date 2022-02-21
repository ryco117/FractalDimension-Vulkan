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

type Config = {
    //Audio scaling
    volumeScale: float32
    autoOrbitJerk: float32

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
    
let defaultConfig = {
    volumeScale = 1.f
    autoOrbitJerk = 0.18f

    bassStartFreq = 20.
    bassEndFreq = 250.
    midsStartFreq = 250.
    midsEndFreq = 3000.
    highStartFreq = 3000.
    highEndFreq = 15000.
    
    minimumBass = 0.0075f
    minimumMids = 0.002f
    minimumHigh = 0.00075f
    minimumBassForJerk = 0.0825f}