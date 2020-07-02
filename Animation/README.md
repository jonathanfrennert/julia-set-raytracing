# Animation

Create MP4 videos of ray-traced quaternion Julia sets. 

## Dependencies (excluding GHC & Cabal) 

- ### REPA (Haskell)
Repa provides high performance, regular, multi-dimensional, shape polymorphic parallel arrays. All numeric data is stored unboxed (https://hackage.haskell.org/package/repa).

- ### JuicyPixels (Haskell)
This library can load and store images in PNG,Bitmap, Jpeg, Radiance, Tiff and Gif images (https://hackage.haskell.org/package/JuicyPixels).

- ### FFmpeg
FFmpeg is a free and open-source project consisting of a vast software suite of libraries and programs for handling video, audio, and other multimedia files and streams (https://www.ffmpeg.org/).

#### Installation
For each Haskell dependency: 
1. Open Command Prompt 
2. Enter ` cabal install <dependency name> `, e.g. ` cabal install repa `

For FFmpeg
Download installer from https://www.ffmpeg.org/download.html.

#### 2. How to use 

When using the executable:
1. Open up the command prompt and enter the directory containing the haskell modules.
2. Compile the main module by entering into the command prompt `ghc --make Main -o AnQJS -outputdir bin -threaded -O2 -with-rtsopts="-qa -A128m -n2m"` 
3. To create an an MP4 video write ` AnQJS.exe <name> <default/custom> +RTS -N<cores> `

If you type in default for the second argument, ` <default/custom> `, default settings will be used to write the MP4 file at ` videos\<name>.mp4 `. If ` custom ` is the second argument, you will type in certain settings given a series of prompts. More advanced settings can be found within the ` settings.hs ` file. The ` <cores> ` is the number of CPU cores you want to use, e.g. ` -N6 ` for using six CPU cores. 

The frames folder is were all frames are placed for FFmpeg to put together as an MP4 video. 
