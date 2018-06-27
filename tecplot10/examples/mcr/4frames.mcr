#!MC 700

#
#       Read in the layout file created in step 1. 
#
$!OpenLayout "4frames.lay"
#
#       Set up exporting to create raster metafile images. 
#       Use a macro variable to instruct the $!EXPORT 
#       command to append the images only after the first
#       image has been written. This prevents tacking
#       more images onto a file that exists prior to running
#       this macro.
#
$!ExportSetup
  ExportFName   = "4frames.rm"
  ExportFormat  = RasterMetafile
  BitDumpRegion = ALLFRAMES
$!VarSet |DoAppend| = "No"
#
# Loop to write the frames for the I-Planes
# Note use of intrinsic variable "MAXI"
#
$!Loop |MAXI|
$!RunMacroFunction "ProcessPlane" (I,|LOOP|)
$!EndLoop
#
# Loop to write the frames for the J-Planes
#
$!Loop |MAXJ|
$!RunMacroFunction "ProcessPlane" (J,|LOOP|)
$!EndLoop
#
# Loop to write the frames for the K-Planes
#
$!Loop |MAXK|
$!RunMacroFunction "ProcessPlane" (K,|LOOP|)
$!EndLoop
$!Quit
#
# Macro function "ProcessPlane".  This function
# takes two parameters, The first parameter is
# the plane type and the second parameter is the
# plane index.
#
$!MacroFunction 
  Name   = "ProcessPlane"
  Retain = No
#
# One at a time operate on each frame.
#
$!Loop 4
#
# Set the IJK-Mode to show a single Plane.
# Since the IRange only effects the drawing
# if the planes being drawn are the I-Planes
# then it is ok to set all I,J, and K-Planes
# at the same time.
#
$!FIELD
  IJKMode
{ CellType = PLANES
  Planes = |1|
  IRange 
    {
      Min = |2|
      Max = |2|
    }
  JRange
    {
      Min = |2|
      Max = |2|
    }
  KRange
    {
      Min = |2|
      Max = |2|
    }
}
#
# End of loop on frames, Just Push the top frame.
# This will bring the next frame to the top 
#
$!FrameControl PushTop
$!EndLoop
#
# Redraw the image with the new setting and then
# dump the image to the file.
#
$!RedrawAll
#
# Export the image. If this is not the first image
# then instruct the export command to append
#
$!Export
  Append = |DoAppend|
$!VarSet |DoAppend| = "Yes"
$!EndMacroFunction
