#!MC 900

$!Macrofunction Name = "3D Rotation Animation"
  ShowInMacroPanel = True
$!PromptForTextString |RotationAxis|
  Instructions = "Enter the axis for the rotation."
$!PromptForTextString |RotationAngle|
  Instructions = "Enter the number of degrees for each rotation."
$!PromptForTextString |NumSteps|
  Instructions = "Enter the number of rotations."
$!PromptForTextString |Animation|
  Instructions = "Enter 0 for No Animation File, 1 for an AVI file, 2 for an RM file."
$!Interface ShowWaitDialogs = No
$!If |Animation| != 0
$!Varset |format| = "AVI"
$!Varset |Extension| = "AVI"
$!If |Animation| == 2
$!Varset |format| = "Rastermetafile"
$!Varset |Extension| = "RM"
$!Endif
$!EXPORTSETUP EXPORTFORMAT = |format|
$!EXPORTSETUP IMAGEWIDTH = 546
$!EXPORTSETUP EXPORTFNAME = "|RotationAxis|AxisRotation.|Extension|"
$!EXPORTSTART 
$!Endif
$!Loop |NumSteps|
$!ROTATE3DVIEW |RotationAxis|
  ANGLE = |RotationAngle|
  ROTATEORIGINLOCATION = DEFINEDORIGIN
$!Redraw
$!If |Animation| != 0
$!EXPORTNEXTFRAME 
$!Endif
$!Endloop
$!If |Animation| != 0
$!EXPORTFINISH 
$!Endif
$!Pause "Animation is completed.  If the rotated image is off-center, reset the center of rotation and animate again."
$!Endmacrofunction

$!Macrofunction Name = "Reset Center of Rotation"
  ShowInMacroPanel = True
$!Varset |CenterX| = (|maxx| - (|maxx|-|minx|)/2)
$!Varset |CenterY| = (|maxy| - (|maxy|-|miny|)/2)
$!Varset |CenterZ| = (|maxz| - (|maxz|-|minz|)/2)
$!GLOBALTHREED ROTATEORIGIN{X = |CenterX|}
$!GLOBALTHREED ROTATEORIGIN{Y = |CenterY|}
$!GLOBALTHREED ROTATEORIGIN{Z = |CenterZ|}
$!View Datafit
$!Endmacrofunction
