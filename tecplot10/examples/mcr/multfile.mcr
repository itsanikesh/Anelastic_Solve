#!MC 700
#       Use a variable to store the number of 
#       files to process.
$!VarSet |NumFiles| = 50
#       Make sure the output is PostScript. 
$!ExportSetup
  ExportFormat    = PS
  Palette         = MONOCHROME
 #       Begin the loop
$!Loop |NumFiles|
#       Here is where we make use of the special feature
#       (i.e. the ALTPLOTFNAMES option) of the $!OPENLAYOUT 
#       command that allows us to override the named 
#       datafiles within the layout file. Also make use of 
#       the intrinsic LOOP macro variable.
$!OpenLayout "cont.lay"
  AltPlotFNames = "t|LOOP|.plt"
#
#       Set the name of the file to be printed.
#
$!ExportSetup
  ExportFName = "t|LOOP|.ps"
#
#       Create the postscript file.
#
$!Export
$!EndLoop
$!Quit
