Description:  This addon is used to add streamtrace starting locations
              where the starting locations form a circular pattern. 
              This is useful when placing streamtraces near a source 
              or a sink in a vector plot.


How to use    The user creates one or more circle geometries and
this addon:   then picks the ones to be used to determine the streamtrace
              starting locations.  Bring up the cstream dialog and
              press the button.  Streamtraces will be placed along
              all picked circles.


How this addon  When the user presses the button, the callback function
works:          does the following:

                   - Make sure a dataset is available,
                   - Make sure it is a 2d plot.
                   - Make sure U and V are assigned.
                   - loop through all items currently picked and find
                     ones that are circles.
                   - Determine the number of points used to draw the circle.
                     and the circle's origin and radius.
                   - Add streamtraces to tecplot.

TecUtil 
Functions used:

                TecUtilAddOnRegisterInfo
                TecUtilDataSetIsAvailable
                TecUtilDialogErrMsg
                TecUtilFrameGetMode
                TecUtilGeomCircleGetRadius
                TecUtilGeomEllipseGetNumPoints
                TecUtilGeomGetPositionCoordSys
                TecUtilGeomGetType
                TecUtilGeomGetXYZAnchorPos
                TecUtilLockOn
                TecUtilLockOff
                TecUtilMenuAddOption
                TecUtilPickListGetCount
                TecUtilPickListGetGeom
                TecUtilPickListGetType
                TecUtilStreamtraceAdd
                TecUtilVarGetNumByAssignment
