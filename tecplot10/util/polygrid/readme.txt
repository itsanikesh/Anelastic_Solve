The polygrid utility is used to convert "almost-random" data into 
finite-element data.  The random data must be arranged in 
non-intersecting polylines where each polyline can have any number
of points.


Data Examples where Polygrid is Useful:


Example 1.

Data is collected on the surface of some irregularly shaped object.
The object is first sliced at constant Z-Stations and the data 
is measured at randomly spaced points along the circumference of
each slice.



Example 2.

Data is collected by an NOAA research vessel at sea.  Measurements are
collected along a single path taken by the vessel.  At various points
along the path, the vessel stops and drops a device that takes 
measurements at varying depths.  In the following picture a pound
symbol (#) represents points where measurements are taken


------------- path of vessel ----->
+-----+-----+------------+-----------+---------------+------+-----+
|     |     |            #           |               |      |     #
|     #     #            |           #               #      #     |
#     |     |            |           |               |      |     |
|     |     #            |           |               #      |     #
|     #     |            #           |               |      |      
#           #            |           #               |      #      
|           |            #           |               |      |      
#           #                        #               |      #      
                                                     #

The exact data format is outlined in the comments at the top of
polygrid.c.  The file sphere.pol is a complete example input file.
To process sphere.pol do the following:

    1.  Compile polygrid (if it is not already compiled).

        For UNIX, type:

               make polygrid

        For Windows, see instructions on compiling for your compiler.

    2.  Run polygrid.

        In UNIX and Windows, type:

               polygrid [maxptsperline] < sphere.pol | preplot - sphere.plt

"sphere.plt" is now ready to be read into Tecplot.





FILE FORMAT:


 This program takes as input a set of arbitrarily shaped 2D or
 3D polylines.  The ouput is a set of triangles that forms the panels
 to completely connect each set of adjacent polylines.  This output
 is in the form required by Tecplot. 

 The input is of the form:

 IConnect           | 1=Connect first and last points, 0=Do not.
 NDim               | Number of dimensions (2 or 3)
 NADDV              | Number of additional variables (excl. X,Y [and Z])
 N1                 | Number of points in the first polyline 
 X1 Y1 Z1 V11 V12.. | Coordinate and field data for first point       
 X2 Y2 Z2 V21 V22.. | Coordinate and field data for second point      
 .                  | .
 .                  | .
 .                  | .
 XN1 YN1 ZN1 VN1..  | Coordinate and field data for last point
 N2                 | Number of points in second polyline.
 X1 Y1 Z1 V11 V12.. | Coordinate and field data for first point       
 X2 Y2 Z2 V21 V22.. | Coordinate and field data for second point      
 .                  | .
 .                  | .
 .                  | .
 XN2 YN2 ZN2 VN1..  | Coordinate and field data for last point of 
 .                    the second polyline
 .
 .
 as many polylines as you want.


