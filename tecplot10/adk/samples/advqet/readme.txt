Tecplot Advanced Quick Edit Tool

1.0 Introduction

    The Advanced Quick Edit Tool (AQET) serves two purposes: first, it
    demonstrates an add-on's use of Tecplot's pick list and second it
    provides a useful add-on that nicely complements Tecplot's standard
    Quick Edit Dialog.

    The AQET dialog and related interface controls were created using the
    Tecplot GUI Builder (TGB). The guts to the callback stubs were then
    added to provide the desired functionality.

    Access to the AQET is achieved by selecting it from Tecplot's "Tools"
    menu. If the option can not be found under the "Tools" menu then Tecplot
    was not instructed to load the add-on. Edit the "tecplot.add" file,
    insert the following line, and restart Tecplot (see the ADK Reference
    manual for details):

      $!LoadAddOn "advqet"

    Controls on the AQET dialog are sensitive to user commands only when
    one or more geometry and/or text objects are selected. Some controls
    are specific to geometry or text and some apply to both. If the picked
    list contains a mix of geometry and text objects then the controls that
    apply specifically to geometric objects will only effect those objects.
    The same is true for those controls that apply specifically to text.


2.0 Controls

    Geometric specific controls allow the user to change the coordinate
    system of one or more geometry objects. Text specific controls allow
    the user to change the coordinate and unit system, box margin, line
    spacing, and anchor position of one or more text objects. General
    controls that apply to both text and geometric objects allow the
    user to change the scope, and zone attachment of one or more objects.


3.0 TecUtil Usage

    Following is a categorized list of the TecUtil functions used by this
    example add-on:

    General utilities:
        TecUtilRedraw
        TecUtilStringDealloc
        TecUtilSetDealloc
        TecUtilSetIsMember

    Add-on setup and registration:
        TecUtilAddOnRegisterInfo
        TecUtilMenuAddOption
        TecUtilStateChangeAddCallback

    Traversal and access to the geometry and text objects
    selected interactively or programatically:
        TecUtilPickListGetCount
        TecUtilPickListGetGeom
        TecUtilPickListGetText
        TecUtilPickListGetType

    Coordinate and unit system conversion:
        TecUtilConvertUnits
        TecUtilConvertXDimension
        TecUtilConvertXPosition
        TecUtilConvertYDimension
        TecUtilConvertYPosition

    Dataset and zone access:
        TecUtilDataSetGetInfo
        TecUtilDataSetIsAvailable
        TecUtilZoneGetEnabled
        TecUtilZoneGetName

    Geometry and text access:
        TecUtilGeom2DMPolyGetPoint
        TecUtilGeom2DMPolySetPoint
        TecUtilGeom3DMPolyGetPoint
        TecUtilGeom3DMPolySetPoint
        TecUtilGeomCircleGetRadius
        TecUtilGeomCircleSetRadius
        TecUtilGeomEllipseGetSize
        TecUtilGeomEllipseSetSize
        TecUtilGeomGetPositionCoordSys
        TecUtilGeomGetType
        TecUtilGeomGetXYZAnchorPos
        TecUtilGeomMPolyGetPointCount
        TecUtilGeomMPolyGetPolylineCnt
        TecUtilGeomRectangleGetSize
        TecUtilGeomRectangleSetSize
        TecUtilGeomSetAttachToZone
        TecUtilGeomSetPositionCoordSys
        TecUtilGeomSetScope
        TecUtilGeomSetXYZAnchorPos
        TecUtilGeomSetZone
        TecUtilGeomSquareGetSize
        TecUtilGeomSquareSetSize

        TecUtilTextBoxSetMargin
        TecUtilTextGetHeight
        TecUtilTextGetPositionCoordSys
        TecUtilTextGetSizeUnits
        TecUtilTextGetXYPos
        TecUtilTextSetAnchor
        TecUtilTextSetAttachToZone
        TecUtilTextSetHeight
        TecUtilTextSetLineSpacing
        TecUtilTextSetPositionCoordSys
        TecUtilTextSetScope
        TecUtilTextSetSizeUnits
        TecUtilTextSetXYPos
        TecUtilTextSetZone
