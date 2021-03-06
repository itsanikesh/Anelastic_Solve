#!MC 800
#
# This file is an example of a tecplot quick macro file.
#
$!MacroFunction Name = "Open cylinder.plt"
$!READDATASET  '".\Demo\plt\cylinder.plt" ' 
  READDATAOPTION = NEW
  RESETSTYLE = YES
  INCLUDETEXT = NO
  INCLUDEGEOM = NO
  INCLUDECUSTOMLABELS = NO
  VARLOADMODE = BYNAME
  VARNAMELIST = '"X(M)" "Y(M)" "Z(M)" "U(M/S)" "V(M/S)" "W(M/S)" "P(N)" "T(K)" "R(KG/S)" "F(x,y)"' 
$!EndMacroFunction

$!MacroFunction Name = "Make Contour Plot"
$!Drawgraphics No
$!FIELDLAYERS SHOWCONTOUR = YES
$!GLOBALCONTOUR VAR = 3
$!FIELDLAYERS SHOWMESH = NO
$!FIELDLAYERS SHOWBOUNDARY = NO
$!FIELD [1-3]  CONTOUR{CONTOURTYPE = BOTHLINESANDFLOOD}
$!FIELD [1-3]  CONTOUR{COLOR = BLACK}
$!GLOBALCONTOUR LEGEND{SHOW = YES}
$!GLOBALCONTOUR LEGEND{BOX{BOXTYPE = FILLED}}
$!GLOBALCONTOUR LEGEND{BOX{FILLCOLOR = CUSTOM2}}
$!GLOBALCONTOUR LABELS{SHOW = YES}
$!GLOBALCONTOUR LABELS{GENERATEAUTOLABELS = NO}
$!GLOBALCONTOUR VAR = 9
$!REDRAW 
$!CONTOURLABELS ADD
  X = 7.96651283119
  Y = 1.00416716105
  Z = 0
  ISALIGNED = YES
$!GLOBALCONTOUR LABELS{NUMFORMAT{FORMATTING = FIXEDFLOAT}}
$!GLOBALCONTOUR LABELS{NUMFORMAT{PRECISION = 2}}
$!CONTOURLABELS ADD
  X = 4.45400949447
  Y = -1.35042128259
  Z = 0
  ISALIGNED = YES
$!REDRAW 
$!CONTOURLEVELS ADD
  RAWDATA
1
1.05696266394
$!REDRAW 
$!CONTOURLEVELS ADD
  RAWDATA
1
1.20260387862
$!CONTOURLABELS ADD
  X = -2.39537201211
  Y = -1.2196108135
  Z = 0
  ISALIGNED = YES
$!CONTOURLABELS ADD
  X = 2.74166411783
  Y = -0.957989875316
  Z = 0
  ISALIGNED = YES
$!CONTOURLEVELS DELETENEAREST
  RANGEMIN = 1.20228581368
$!CONTOURLEVELS DELETENEAREST
  RANGEMIN = 1.15574034782
$!CONTOURLEVELS DELETENEAREST
  RANGEMIN = 1.11025229435
$!DrawGraphics Yes
$!REDRAW 
$!EndMacroFunction


$!MacroFunction Name = "Create Scatter Plot"
$!FIELDLAYERS SHOWCONTOUR = NO
$!FIELDLAYERS SHOWMESH = NO
$!GLOBALCONTOUR VAR = 8
$!FIELDLAYERS SHOWSCATTER = YES
$!FIELD [1-3]  SCATTER{SYMBOLSHAPE{ISASCII = NO}}
$!FIELD [1-3]  SCATTER{SYMBOLSHAPE{GEOMSHAPE = CIRCLE}}
$!FIELD [1-3]  SCATTER{COLOR = BLACK}
$!FIELD [1-3]  SCATTER{ISFILLED = YES}
$!FIELD [1-3]  SCATTER{FILLCOLOR = MULTI}
$!GLOBALSCATTER VAR = 7
$!FIELD [1-3]  SCATTER{SIZEBYVARIABLE = YES}
$!GLOBALSCATTER VAR = 4
$!REDRAW 
$!EndMacroFunction

$!MacroFunction Name = "Create Streamtraces"
$!FIELDLAYERS SHOWSCATTER = NO
$!GLOBALTWODVECTOR UVAR = 4
$!GLOBALTWODVECTOR VVAR = 5
$!RESETVECTORLENGTH 
$!GLOBALCONTOUR VAR = 7
$!FIELDLAYERS SHOWVECTOR = YES
$!STREAMTRACE ADD
  STREAMTYPE = TWODLINE
  STARTPOS
    {
    X = -2.21974684528
    Y = 0.873356691962
    }
$!STREAMTRACE ADD
  STREAMTYPE = TWODLINE
  STARTPOS
    {
    X = -2.22462532213
    Y = 0.655339243477
    }
$!STREAMTRACE ADD
  STREAMTYPE = TWODLINE
  STARTPOS
    {
    X = -2.22950379899
    Y = 0.437321794991
    }
$!STREAMTRACE ADD
  STREAMTYPE = TWODLINE
  STARTPOS
    {
    X = -2.23438227585
    Y = 0.219304346505
    }
$!STREAMTRACE ADD
  STREAMTYPE = TWODLINE
  STARTPOS
    {
    X = -2.2392607527
    Y = 0.00128689801996
    }
$!STREAMTRACE ADD
  STREAMTYPE = TWODLINE
  STARTPOS
    {
    X = -2.24413922956
    Y = -0.216730550466
    }
$!STREAMTRACE ADD
  STREAMTYPE = TWODLINE
  STARTPOS
    {
    X = -2.24901770642
    Y = -0.434747998951
    }
$!STREAMTRACE ADD
  STREAMTYPE = TWODLINE
  STARTPOS
    {
    X = -2.25389618327
    Y = -0.652765447437
    }
$!STREAMTRACE ADD
  STREAMTYPE = TWODLINE
  STARTPOS
    {
    X = -2.25877466013
    Y = -0.870782895922
    }
$!STREAMTRACE ADD
  STREAMTYPE = TWODLINE
  STARTPOS
    {
    X = -2.26365313699
    Y = -1.08880034441
    }
$!GLOBALSTREAM COLOR = MULTI
$!GLOBALSTREAM LINETHICKNESS = 0.4
$!FIELDLAYERS SHOWVECTOR = NO
$!STREAMTRACE ADD
  STREAMTYPE = TWODLINE
  STARTPOS
    {
    X = -2.39537201211
    Y = 1.44020205802
    }
$!STREAMTRACE ADD
  STREAMTYPE = TWODLINE
  STARTPOS
    {
    X = -1.51724617793
    Y = 2.22506487257
    }
$!STREAMTRACE ADD
  STREAMTYPE = TWODLINE
  STARTPOS
    {
    X = 2.43432007587
    Y = 0.219304346505
    }
$!STREAMTRACE ADD
  STREAMTYPE = TWODLINE
  STARTPOS
    {
    X = 1.51228794998
    Y = -0.173127060768
    }
$!STREAMTRACE ADD
  STREAMTYPE = TWODLINE
  STARTPOS
    {
    X = -1.60505876135
    Y = -2.22249107653
    }
$!STREAMTRACE ADD
  STREAMTYPE = TWODLINE
  STARTPOS
    {
    X = -0.595214052048
    Y = -2.61492248381
    }
$!STREAMTRACE ADD
  STREAMTYPE = TWODLINE
  STARTPOS
    {
    X = -2.43927830382
    Y = -1.65564571047
    }
$!FIELDLAYERS SHOWSCATTER = YES
$!FIELD [1-3]  SCATTER{SIZEBYVARIABLE = NO}
$!FIELD [1-3]  SCATTER{FRAMESIZE = 0.5}
$!REDRAW 
$!EndMacroFunction
