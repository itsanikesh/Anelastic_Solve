#!MC 700
$!EXPORTSETUP
  EXPORTFORMAT = PS
  PALETTE = MONOCHROME
$!LOOP 10
$!OPENLAYOUT "batch.lay"
  ALTPLOTFNAMES = "d|LOOP|.plt"
$!EXPORTSETUP
  EXPORTFNAME = "d|LOOP|.out"
$!EXPORT
$!ENDLOOP
$!QUIT