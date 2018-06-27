      subroutine TrimString(S)
      character*(*) S
      integer*4 I
      integer*4 LastNonSpace
      I = 1
      LastNonSpace = 0
      ISLen = Len(S)
   10 if (I.LT.ISLen) Then
      if (S(I:I).NE.' ') LastNonSpace = I
      I = I + 1
      GoTo 10
      Endif
      if (LastNonSpace.NE.0) S(LastNonSpace+1:LastNonSpace+1) = CHAR(0)
      Return
      End



      INTEGER*4 Function LoaderCallback(InstructionPtr)
      INCLUDE 'ADDGLBL.INC'
      INCLUDE 'FGLUE.INC'
      INCLUDE 'GUI.INC'
      INCLUDE 'GUIDEFS.INC'
      POINTER (InstructionPtr,Instructions)
      INTEGER*4     IsOk
      INTEGER*4     Count
      INTEGER*4     I
      INTEGER*4     SLen
      CHARACTER*256 INSTRING
C
C... Begin customization ....
C
      POINTER       (VarListPtr,VarList)
      INTEGER*4     FileNameSet
      CHARACTER*512 FName
      CHARACTER*6   VarName
      INTEGER*4     ISkip
      INTEGER*4     NumVars
      INTEGER*4     NumPts
      INTEGER*4     FinalNumPts
      integer*4     IVarDataType(500)
      REAL*8        ValueArr(500)
      INTEGER*4     FileIsOpen

      IsOk       = 1
      FileIsOpen = 0
      ISkip      = 1

      Call TecUtilLockOn()
  
      Count = TecUtilStringListGetCount(InstructionPtr)

      i = 1
    1 if (i.ge.Count) GoTo 10
        Call TecUtilStringListGetString(InstructionPtr,i,INSTRING,SLen)
C
C       The instruction is now in the variable INSTRING and has a length 
C       of SLen
C
        if (INSTRING(1:SLen).Eq.'-FILENAME') Then
C
C         ... Found the filename.
C
          i = i + 1
          Call TecUtilStringListGetString(InstructionPtr,i,FName,SLen)
          FileNameSet = 1
        Else if (INSTRING(1:SLen).Eq.'-SKIP') Then
          i = i + 1
          Call TecUtilStringListGetString(InstructionPtr,i,
     &                                    INSTRING,SLen)
          Read(INSTRING,'(I)',Err=88) ISkip
          if (ISkip.Lt.1) GoTo 88
          GoTo 89
   88     IsOk = 0
          Call TecUtilDialogErrMsg(
     &    'Invalid Skip parameter in for example fortran data loader')
   89     Continue
        Else
          IsOk = 0
          Call TecUtilDialogErrMsg(
     &    'Invalid instructions for example fortran data loader')
        Endif

      i = i + 1
      GoTo 1
   10 Continue

      if (FileNameSet.Eq.0) Then
        IsOk = 0
        Call TecUtilDialogErrMsg(
     &  'Missing filename for example fortran data loader')
      Endif

C
C       Open the file
C
      if (IsOk.Eq.1) Then
        OPEN(11,FILE=FName,STATUS='OLD',Err=99)
        FileIsOpen=1
        GoTo 100
99      IsOk = 0
        Call TrimString(FName)
        Call TecUtilDialogErrMsg(
     &    'Cannot open file: '//FName//char(0))
100     Continue
      Endif

C
C       First number in the file is the number of variables (columns) in the file
C
      if (IsOk.Eq.1) Then
        Read(11,*,Err=999) NumVars
        if (NumVars.LT.2) Then
          IsOk = 0
          Call TecUtilDialogErrMsg(
     &    'Invalid NumVars value at top of file')
        Endif
      Endif

C
C       Second number in the file is the number of datapoints in the file
C
      if (IsOk.Eq.1) Then
        Read(11,*,Err=999) NumPts
        if (NumPts.LT.2) Then
          IsOk = 0
          Call TecUtilDialogErrMsg(
     &    'Invalid NumPts value at top of file')
        Endif
      Endif
C
C     Set up the dataset.
C
      FinalNumPts = 1 + (NumPts-1)/ISkip
      if (IsOk.Eq.1) Then
C
C       First build the stringlist of var names....
C       Also load the Var datatype array for future use.
C
        call TecUtilStringListAlloc(VarListPtr)
        Do 20 I = 1,NumVars
          Write(VarName,22) I
   22   FORMAT('V',I2)
          IsOk = TecUtilStringListAppendString(VarListPtr, 
     &                                         VarName//char(0))
          IVarDataType(I) = FIELDDATATYPE_FLOAT
   20   Continue
C
C       Create the dataset
C
        IsOk = TecUtilDataSetCreate('Some title'//char(0),
     &                              VarListPtr,TRUE)
        call TecUtilStringListDealloc(VarListPtr)
      Endif
C
C       Add a zone.
C
      if (IsOk.Eq.1) Then
        IsOk = TecUtilDataSetAddZone('Zone for Example Loader'//char(0),
     &                               FinalNumPts,
     &                               1,
     &                               1,
     &                               ZONETYPE_ORDERED,
     &                               IVarDataType)
      Endif

C
C     Read in the data.  For the purposes of this example just use
C     TecUtilDataValueSetByZoneVar.  This is very inefficient.  Better
C     to use raw or by ref.  See other fortran examples for this.
C
      IFinal = 1
      if (IsOk.Eq.1) Then
        Do 35 I = 1,NumPts
        Read(11,*,Err=999) (ValueArr(N),N=1,NumVars)
        if (MOD(I-1,ISkip).Eq.0) Then
          Do 30 N = 1,NumVars
   30       IsOk = TecUtilDataValueSetByZoneVar(1,N,IFinal,ValueArr(N))
          IFinal = IFinal + 1
        Endif
   35   Continue
      Endif
C
C     Inform tecplot of the dataset load instructions.
C
      if (IsOk.Eq.1) Then
        IsOk = TecUtilImportSetLoaderInstr(
     &         'Fortran Example Loader'//char(0),
     &         InstructionPtr)
      Endif
C
C     Start tecplot off in XY plot frame mode....
C
      if (IsOk.Eq.1) Then
        III = TecUtilFrameSetMode(Frame_XY)
      Endif
      GoTo 1000
 999  IsOk = 0
      Call TecUtilDialogErrMsg(
     &    'I/O Error processing datafile')
C
1000  if (FileIsOpen.Eq.1) Then
        Close(11)
      Endif

      Call TecUtilLockOff()
      LoaderCallback = IsOk
      Return
      End






      Subroutine LoaderSelectedCallback()
      INCLUDE 'ADDGLBL.INC'
      INCLUDE 'FGLUE.INC'
      INCLUDE 'GUI.INC'
      INCLUDE 'GUIDEFS.INC'
C
C    This function is called when
C    the user selects this dataset
C    loader from the list of importers
C    in the File/Import dialog (as below).
C    
      Call TecUtilLockOn()
      Call BuildDialog1(MAINDIALOGID)
      Call TecGUIDialogLaunch(Dialog1Manager)
      Call TecUtilLockOff()
      Return
      End
