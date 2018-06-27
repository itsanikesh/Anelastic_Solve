



      INTEGER*4 Function ConverterCallback(DataFName,
     &                                     TempBinFName,
     &                                     MessageString)
      CHARACTER*(*) DataFName
      CHARACTER*(*) TempBinFName
      CHARACTER*(*) MessageString
      INCLUDE 'ADDGLBL.INC'
      INCLUDE 'FGLUE.INC'
      INTEGER*4 IsOk
      INTEGER*4 FileIsOpen
C
      INTEGER*4     FileNameSet
      INTEGER*4     ISkip
      INTEGER*4     NumVars
      INTEGER*4     NumPts
      REAL*4        ValueArr(500)
      INTEGER*4     InFileIsOpen
      INTEGER*4     TecXXXInitialized
      CHARACTER*500 VarNames
      IsOk = 1
      InFileIsOpen = 0
      TecXXXInitialized = 0

      call TecUtilLockOn()
C
C       Open the file
C
      OPEN(11,FILE=DataFName,STATUS='OLD',Err=99)
      InFileIsOpen=1
      GoTo 100
99    IsOk = 0
      Write(MessageString,*) 'Cannot open input file'
100   Continue

C
C       First number in the file is the number of variables (columns) in the file
C
      if (IsOk.Eq.1) Then
        Read(11,*,Err=999) NumVars
        if (NumVars.LT.2) Then
          IsOk = 0
          Write(MessageString,*) 'Invalid NumVars value at top of file'
        Endif
      Endif

C
C       Second number in the file is the number of datapoints in the file
C
      if (IsOk.Eq.1) Then
        Read(11,*,Err=999) NumPts
        if (NumPts.LT.2) Then
          IsOk = 0
          Write(MessageString,*) 'Invalid NumPts value at top of file'
        Endif
      Endif
C
C     Set up the dataset.
C
      if (IsOk.Eq.1) Then
C
C       First build the string containing the names of the variables.
C       Only can handle up to 99 variables.
C
        IPos = 1
        Do 20 I = 1,NumVars
          VarNames(IPos:IPos) = 'V'
          IPos = IPos + 1
          ITens = I/10
          VarNames(IPos:IPos) = Char(IChar('0')+ITens)
          IPos = IPos + 1
          IOnes = I - ITens*10
          VarNames(IPos:IPos) = Char(IChar('0')+IOnes)
          IPos = IPos + 1
          VarNames(IPos:IPos) = ' '
          IPos = IPos + 1
   20   Continue
        IPos = IPos - 1
C
C       Initialize the dataset
C

        IErr = TecUtilTecIni('Some Title'//char(0),
     &                       VarNames(1:IPos)//char(0),
     &                       TempBinFName//char(0),
     &                       '.'//char(0),
     &                       0,
     &                       0)
        if (IErr.NE.0) Then
          IsOk = 0
          Write(MessageString,*) 
     &    'Error initializing dataset for writing'
        Else
          TecXXXInitialized = 1
        Endif
      Endif
C
C       Add a zone.
C
      if (IsOk.Eq.1) Then
        IErr = TecUtilTecZne('Zone title'//char(0),
     &                       NumPts,
     &                       1,
     &                       1,
     &                       'POINT'//char(0),
     &                       char(0))
        if (IErr.NE.0) Then
          IsOk = 0
          Write(MessageString,*) 
     &    'Error initializing zone for converting dataset'
        Endif
      Endif
C
C     Read in the data and stuff the values in one point at a time.  Not
C     real efficient but avoids having to create a large array.
C
      if (IsOk.Eq.1) Then
        Do 30 I = 1,NumPts
        Read(11,*,Err=999) (ValueArr(N),N=1,NumVars)
        IErr = TecUtilTecDat(NumVars,ValueArr,0)
        if (IErr.NE.0) Then
          IsOk = 0
          Write(MessageString,*) 'Error loading data for point:',I
          GoTo 31
        Endif
   30   Continue
      Endif
   31 Continue
C
      IErr = 0
      if (TecXXXInitialized.Eq.1) IErr = TecUtilTecEnd()
      if (IErr.NE.0.And.IsOk.Eq.1) Then
        IsOk = 0
        Write(MessageString,*) 
     &    'Error converting datafile'
      Endif
C
      GoTo 1000
 999  IsOk = 0
C...  Still need to call TecUtilTecEnd to clean up.
      if (TecXXXInitialized.Eq.1) IErr = TecUtilTecEnd()
      Write(MessageString,*) 
     &    'I/O Error while processing datafile'
1000  if (InFileIsOpen.Eq.1) Then
        Close(11)
      Endif
C 
      Call TecUtilLockOff()
      ConverterCallback = IsOk
      Return
      End

