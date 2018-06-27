#include "TECADDON.h"
#include "ADDGLBL.h"
#include <string.h>


AddOn_pa AddOnID;


/**
 * This function is called when the
 * $!ADDONCOMMAND macro command is
 * processed.
 */
static Boolean_t STDCALL MacroCommandCallback(char *MacroCommandString,  /* IN */
                                              char **ErrMsg)             /* OUT (only if returning FALSE) */
{

  Boolean_t IsOk = TRUE;

  /* 
   * MacroCommandString is the add-on macro command string needing processing.
   *
   * *ErrMsg is an error message string which must be allocated and set by this
   * function if and only if the return value is FALSE.
   */
  
  TecUtilLockStart(AddOnID);
  
  /*
   * TODO: Process the macro command.
   *
   * Example:
   *
   * $!ADDONCOMMAND ADDONID='Modify Data' COMMAND='MYCOMMAND'
   */
  
  if (!strcmp(MacroCommandString,"MYCOMMAND")) /* For example */
    {
      /* IsOk = ProcessMacroCommand_MYCOMMAND(); */
    }

  if (!IsOk)
    {
      /*
       * Some kind of error, so inform the user about it.
       */
  
      *ErrMsg = TecUtilStringAlloc(1000,"String for Error Message");
      strcpy(*ErrMsg,"Error processing macro command");
    }
  else
    {
      /* Ignore the *ErrMsg parameter */
    }

  TecUtilLockFinish(AddOnID);
  return (IsOk);
}

/**
 */
static void STDCALL StateChangeCallback(StateChange_e StateChange)
{

  switch (StateChange)
    {
   /*
    * This function will be called by Tecplot
    * each time a state change occurs.
    *
    *
    * NOTE:
    *
    * Some State changes also have some supplemental "state"
    * information that can be retrieved if you desire.
    * Comments in the case statements below identify these
    * state changes.  To retrieve the supplemental information
    * use the functions TecUtilStateChangeGetXXXXX. You may
    * only call these functions during the scope of this
    * callback.  Once control returns from this call the
    * supplemental information will become unaccessible.
    *
    */

           /*   State Change                Supplemental information */

      case StateChange_VarsAltered:     /* set of altered variables */
      case StateChange_VarsAdded:       /* set of added variables */
      case StateChange_ZonesDeleted:    /* set of deleted zones */
      case StateChange_ZonesAdded:      /* set of added zones */
      case StateChange_NodeMapsAltered: /* set of node maps altered */
      case StateChange_MouseModeUpdate: /* the new mouse mode */
      case StateChange_Style:           /* Style Parameters P1,P2,P3,P4,P5,P6 */
      case StateChange_View:            /* View action (View_e) */
      case StateChange_Streamtrace:     /* Streamtrace action (Streamtrace_e) */
      case StateChange_AuxDataAltered:  /* Auxiliary Location (AuxDataLocation_e)*/
      case StateChange_AuxDataAdded:    /* Auxiliary Location (AuxDataLocation_e)*/
      case StateChange_AuxDataDeleted:  /* Auxiliary Location (AuxDataLocation_e)*/

    /* State changes which do not have any supplemental "state" information. */
      case StateChange_FrameDeleted:        /* A frame was delete */
      case StateChange_NewTopFrame:         /* A new frame has become the current frame */   
      case StateChange_Text:                /* One or more text elements has changed */
      case StateChange_Geom:                /* One or more geometry elements has changed */
      case StateChange_DataSetReset:        /* A new dataset has been loaded */
      case StateChange_NewLayout:           /* The current layout has been cleared and reset */
      case StateChange_CompleteReset:       /* Anything could have happened */
      case StateChange_LineMapAssignment:   /* A line mapping definition has been altered (includes zone and axis information) */
      case StateChange_ContourLevels:       /* The contour levels have been altered */
      case StateChange_ModalDialogLaunch:   /* A modal dialog has been launched */
      case StateChange_ModalDialogDismiss:  /* A modal dialog has been dismissed */
      case StateChange_QuitTecplot:         /* Tecplot is about to exit */
      case StateChange_ZoneName:            /* The name of a zone has been altered */
      case StateChange_VarName:             /* The name of a variable has been altered */
      case StateChange_LineMapName:           /* The name of an X-Y mapping has been altered */
      case StateChange_LineMapAddDeleteOrReorder: /* The set of existing X-Y mappings has been altered */
      case StateChange_ColorMap:            /* The color mapping has been altered */
      case StateChange_ContourVar:          /* The contour variable has been reassigned */
      case StateChange_NewAxisVariables:    /* The axis variables have been reassigned */
      case StateChange_PickListCleared:     /* All picked objects are unpicked */
      case StateChange_PickListGroupSelect: /* A group of objects has been added to the pick list */
      case StateChange_PickListSingleSelect:/* A single object has been added to or removed from the pick list */
      case StateChange_PickListStyle:       /* An action has been performed on all of the objects in the pick list */
      case StateChange_DataSetFileName:     /* The current data set has been saved to a file */
      case StateChange_DataSetTitle:        /* The current data set title has been changed */
      case StateChange_DataSetLockOn:       /* The dataset attached to the active frome in Tecplot has been locked */
      case StateChange_DataSetLockOff:      /* The dataset attached to the active frome in Tecplot has been unlocked */
      case StateChange_DrawingInterrupted:  /* The user has interrupted the drawing */


    /* Version 9 and later Note: If you are using modeless dialogs, you should
       trap the following state changes and take appropriate
       action when print preview is launched and dismissed.

       Usually you will either disable or close your dialog
       when print preview is launched. */

      case StateChange_PrintPreviewLaunch:  /* Modeless dialogs should close or disable themselves */
      case StateChange_PrintPreviewDismiss: /* Modeless dialogs can re-launch or enable themselves */


      case StateChange_SuspendInterface:    /* Replaces StateChange_DrawGraphicsOn */
      case StateChange_UnsuspendInterface:  /* Replaces StateChange_DrawGraphicsOff */
        {
          /* TODO: Add code to handle state changes.... */
        } break;
      default: break;
    } /* end switch */
}

/**
 * This function demonstrates four ways to modify large amounts of data in
 * Tecplot. It does nothing more than modify the specified zone (assumed to be
 * a rectangular) using four different techniques. Each of the four code blocks
 * has comments above them describing the technique along with some information
 * about it's performance relative to the other cases.
 */
static void ModifyData(EntIndex_t Zone)
{
  LgIndex_t    IMax, JMax, KMax;
  LgIndex_t    I, J;
  double       XMin, XMax;
  double       YMin, YMax;
  double       DX;
  double       DY;
  FieldData_pa XFD, YFD;

  TecUtilZoneGetInfo(Zone, &IMax, &JMax, &KMax,
                     NULL, NULL, NULL, NULL, NULL,
                     NULL, NULL, NULL, NULL, NULL);
  CHECK(KMax == 1); /* our example is only 2D */
  XFD = TecUtilDataValueGetRef(Zone, 1);
  YFD = TecUtilDataValueGetRef(Zone, 2);

  /* define the grid boundaries */
  XMin = 0.0;
  YMin = 0.0;
  XMax = 1.0;
  YMax = 1.0;
  DX = XMax - XMin;
  DY = YMax - YMin;

  /*
   * Case 1:
   *   Modify the data using Tecplot 10 Release 4's newly introduced get and
   *   set field value access functions. These functions perform no bounds
   *   checking and as a result can be up to an order of magnitude faster than
   *   TecUtilDataValueSetByRef.
   *
   *   Note that unlike most TecUtil API's that are ones based these access
   *   functions are zero based.
   */
    {
      FieldValueSetFunction_pf XSetFieldValue = TecUtilDataValueRefGetSetFunc(XFD);
      FieldValueSetFunction_pf YSetFieldValue = TecUtilDataValueRefGetSetFunc(YFD);
      for (J = 0; J < JMax; J++)
        {
          LgIndex_t BaseOffset = J*IMax;
          for (I = 0; I < IMax; I++)
            {
              /* calculate a value for the point */
              double XFract = (IMax == 1 ? 0.0 : ((double)I)/(IMax-1));
              double YFract = (JMax == 1 ? 0.0 : ((double)J)/(JMax-1));
              double XVal = XMin + DX*XFract;
              double YVal = YMin + DY*YFract;

              LgIndex_t PointIndex = I + BaseOffset; /* ...zero based indexing */
              XSetFieldValue(XFD, PointIndex, XVal);
              YSetFieldValue(YFD, PointIndex, YVal);
            }
        }
    }

   /*
    * Case 2:
    *   Modify the data using Tecplot's raw data pointer unless it is not
    *   directly accessible (i.e. raw data pointer is NULL). If it is not
    *   directly accessible then we use the same high performance get and set
    *   field value access functions as in Case 1. Directly accessing the raw
    *   data is about twice as fast as the high performance functions used in
    *   Case 1 (and in this case if the data is not directly accessible).
    *
    *   Keep in mind that in this particular example the high performance
    *   functions in the code below we will never be used because we just
    *   created the data ourselves before calling ModifyData() so we know that
    *   it will be directly accessible. However, once we release the data to
    *   Tecplot (after our outer call TecUtilLockFinish()) Tecplot might move
    *   the data in such a way that it is no longer directly accessible. Then
    *   future calls to this function, if they were made, may use the high
    *   performance get and set value access functions.
    *
    *   Note that this code could be made even more efficient if the data type
    *   check was performed outside the loops instead of inside the loops,
    *   however this would require more code and add complexity to the sample.
    *   Since efficiency is probably the reason for coding the the hybrid
    *   solution we recommended moving the data type check outside the loops.
    */
    {
      void            *XDataBuffer;
      void            *YDataBuffer;
      float           *XFloatBuffer  = NULL;
      float           *YFloatBuffer  = NULL;
      double          *XDoubleBuffer = NULL;
      double          *YDoubleBuffer = NULL;
      FieldDataType_e  XDataType;
      FieldDataType_e  YDataType;
      FieldValueSetFunction_pf XSetFieldValue = TecUtilDataValueRefGetSetFunc(XFD);
      FieldValueSetFunction_pf YSetFieldValue = TecUtilDataValueRefGetSetFunc(YFD);
      TecUtilDataValueGetRawPtr(Zone, 1, &XDataBuffer, &XDataType);
      TecUtilDataValueGetRawPtr(Zone, 2, &YDataBuffer, &YDataType);

      if (XDataType == FieldDataType_Float)
        XFloatBuffer = (float *)XDataBuffer;
      else if (XDataType == FieldDataType_Double)
        XDoubleBuffer = (double *)XDataBuffer;
      else /* if... more types would be added here */
        CHECK(FALSE);

      if (YDataType == FieldDataType_Float)
        YFloatBuffer = (float *)YDataBuffer;
      else if (YDataType == FieldDataType_Double)
        YDoubleBuffer = (double *)YDataBuffer;
      else /* if... more types would be added here */
        CHECK(FALSE);

      for (J = 0; J < JMax; J++)
        {
          LgIndex_t BaseOffset = J*IMax;
          for (I = 0; I < IMax; I++)
            {
              /* calculate a value for the point */
              double XFract = (IMax == 1 ? 0.0 : ((double)I)/(IMax-1));
              double YFract = (JMax == 1 ? 0.0 : ((double)J)/(JMax-1));
              double XVal = XMin + DX*XFract;
              double YVal = YMin + DY*YFract;

              LgIndex_t PointIndex = I + BaseOffset; /* ...zero based indexing */

              /* for higher performance move these tests outside the loops */
              if (XFloatBuffer != NULL)
                XFloatBuffer[PointIndex]  = (float)XVal;
              else if (XDoubleBuffer != NULL)
                XDoubleBuffer[PointIndex] = XVal;
              else
                XSetFieldValue(XFD, PointIndex, XVal);

              if (YFloatBuffer != NULL)
                YFloatBuffer[PointIndex]  = (float)YVal;
              else if (YDoubleBuffer != NULL)
                YDoubleBuffer[PointIndex] = YVal;
              else
                YSetFieldValue(YFD, PointIndex, YVal);
            }
        }
    }

  /*
   * Case 3:
   *   Modify the data using Tecplot 10 Release 3's newly introduced get and
   *   set field array functions. These functions perform bounds checking and
   *   provide high performance. These functions are nearly as fast as using
   *   raw data pointers in Case 2.
   */
    {
      /*
       * Allocate the buffers large enough to hold one row of I. We'll just
       * assume that I moves fastest. To be extra efficient we probably should
       * pick the largest of IMax or JMax and then adjust the inner and outer
       * loops and indexing accordingly. Note that as another short-cut we
       * allocate the buffers using the widest type, double, even though we may
       * use the buffer as a float array.
       */
      void            *XDataBuffer = (void *)malloc(IMax*sizeof(double));
      void            *YDataBuffer = (void *)malloc(IMax*sizeof(double));
      float           *XFloatBuffer  = NULL;
      float           *YFloatBuffer  = NULL;
      double          *XDoubleBuffer = NULL;
      double          *YDoubleBuffer = NULL;
      FieldDataType_e  XDataType = TecUtilDataValueGetRefType(XFD);
      FieldDataType_e  YDataType = TecUtilDataValueGetRefType(YFD);

      if (XDataType == FieldDataType_Float)
        XFloatBuffer = (float *)XDataBuffer;
      else if (XDataType == FieldDataType_Double)
        XDoubleBuffer = (double *)XDataBuffer;
      else /* if... more types would be added here */
        CHECK(FALSE);

      if (YDataType == FieldDataType_Float)
        YFloatBuffer = (float *)YDataBuffer;
      else if (YDataType == FieldDataType_Double)
        YDoubleBuffer = (double *)YDataBuffer;
      else /* if... more types would be added here */
        CHECK(FALSE);

      for (J = 0; J < JMax; J++)
        {
          LgIndex_t BaseOffset = J*IMax;
          LgIndex_t DestOffset = BaseOffset + 1; /* ...ones based indexing */
          for (I = 0; I < IMax; I++)
            {
              /* calculate a value for the point */
              double XFract = (IMax == 1 ? 0.0 : ((double)I)/(IMax-1));
              double YFract = (JMax == 1 ? 0.0 : ((double)J)/(JMax-1));
              double XVal = XMin + DX*XFract;
              double YVal = YMin + DY*YFract;

              /* for higher performance move these tests outside the loop */
              if (XFloatBuffer != NULL)
                XFloatBuffer[I]  = (float)XVal;
              else if (XDoubleBuffer != NULL)
                XDoubleBuffer[I] = XVal;
              else /* if ...more cases could be added */
                CHECK(FALSE);

              if (YFloatBuffer != NULL)
                YFloatBuffer[I]  = (float)YVal;
              else if (YDoubleBuffer != NULL)
                YDoubleBuffer[I] = YVal;
              else /* if ...more cases could be added */
                CHECK(FALSE);
            }
          TecUtilDataValueArraySetByRef(XFD, DestOffset, IMax, XDataBuffer);
          TecUtilDataValueArraySetByRef(YFD, DestOffset, IMax, YDataBuffer);
        }

      free(XDataBuffer);
      free(YDataBuffer);
    }

  /*
   * Case 4:
   *   Modify the data using Tecplot's ones based get and set field value
   *   functions. Unlike Case 1 these functions perform bounds checking and are
   *   better for debugging an add-on and can be used as a drop-in replacement
   *   for the functions used in Case 1 during development.
   */
    {
      for (J = 0; J < JMax; J++)
        {
          LgIndex_t BaseOffset = J*IMax;
          for (I = 0; I < IMax; I++)
            {
              /* calculate a value for the point */
              double XFract = (IMax == 1 ? 0.0 : ((double)I)/(IMax-1));
              double YFract = (JMax == 1 ? 0.0 : ((double)J)/(JMax-1));
              double XVal = XMin + DX*XFract;
              double YVal = YMin + DY*YFract;

              LgIndex_t PointIndex = I + BaseOffset + 1; /* ...ones based indexing */
              TecUtilDataValueSetByRef(XFD, PointIndex, XVal);
              TecUtilDataValueSetByRef(YFD, PointIndex, YVal);
            }
        }
    }
}

/**
 */
static void STDCALL MenuCallback(void)
{
# define FRAME_OFFSET 0.25 /* inches */
  Boolean_t     IsOk = TRUE;
  double        X, Y, Width, Height;
  StringList_pa VarNames = TecUtilStringListAlloc();

  TecUtilLockStart(AddOnID);

  IsOk = (VarNames != NULL &&
          TecUtilStringListAppendString(VarNames, "X") &&
          TecUtilStringListAppendString(VarNames, "Y"));
  if (IsOk)
    {
      FieldDataType_e VarDataType[] = {FieldDataType_Float, FieldDataType_Float};

      /*
       * Create a new frame in which we can create our new data. Make the new frame
       * the same size the current one but shifted down and to the right 1/4".
       */
      TecUtilFrameGetPosAndSize(&X, &Y, &Width, &Height);
      IsOk = IsOk && TecUtilFrameCreateNew(TRUE, X+FRAME_OFFSET, Y+FRAME_OFFSET, Width, Height);
      IsOk = IsOk && TecUtilMouseSetMode(Mouse_Select);

      /* create the dataset and a zone */
      IsOk = IsOk && TecUtilDataSetCreate("Modify Data Sample", VarNames, TRUE);
      IsOk = IsOk && TecUtilDataSetAddZone("Zone", 100, 100, 1,
                                           ZoneType_Ordered, VarDataType);
      if (IsOk)
        ModifyData(1); /* ...this code is what the sample demonstrates */

      if (IsOk)
        {
          /* broadcast our changes */
          ArgList_pa ArgList  = TecUtilArgListAlloc();
          Set_pa     ZoneList = TecUtilSetAlloc(FALSE);

          IsOk = (ArgList  != NULL &&
                  ZoneList != NULL &&
                  TecUtilSetAddMember(ZoneList,1,FALSE) &&
                  TecUtilSetAddMember(ZoneList,2,FALSE) &&
                  TecUtilArgListAppendInt(ArgList, SV_STATECHANGE, StateChange_ZonesAdded) &&
                  TecUtilArgListAppendSet(ArgList, SV_ZONELIST,    ZoneList));
          if (IsOk)
            TecUtilStateChangedX(ArgList);

          if (ArgList  != NULL) TecUtilArgListDealloc(&ArgList);
          if (ZoneList != NULL) TecUtilSetDealloc(&ZoneList);
        }
      else
        TecUtilFrameDeleteTop();

      /* set the appropriate state change */
      IsOk = IsOk && (TecUtilFrameSetPlotType(PlotType_Cartesian2D) == SetValue_Ok);
    }

  if (VarNames != NULL) TecUtilStringListDealloc(&VarNames);

  TecUtilLockFinish(AddOnID);
}

/**
 * When Tecplot first loads an add-on, it makes a 
 * call to initialize the add-on. This function
 * must be named InitTecAddOn, as shown below.
 */
EXPORTFROMADDON void STDCALL InitTecAddOn(void)
{


  /*
   * NOTE:  TecUtilLockOn MUST be used for InitTecAddOn instead
   *        of TecUtilLockStart because AddonID has yet to be
   *        established.  TecUtilLockOn is in effect an "annonymous"
   *        locking of tecplot (old style).
   */

  TecUtilLockOn();

  /*
   * The function TecUtilAddOnRegister() is the
   * only function that is REQUIRED to be called from
   * the initialization function.
   *
   * The information you give Tecplot by calling
   * this function will show up in the Help/About Add-ons
   * dialog box.
   */

  /*
   * Note that if your add-on requires a specific version of Tecplot,
   * you would check for that here using TecUtilGetTecplotVersion()
   */

  AddOnID = TecUtilAddOnRegister(100,
                                 ADDON_NAME,
                                 "V"ADDON_VERSION"("TecVersionId") "ADDON_DATE,
                                 "Tecplot, Inc.");


  TecUtilMacroAddCommandCallback(ADDON_NAME,
                                 MacroCommandCallback);
  {
    ArgList_pa ArgList;
    ArgList = TecUtilArgListAlloc();
    TecUtilArgListAppendFunction(ArgList, SV_CALLBACKFUNCTION, (const void *)StateChangeCallback);
    TecUtilArgListAppendInt(ArgList,      SV_STATECHANGEMODE,  StateChangeMode_v100);
    TecUtilStateChangeAddCallbackX(ArgList);
    TecUtilArgListDealloc(&ArgList);
  }
  TecUtilMenuAddOption("Tools",
                       "Modify Data",
                       '\0',
                       MenuCallback);



  /*
   * See note on TecUtilLockOn at start of this function.
   */
  TecUtilLockOff();
}

