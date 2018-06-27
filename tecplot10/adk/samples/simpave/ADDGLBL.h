#ifndef ADDGLBL_H_ /* Only include once */
#define ADDGLBL_H_

#define ADDON_NAME      "SimpAvg"
#define ADDON_VERSION   "1.0"
#define ADDON_DATE      __DATE__

extern AddOn_pa AddOnID;

/* DOCSTART:gr/simpavg_CurveParams_decl.txt */
typedef struct
  {
    Boolean_t UseIndVarRange;
    double    IndVarMin;
    double    IndVarMax;
  } CurveParams_s;
/* DOCEND */

#endif /* ADDGLBL_H_ */



 
