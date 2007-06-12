/*
 * PRMan-style DSO display driver interface.
 */
#ifndef NDSPY_H
#define NDSPY_H

#include <ri.h>

#define PkDspyNone          0
#define PkDspyFloat32       1
#define PkDspyUnsigned32    2
#define PkDspySinged32      3
#define PkDspyUnsigned16    4
#define PkDspySinged16      5
#define PkDspyUnsigned8     6
#define PkDspySinged8       7
#define PkDspyString        8
#define PkDspyMatrix        9

typedef float          PtDspyFloat32;
typedef unsigned int   PtDspyUnsigned32;
typedef int            PtDspySigned32;
typedef unsigned short PtDspyUnsigned16;
typedef short          PtDspySigned16;
typedef unsigned char  PtDspyUnsigned8;
typedef char           PtDspySigned8;
typedef char          *PtDspyString;
typedef float          PtDspyMatrix[4][4];

#define PkDspyByteOrderHiLo   0
#define	PkDspyByteOrderLoHi   1
#define	PkDspyByteOrderNative 2

typedef enum {
	PkDspyErrorNone = 0,
	PkDspyErrorNoMemory,
	PkDspyErrorUnsupported,
	PkDspyErrorBadParams,
	PkDspyErrorNoResource,
	PkDspyErrorUndefined
} PtDspyError;

typedef struct _UserParameter
{
	RtToken   name;
	char      vtype, vcount;
	RtPointer value;
	int       nbytes;
} UserParameter

typedef struct _PtDspyDevFormat
{
	char         *name;
	unsigned int  type;
} PtDspyDevFormat;

typedef struct _PtFlagStuff
{
	int flags;
} PtFlagStuff

#ifdef __cplusplus
extern "C" {
#endif

PtDspyError (*DspyImageOpenFunc)(PtDspyImageHandle * image,
                                 const char *drivername,
                                 const char *filename,
                                 int width,
                                 int height,
                                 int paramCount,
                                 const UserParameter *parameters,
                                 int formatCount,
                                 PtDspyDevFormat *format,
                                 PtFlagStuff *flagstuff);

#ifdef __cplusplus
}	/* extern "C" { */
#endif

#endif
