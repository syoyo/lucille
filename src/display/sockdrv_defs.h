#ifndef SOCKDRV_PROTOCOLS_H
#define SOCKDRV_PROTOCOLS_H

/* Defines shared between clients and servers. */

#define DEFAULT_PORT 12346 /* change as you like. */
#define MAXPACKETS 32*32
#define DEFAULT_PATH_WIN32 "c:\\cygwin\\home\\syoyo\\work\\lucille\\tools"
#define DEFAULT_APP "rockenfield"
#define LOCALADDR "127.0.0.1"

/* Commands sent to the server */
#define COMMAND_NEW      0
#define COMMAND_FINISH   1
#define COMMAND_PIXEL    2

/* Commands sent from the server */
#define COMMAND_CANCEL   10	// Cancel(e.g. The window is closed.)


#endif

