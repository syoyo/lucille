/*
 * $Id: sockdrv.c,v 1.4 2004/08/15 05:20:44 syoyo Exp $
 *
 * Socket(inter process communication) display driver.
 *
 * TODO:
 *  - specify the path to rockenfield by program argument.
 *  - specify the port number for rockenfield by program argument.
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#if !defined(WIN32)
#include <sys/socket.h>
#include <sys/types.h>
#include <netinet/in.h>
#include <arpa/inet.h>		// inet_addr()
#include <unistd.h>
#endif

#ifdef WIN32
#include <winsock2.h>		// use winsock2
WSADATA   wsa;
#endif

#include "memory.h"
#include "sockdrv.h"
#include "log.h"


#include "sockdrv_defs.h"

#ifdef WIN32
static SOCKET gfd = 0;
#else
static int gfd = 0;
#endif

static int gcount = 0;

typedef struct _pixpacket
{
	int   x;
	int   y;
	float col[4];
} pixpacket;

typedef struct _imageinfo
{
	int   width;
	int   height;
} imageinfo;

static pixpacket gpackets[MAXPACKETS];

int
spawn_process()
{
#if defined(WIN32)
	BOOL ret;
	STARTUPINFO si;
	PROCESS_INFORMATION pi;

	ZeroMemory(&si, sizeof(si));
	ZeroMemory(&pi, sizeof(pi));
	si.cb = sizeof(si);

	ret = CreateProcess(NULL, DEFAULT_APP,
			    NULL, NULL, FALSE, 0,
			    NULL, NULL, &si, &pi);
	if (ret == FALSE) {
		ri_log(LOG_ERROR, "Couldn't launch [ %s ] from path.",
			DEFAULT_APP);
		ri_log(LOG_ERROR, "Current path = %s", getenv("PATH"));
		exit(-1);
	} else {
		ri_log(LOG_INFO, "Lanched [ %s ].", DEFAULT_APP);
	}

	CloseHandle(pi.hProcess);
	CloseHandle(pi.hThread);

#elif defined(LINUX) || defined(__APPLE__)
	pid_t pid;

	if ((pid = fork()) == 0) {
		/* child */
		ri_log(LOG_INFO, "Launching %s ...", DEFAULT_APP);
		if (execlp(DEFAULT_APP, DEFAULT_APP, NULL) < 0) {
			ri_log(LOG_ERROR, "Could not find \"%s\" from path.", DEFAULT_APP);
			ri_log(LOG_ERROR, "Please try to add the path to \"%s\" into your environment variable PATH.", DEFAULT_APP);
			ri_log(LOG_ERROR, "Current PATH = %s.", getenv("PATH"));
			perror("eveclp");
			exit(-1);
		}
	} if (pid == -1) {
		/* err */
		return 0;
	} else {
		/* parent */
	}

	// wait a minute until forked process starts.
	//sleep(2);
#endif

	return 1;
}

int
sock_dd_open(const char *name, int width, int height,
	     int bits, RtToken component, const char *format)
{
	int                len;
	int                ret;
	int                comm;
	int	           success = 0;
	imageinfo          info;
	struct sockaddr_in addr;
	const int          nmaxtries = 50;
	int	           ntries = 0;

#ifdef WIN32
	ret = WSAStartup(MAKEWORD(2, 0), &wsa);
	if (ret != 0) {
		fprintf(stderr, "[error] WinSock2 initialization error.\n");
		fprintf(stderr, "[error] err code = %d.\n", ret);
	}
#endif

	gfd = socket(AF_INET, SOCK_STREAM, 0);
#ifdef WIN32
	if (gfd == INVALID_SOCKET) {
		printf("[error] invalid socket\n");
		fflush(stdout);
		return 0;
	}
#endif

	memset((void *)&addr, 0, sizeof(addr));
	addr.sin_family      = AF_INET;
	addr.sin_port        = htons(DEFAULT_PORT);
#ifdef WIN32
	addr.sin_addr.S_un.S_addr = inet_addr(LOCALADDR);
#else
	addr.sin_addr.s_addr = inet_addr(LOCALADDR);
#endif
 
	ret = connect(gfd, (struct sockaddr *)&addr, sizeof(addr));
	if (ret != 0) {
		// Assume that a server is not yet launched.
		if (!spawn_process()) return 0;

		for (ntries = 0; ntries < nmaxtries; ntries++) {
			ret = connect(gfd, (struct sockaddr *)&addr, sizeof(addr));
			if (ret == 0) {
				// connect successed
				success  = 1;
				break;
			}

			// Possibly a server is starting up...
			// wait some time, then re-try to connect.
#ifdef WIN32
			Sleep(500);
#else
			usleep(5000);
#endif

		}

		if (!success) {
#ifdef WIN32

			fprintf(stderr, "[error] connect() returns SOCKET_ERROR or connection is timeout.\n");
			fprintf(stderr, "[error] err code = %d\n", WSAGetLastError());
#else
			perror("connect");
#endif
			return 0;
		} 

	} else {
		// A server is alrealy running.
		printf("connect ok\n");
		fflush(stdout);
	}


	comm = COMMAND_NEW;
	len = sizeof(imageinfo);
	info.width  = width;
	info.height = height;

	printf("sending comm_new\n"); fflush(stdout);
	
	send(gfd, (char *)&comm, sizeof(int), 0);
	send(gfd, (char *)&len, sizeof(int), 0);
	send(gfd, (char *)&info, len, 0);

	(void)name;
	(void)bits;
	(void)component;
	(void)format;

	return 1;
}


int
sock_dd_write(int x, int y, const void *pixel)
{
	int  len;
	int  comm;

	if (gfd == 0) return 0;

	gpackets[gcount].x = x;
	gpackets[gcount].y = y;
	gpackets[gcount].col[0] = ((float *)pixel)[0];
	gpackets[gcount].col[1] = ((float *)pixel)[1];
	gpackets[gcount].col[2] = ((float *)pixel)[2];
	gpackets[gcount].col[3] = 1.0;

	gcount++;

	/* send MAXPACKETS pixels at once */
	if (gcount >= MAXPACKETS) {

		comm = COMMAND_PIXEL;
		len = sizeof(pixpacket) * MAXPACKETS;

	
		send(gfd, (char *)&comm, sizeof(int), 0);
		send(gfd, (char *)&len, sizeof(int), 0);
		send(gfd, (char *)&gpackets[0], len, 0);

		gcount = 0;

	}

	return 1;
}

int
sock_dd_close()
{
	int comm;

	comm = COMMAND_FINISH;
	send(gfd, (char *)&comm, sizeof(int), 0);


#ifdef WIN32
	closesocket(gfd);
#else
	close(gfd);
#endif

#ifdef WIN32
	WSACleanup();
#endif

	return 1;
}

int
sock_dd_progress()
{
	return 1;
}
