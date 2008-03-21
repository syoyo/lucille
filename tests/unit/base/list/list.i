%module base_list
%{
#include "list.h"
%}

typedef struct _ri_list_t
{
	void              *data;
	struct _ri_list_t *next;
	struct _ri_list_t *prev;
} ri_list_t;

extern ri_list_t *ri_list_new();
