#include "render.h"
#include "context.h"
#include "backdoor.h"

void
ri_backdoor_world_begin_cb(void (*callback)(void))
{
	ri_render_get()->context->world_begin_cb = callback;
}

void
ri_backdoor_world_end_cb(void (*callback)(void))
{
	ri_render_get()->context->world_end_cb = callback;
}

void
ri_backdoor_render_end_cb(void (*callback)(void))
{
	ri_render_get()->context->render_end_cb = callback;
}

void
heavens_door()
{
	printf("Heavens Door!!!; You can't render any more...\n");
	exit(-1);
}
