#include <stdio.h>
#include <stdlib.h>

#include "sis.h"

typedef struct _similarity_t
{
	int id;
	int sameas;
	int tag;
} similarity_t;

static int  get_root(int id, similarity_t *similaritys);
static int  merge(similarity_t *similaritys, int id1, int id2);


#if 0
int
main(int argc, char **argv)
{
	char buf[255];
	int i;
	int r, g, b, c, maxlabel;
	int w, h;
	int depth;
	int tick;
	int *levels;
	int *labels;

	fscanf(stdin, "%s", &buf);
	if (strcmp(buf, "P3") != 0) exit(-1);

	fscanf(stdin, "%d %d", &w, &h);
	fscanf(stdin, "%d", &depth);

	fprintf(stderr, "w x h = (%d, %d)\n", w, h);

	levels = (int *)malloc(sizeof(int) * w * h);
	labels = (int *)malloc(sizeof(int) * w * h);

	for (i = 0; i < w * h; i++) {
		fscanf(stdin, "%d %d %d ", &r, &g, &b);
		if (r == 0) levels[i] = 0;
		else        levels[i] = 1;
	}

	connected_components(labels, levels, w, h);

	fprintf(stdout, "P3\n");
	fprintf(stdout, "%d %d\n", w, h);
	fprintf(stdout, "255\n");

	maxlabel = 0;
	for (i = 0; i < w * h; i++) {
		if (maxlabel < labels[i]) maxlabel = labels[i];
	}


	fprintf(stderr, "maxlabel = %d\n", maxlabel);

	if (maxlabel == 0) exit(-1);

	for (i = 0; i < w * h; i++) {
		c = (int)((labels[i] / (double)maxlabel) * 255.0);
		if (c < 0) c = 0;
		if (c > 255) c = 255;
		fprintf(stdout, "%d %d %d\n", c, c, c);
	}
}
#endif

void
connected_components(pixelinfo_t *pixels, int width, int height)
{
	int i;
	int c, r;
	int pos, lastrowpos;
	int curr_label;
	int curr_tag = 0;
	int highest_label = 0;
	similarity_t *similaritys;

	similaritys = (similarity_t *)malloc(sizeof(similarity_t) *
					     width * height);

	for (i = 0; i < width * height; i++) {
		similaritys[i].id = 0;	
		similaritys[i].sameas = 0;	
		similaritys[i].tag = 0;	
	}

	/* first pixel(top-left). */
	if (pixels[0].valid) {
		similaritys[highest_label].id = highest_label;
		similaritys[highest_label].sameas = highest_label;
		pixels[0].label = highest_label; highest_label++;
	}

	/* label the first row. */
	for (c = 1; c < width; c++) {
		if (!pixels[c].valid) continue;

		if (pixels[c - 1].valid &&
		    pixels[c].level == pixels[c - 1].level) {
			pixels[c].label = pixels[c - 1].label;
		} else {
			similaritys[highest_label].id = highest_label;
			similaritys[highest_label].sameas = highest_label;
			pixels[c].label = highest_label; highest_label++;
			fprintf(stderr, "c = %d, label = %d\n", c, highest_label);
		}
	}

	/* label subsequent rows. */
	for (r = 1; r < height; r++) {
		/* label the first pixel on this row. */
		pos = r * width;
		lastrowpos = (r - 1) * width;

		if (pixels[pos].valid) { 

			if (pixels[lastrowpos].valid &&
			    pixels[pos].level == pixels[lastrowpos].level) {
				pixels[pos].level = pixels[lastrowpos].level;
			} else {
				similaritys[highest_label].id =
					highest_label;
				similaritys[highest_label].sameas =
					highest_label;
				pixels[pos].label = highest_label;
				highest_label++;

				fprintf(stderr, "r = %d, label = %d\n",
					r, highest_label);
			}
			
		}

		/* label subsequent pixels on this row. */
		for (c = 1; c < width; c++) {
			curr_label = -1;

			pos = r * width + c;
			lastrowpos = (r - 1) * width + c;

			if (!pixels[pos].valid) continue;

			/* check adjacent pixel. */
			if (pixels[pos - 1].valid &&
			    pixels[pos].level == pixels[pos - 1].level) {
				curr_label = pixels[pos - 1].label;
			}

			/* check above pixel. */
			if (pixels[lastrowpos].valid &&
			    pixels[pos].level == pixels[lastrowpos].level) {
				if (curr_label >= 0) {
					merge(similaritys,
					      curr_label,
					      pixels[lastrowpos].label);
				} else {
					curr_label = pixels[lastrowpos].label;
				}
			}

			if (curr_label >= 0) {
				pixels[pos].label = curr_label;
			} else {
				similaritys[highest_label].id = highest_label;
				similaritys[highest_label].sameas =
							highest_label;
				pixels[pos].label = highest_label;
				highest_label++;
				fprintf(stderr, "pos = %d, label = %d\n", pos, highest_label);
			}
		}

	}

	fprintf(stderr, "highest_label = %d\n", highest_label);

	/* relabeling */
	for (i = 0; i < highest_label; i++) {
		if (similaritys[i].sameas == i) {
			similaritys[i].tag = curr_tag; curr_tag++;
		}
	}

	for (i = 0; i < width * height; i++) {
		if (!pixels[i].valid) continue;
		pixels[i].label = 
			similaritys[get_root(pixels[i].label, similaritys)].tag;
	}

	/* changing label to start from 1. */
	for (i = 0; i < width * height; i++) {
		if (!pixels[i].valid) continue;
		pixels[i].label++; 
	}

	free(similaritys);
}

int
get_root(int id, similarity_t *similaritys)
{
	int where;

	while (similaritys[id].sameas != id) {
		/* link this node to its parent's parent. */
		where = similaritys[id].sameas;
		similaritys[id].sameas = similaritys[where].sameas;
		
		id = similaritys[id].sameas;
	}

	return id;
}

int
merge(similarity_t *similaritys, int id1, int id2)
{
	if (get_root(id1, similaritys) != get_root(id2, similaritys)) {
		similaritys[get_root(id1, similaritys)].sameas = 
			get_root(id2, similaritys);
		return 0;
	}

	return 1;
}

