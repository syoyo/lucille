/*
 * $Id: loader_obj.c,v 1.4 2004/02/12 05:17:33 syoyo Exp $
 *
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "loader_obj.h"
#include "vector.h"
#include "memory.h"
#include "material.h"
#include "render.h"

static void calc_mesh_size(FILE *fp, int *nvert, int *nnorm, int *nface);
//static ri_geom_t *read_obj(FILE *fp, int nvert, int nnorm, int nface);
static void read_obj(ri_render_t *render, FILE *fp, int nvert, int nnorm, int nface);
static void goto_eol(FILE *fp);
static int has_vn, has_vt;
static void calc_vertex_normal(ri_vector_t   *normals,	/* output */
			       ri_vector_t   *vertices,
		               int nvertices,	
			        unsigned int *indices,
		               int nindices);
static double calc_area(const ri_vector_t *v0, const ri_vector_t *v1, const ri_vector_t *v2);
static void   scan_triple(FILE *fp, int *c0, int *c1, int *c2);
static ri_geom_t *build_geom(float *vlist, int nv, float *vnlist, int nn,
	                     float *vtlist, int nt,
			     unsigned long *flist, unsigned long nf);

/* Function: load_obj
 *
 *     Loads .obj file and adds it to render's geometry list.
 *
 * Parameters:
 * 
 *     *render   - The renderer geometry stores.
 *      filename - .obj file name.
 *      debug    - debug option(1 on, 0 off)
 */
void
load_obj(ri_render_t *render, const char *filename, int debug)
{
	FILE *fp;
	//ri_geom_t *geom;
	int nverts, nnorms, nfaces;

	(void)debug;
#if 0
	char *ext;
	char  buf[1024];

	ext = strrchr(filename, '.');
	if (ext != NULL && (strcasecmp(ext, ".gz") == 0)) {
		sprintf(buf, "gunzip -c %s", filename);
		fp = popen(buf, "r");
	} else {
		fp = fopen(filename, "r");
	}
#else
	fp = fopen(filename, "r");
#endif
	if (!fp) {
		printf("can't open file [ %s ]\n", filename);
		exit(-1);
	}

	has_vn = 0;
	has_vt = 0;
	nverts = 0;
	nnorms = 0;
	nfaces = 0;

	read_obj(render, fp, nverts, nnorms, nfaces);


	fclose(fp);

}

/* --- private functions --- */

static void
calc_mesh_size(FILE *fp, int *nvert, int *nnorm, int *nface)
{
	char token[255];

	fscanf(fp, "%s", token);
	while(!feof(fp)) {
		if (strcmp(token, "v") == 0) {
			(*nvert)++;
			goto_eol(fp);
		} else if (strcmp(token, "f") == 0) {
			(*nface)++;
			goto_eol(fp);
		} else if (strcmp(token, "vn") == 0) {
			(*nnorm)++;
			has_vn = 1;
			goto_eol(fp);
		} else if (strcmp(token, "vt") == 0) {
			has_vt = 1;
			goto_eol(fp);
		} else {
			goto_eol(fp);
		}

		fscanf(fp, "%s", token);
	}
}

static void
read_obj(ri_render_t *render, FILE *fp, int nvert, int nnorm, int nface)
{
	const int defbufsize = 1024;

	char token[255];
	int   nv = 0;
	int   nf = 0;
	int   nt = 0;
	int   nn = 0;
	//int   i;
	int   v0, v1, v2;
	int   nv0, nv1, nv2;
	int   tv0, tv1, tv2;
	//int   tmp0, tmp1, tmp2;
	float x, y, z;
	float bmin[3], bmax[3];
	ri_geom_t *geom;
	float scale;

	unsigned long allocednv = defbufsize;
	unsigned long allocednn = defbufsize;
	unsigned long allocednt = defbufsize;
	unsigned long allocednf = defbufsize;
	char  gname[256];
	unsigned long voffset = 0;
	unsigned long vnoffset = 0;
	unsigned long vtoffset = 0;
	unsigned long foffset = 0;
	float *vlist  = NULL;
	float *vnlist = NULL;
	float *vtlist = NULL;
	float *tmpnlist = NULL;
	unsigned long *flist = NULL;
	int firstv = 1;
	int gflag = 0;
	int nogcommand = 1;

	(void)nnorm;

	nv = 0;
	nf = 0;
	nn = 0;

	bmin[0] = 1.0e+6;
	bmin[1] = 1.0e+6;
	bmin[2] = 1.0e+6;
	bmax[0] = -1.0e+6;
	bmax[1] = -1.0e+6;
	bmax[2] = -1.0e+6;

	scale = render->context->option->prt_scale;
	printf("scale = %f\n", scale);
	
	fscanf(fp, "%s", token);
	while(!feof(fp)) {
		
		if (strcmp(token, "v") == 0) {
			if (firstv) {
				if (gflag) {
					geom = build_geom(
						   vlist, nv - voffset,
						   vnlist, nn - vnoffset,
						   vtlist, nt - vtoffset,
						   flist, nf - foffset);

					ri_render_add_geom(render, geom);

					gflag = 0;	
				}

				if (vlist) free(vlist);
				if (vnlist) free(vnlist);
				if (vtlist) free(vtlist);
				if (tmpnlist) free(tmpnlist);
				if (flist) free(flist);

				vlist = (float *)malloc(sizeof(float) *
							defbufsize * 3);
				vnlist = (float *)malloc(sizeof(float) *
							defbufsize * 3);
				tmpnlist = (float *)malloc(sizeof(float) *
							defbufsize * 3);
				vtlist = (float *)malloc(sizeof(float) *
							defbufsize * 2);
				flist = (unsigned long *)malloc(
						sizeof(unsigned long) *
						defbufsize * 3);
				
				firstv = 0;
				has_vt = 0;
				has_vn = 0;

				voffset = nv;
				vnoffset = nn;
				vtoffset = nt;
				foffset = nf;

				allocednv = defbufsize;
				allocednn = defbufsize;
				allocednt = defbufsize;
				allocednf = defbufsize;
			}

			if ((nv - voffset) >= allocednv) {
				if ((nv - voffset) > 10000000) {
					allocednv = (int)(allocednv * 1.5);
					vnlist= realloc(vnlist,
						sizeof(float) * allocednv * 3);
					vlist = realloc(vlist,
						sizeof(float) * allocednv * 3);
				} else {
					allocednv = allocednv * 2;
					vnlist = realloc(vnlist,
						sizeof(float) * allocednv * 3);
					vlist = realloc(vlist,
						sizeof(float) * allocednv * 3);
				}
			}

			fscanf(fp, "%f %f %f", &x, &y, &z);
			
			x =  x * scale;
			y =  y * scale;
			z =  z * scale;

			vlist[(nv - voffset) * 3 + 0] = x;
			vlist[(nv - voffset) * 3 + 1] = y;
			vlist[(nv - voffset) * 3 + 2] = z;

			if (bmin[0] > x) bmin[0] = x;
			if (bmin[1] > y) bmin[1] = y;
			if (bmin[2] > z) bmin[2] = z;

			if (bmax[0] < x) bmax[0] = x;
			if (bmax[1] < y) bmax[1] = y;
			if (bmax[2] < z) bmax[2] = z;
		
			nv++;
			if (nv % 10000 == 0) {
				printf("read %d of %d vertices\n", nv, nvert);
			}

			goto_eol(fp);
		//} else if (strcmp(token, "vn") == 0 && has_vn) {
		} else if (strcmp(token, "vn") == 0) {
			has_vn = 1;

			if ((nn - vnoffset) >= allocednn) {
				if ((nn -vnoffset) > 10000000) {
					allocednn = (int)(allocednn * 1.5);
					tmpnlist = realloc(tmpnlist,
						sizeof(float) * allocednn * 3);
				} else {
					allocednn = allocednn * 2;
					tmpnlist = realloc(tmpnlist,
						sizeof(float) * allocednn * 3);
				}

			}

			fscanf(fp, "%f %f %f", &x, &y, &z);
#if 0
			tmpnormals[nn].e[0] = x;
			tmpnormals[nn].e[1] = y;
			tmpnormals[nn].e[2] = z;
			tmpnormals[nn].e[3] = 1.0;
#endif
			tmpnlist[(nn - vnoffset) * 3 + 0] = x;
			tmpnlist[(nn - vnoffset) * 3 + 1] = y;
			tmpnlist[(nn - vnoffset) * 3 + 2] = z;
			nn++;
			goto_eol(fp);

		} else if (strcmp(token, "vt") == 0) {
			has_vt = 1;
		} else if (strcmp(token, "f") == 0) {
			if ((nf - foffset) >= allocednf) {
				if ((nf - foffset) > 10000000) {
					allocednf = (unsigned long)
							(allocednf * 1.5);
					flist = realloc(flist,
						sizeof(unsigned long) * 
						allocednf * 3);
				} else {
					allocednf = allocednf * 2;
					flist = realloc(flist,
						sizeof(unsigned long) * 
						allocednf * 3);
				}

			}

			if (has_vt) {
				scan_triple(fp, &v0, &tv0, &nv0);
				scan_triple(fp, &v1, &tv1, &nv1);
				scan_triple(fp, &v2, &tv2, &nv2);
			} else if (has_vn) {
				fscanf(fp, "%d//%d %d//%d %d//%d",
						&v0, &nv0,
						&v1, &nv1,
						&v2, &nv2);
			} else {
				fscanf(fp, "%d %d %d",
						&v0,
						&v1,
						&v2);
			}

			v0--; v1--; v2--;
#if 0
			if (v0 < 0 || v0 >= nvert) {
				printf("??? v0 = %d, nvert = %d\n", v0, nvert);
				exit(-1);
			}  
			if (v1 < 0 || v1 >= nvert) {
				printf("??? v1 = %d, nvert = %d\n", v1, nvert);
				exit(-1);
			}  
			if (v2 < 0 || v2 >= nvert) {
				printf("??? v2 = %d, nvert = %d\n", v2, nvert);
				exit(-1);
			}  
			if (has_vn) {
				nv0--; nv1--; nv2--;
			}
#endif
			if (v0 < 0) {
				printf("??? v0 = %d\n", v0);
				exit(-1);
			}  
			if (v1 < 0) {
				printf("??? v1 = %d\n", v1);
				exit(-1);
			}  
			if (v2 < 0) {
				printf("??? v2 = %d\n", v2);
				printf("  v0 = %d\n", v0);
				printf("  v1 = %d\n", v1);
				printf("  v2 = %d\n", v2);
				exit(-1);
			}  
			if (has_vn) {
				nv0--; nv1--; nv2--;
			}

#if 0
			indices[3 * nf + 0] = v0;
			indices[3 * nf + 1] = v1;
			indices[3 * nf + 2] = v2;
#endif

			v0 -= voffset;
			v1 -= voffset;
			v2 -= voffset;

			flist[(nf - foffset) * 3 + 0] = v0;
			flist[(nf - foffset) * 3 + 1] = v1;
			flist[(nf - foffset) * 3 + 2] = v2;

			// hack
			if (has_vn) {
#if 0
				ri_vector_copy(&(normals[v0]),
						tmpnormals[nv0]); 
				ri_vector_copy(&(normals[v1]),
						tmpnormals[nv1]); 
				ri_vector_copy(&(normals[v2]),
						tmpnormals[nv2]); 
#endif
				nv0 -= vnoffset;
				nv1 -= vnoffset;
				nv2 -= vnoffset;

#ifdef DEBUG
				printf("nv012 = %d, %d, %d\n", nv0, nv1, nv2);
				printf("v012 = %d, %d, %d\n", v0, v1, v2);
#endif
				vnlist[3 * v0 + 0] = tmpnlist[3 * nv0 + 0];
				vnlist[3 * v0 + 1] = tmpnlist[3 * nv0 + 1];
				vnlist[3 * v0 + 2] = tmpnlist[3 * nv0 + 2];
				vnlist[3 * v1 + 0] = tmpnlist[3 * nv1 + 0];
				vnlist[3 * v1 + 1] = tmpnlist[3 * nv1 + 1];
				vnlist[3 * v1 + 2] = tmpnlist[3 * nv1 + 2];
				vnlist[3 * v2 + 0] = tmpnlist[3 * nv2 + 0];
				vnlist[3 * v2 + 1] = tmpnlist[3 * nv2 + 1];
				vnlist[3 * v2 + 2] = tmpnlist[3 * nv2 + 2];
			}

			nf++;
			if (nf % 10000 == 0) {
				printf("read %d of %d indices\n", nf, nface);
			}
			goto_eol(fp);
		} else if (strcmp(token, "g") == 0) {
			gflag = 1;
			nogcommand = 0;
			firstv = 1;
		
			fscanf(fp, "%s", gname);
			printf("gname = %s\n", gname);

			if (fgetc(fp) != '\n') {
				fscanf(fp, "%s", gname);
				printf("second gname = %s\n", gname);
			}

			goto_eol(fp);

		} else {
			goto_eol(fp);
		}

		fscanf(fp, "%s", token);
	}

	if (gflag) {	/* this is the last group */
#if DEBUG
		printf("nn = %d, vnoffset = %lu\n", nn, vnoffset);
#endif
		geom = build_geom(
			   vlist, nv - voffset,
			   vnlist, nn - vnoffset,
			   vtlist, nt - vtoffset,
			   flist, nf - foffset);

		ri_render_add_geom(render, geom);
	}

	if (nogcommand) {	/* no 'g' command in the .obj */
		printf("nv = %d, nf = %d\n", nv, nf);
		geom = build_geom(
			   vlist, nv,
			   vnlist, nn,
			   vtlist, nt,
			   flist, nf);
		ri_render_add_geom(render, geom);
	}

	if (vlist)  free(vlist);
	if (vnlist) free(vnlist);
	if (tmpnlist) free(tmpnlist);
	if (vtlist) free(vtlist);
	if (flist)  free(flist);

	printf("** nv = %d, nn = %d, nf = %d\n", nv, nn, nf);
	printf("** obj bmin = (%f, %f, %f)\n", bmin[0], bmin[1], bmin[2]);
	printf("** obj bmax = (%f, %f, %f)\n", bmax[0], bmax[1], bmax[2]);

#if 0
	if (!has_vn) {
		calc_vertex_normal(normals,
				   positions, nvert, indices, nface * 3);
		ri_geom_add_normals(geom, nvert, normals);
	}

	ri_geom_add_positions(geom, nvert, positions);
	if (has_vn) {
		ri_geom_add_normals(geom, nnorm, normals);
	}
	//ri_geom_add_colors(geom, nvert, colors);
	ri_geom_add_indices(geom, nface * 3, indices);

	free(positions);
	free(normals);
	free(indices);
#endif

	return;
}

static void
goto_eol(FILE *fp)
{
	char c;
	while(((c = (char)fgetc(fp)) != '\n') && !feof(fp));
}

static void
calc_vertex_normal(ri_vector_t  *normals,
		   ri_vector_t  *vertices,
		   int nvertices,
		   unsigned int *indices,
		   int nindices)
{
	int     i;
	unsigned int i0, i1, i2;  
	ri_vector_t *v0, *v1, *v2;
	ri_vector_t v01, v02;
	ri_vector_t normal;
	double  area;

	fprintf(stderr, "calc_vertex_normal\n");

	/* calculate vertex normal from polygon mesh.
	 * a vertex normal is an aaverage of the face vectors with
	 * area weighting.
 	 */	

	for (i = 0; i < nvertices; i++) {
		ri_vector_zero(&(normals[i]));
	}

	for (i = 0; i < nindices / 3; i++) {
		i0 = indices[3 * i + 2];
		i1 = indices[3 * i + 1];
		i2 = indices[3 * i + 0];

		v0 = &(vertices[i0]);
		v1 = &(vertices[i1]);
		v2 = &(vertices[i2]);

		ri_vector_sub(&v01, v1, v0);
		ri_vector_sub(&v02, v2, v0);
		ri_vector_cross3(&normal, &v01, &v02); 
		ri_vector_normalize(&normal);

		//area = calc_area(v0, v1, v2);
		area = 1.0;
			

		/* weight by the area of the face. */
		ri_vector_scale(&normal, (float)area);
		ri_vector_add(&(normals[i0]), &normals[i0], &normal);
		ri_vector_add(&(normals[i1]), &normals[i1], &normal);
		ri_vector_add(&(normals[i2]), &normals[i2], &normal);
	}

	for (i = 0; i < nvertices; i++) {
		ri_vector_normalize(&(normals[i]));
	}
}

static double
calc_area(const ri_vector_t *v0, const ri_vector_t *v1, const ri_vector_t *v2)
{
	ri_vector_t v01, v02;
	ri_vector_t cross;
	
	ri_vector_sub(&v01, v1, v0);
	ri_vector_sub(&v02, v2, v0);
	ri_vector_cross3(&cross, &v01, &v02);

	return (ri_vector_length(&cross) * 0.5); 
}

static void
scan_triple(FILE *fp, int *c0, int *c1, int *c2)
{
        int delim;

        fscanf(fp, "%d/", c0);
        delim = getc(fp);
        if (delim == '/') {
                *c1 = -1;
                fscanf(fp, "%d", c2);
        } else {
                ungetc(delim, fp);
                fscanf(fp, "%d/%d", c1, c2);
        }
}

static ri_geom_t *
build_geom(float *vlist, int nv, float *vnlist, int nn,
	   float *vtlist, int nt, unsigned long *flist, unsigned long nf)
{
	ri_geom_t     *geom;
	ri_vector_t   *positions = NULL;
	ri_vector_t   *normals   = NULL;
	unsigned int  *indices  = NULL;
	unsigned long  i;
	
	geom = ri_geom_new();

	positions = (ri_vector_t *)ri_mem_alloc(sizeof(ri_vector_t) *
					  nv);
	normals = (ri_vector_t *)ri_mem_alloc(sizeof(ri_vector_t) *
					  nv);

	indices = (unsigned int *)ri_mem_alloc(sizeof(unsigned int) *
					      nf * 3);

	for (i = 0; i < (unsigned long)nv; i++) {
#if 0
#ifdef DEBUG
		printf("v[%lu] = %f, %f, %f\n",
			i,
			vlist[3 * i + 0],	
			vlist[3 * i + 1],	
			vlist[3 * i + 2]);
#endif
#endif
		positions[i].e[0] = vlist[3 * i + 0];
		positions[i].e[1] = vlist[3 * i + 1];
		positions[i].e[2] = vlist[3 * i + 2];
		positions[i].e[3] = 1.0;
	}



	for (i = 0; i < (unsigned long)nf; i++) {
		indices[3 * i + 0] = flist[3 * i + 0];
		indices[3 * i + 1] = flist[3 * i + 1];
		indices[3 * i + 2] = flist[3 * i + 2];

	}

	if (!nn) {
		calc_vertex_normal(normals,
				   positions, nv, indices, nf * 3);
		nn = nv;
	} else {

		for (i = 0; i < (unsigned long)nn; i++) {
			normals[i].e[0] = vnlist[3 * i + 0];
			normals[i].e[1] = vnlist[3 * i + 1];
			normals[i].e[2] = vnlist[3 * i + 2];
			normals[i].e[3] = 0.0;
		}
	}


	ri_geom_add_positions(geom, nv, positions);
	ri_geom_add_normals(geom, nn, normals);
	ri_geom_add_indices(geom, nf * 3, indices);

	geom->shader = NULL;
	geom->material = ri_material_new();
	geom->two_side = 0;

	ri_mem_free(positions);
	ri_mem_free(normals);
	ri_mem_free(indices);

	return geom;
}

