#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "texture.h"

unsigned char *
ppm_load(
    int             *out_width,
    int             *out_height,
    const char      *name)
{

    FILE *fp;
    int   w, h;
    int   d;
    unsigned char *img;

    char buf[2048];

    fp = fopen(name, "r");
    if (!fp) {
        printf("can't open file %s\n", name);
        exit(1);
    }

    fscanf(fp, "%s\n", buf);
    if (buf[0] != 'P' && buf[1] != '6') {
        printf("Not a ppm file\n");
        exit(1);
    }
    
    // skip comment
    fgets(buf, 1024, fp);
    printf(buf);

    fscanf(fp, "%d %d\n", &w, &h);
    fscanf(fp, "%d\n", &d);

    if (d != 255) {
        printf("Maxval must be 255\n");
        exit(1);
    }

    img = (unsigned char *)malloc(w * h * 3);

    fread(img, w * h * 3, 1, fp);

    fclose(fp);

    (*out_width)  = w;
    (*out_height) = h;

    return img;
}


texture_t *
texture_load(const char *name)
{
    texture_t *tex;

    tex = (texture_t *)malloc(sizeof(texture_t));

    tex->image = ppm_load(&tex->width, &tex->height, name);

    tex->name = strdup(name);

    printf("Texture loaded [ %s ] %d x %d\n", name, tex->width, tex->height);

    return tex;

}

void
texture_map(float *out, texture_t *texture, float u, float v)
{

    int   i;
    float px, py;
    float dx, dy;
    int   pix, piy;
    int   xi[4], yi[4];
    float xf[4], yf[4];
    float w[4];
    float texel[4][4];
    float inv = 1.0f / 255.0f;
    float col[4];

    if (u < 0.0f) u = 0.0f;
    if (v < 0.0f) v = 0.0f;
    if (u > 1.0f) u = 1.0f;
    if (v > 1.0f) v = 1.0f;

    px = u * texture->width;
    py = v * texture->height;

    pix = (int)px;
    piy = (int)py;

    dx  = px - pix;
    dy  = py - piy;

    //
    // 0 --- 2
    // |     |
    // 1 --- 3
    //
    w[0] = (1.0 - dx) * (1.0 - dy);
    w[1] = (1.0 - dx) *        dy ;
    w[2] =        dx  * (1.0 - dy);
    w[3] =        dx  *        dy ;

    xi[0] = px;     if (xi[0] >= texture->width) xi[0] = texture->width - 1;
    xi[1] = px;     if (xi[1] >= texture->width) xi[1] = texture->width - 1;
    xi[2] = px + 1; if (xi[2] >= texture->width) xi[2] = texture->width - 1;
    xi[3] = px + 1; if (xi[3] >= texture->width) xi[3] = texture->width - 1;

    yi[0] = py;     if (yi[0] >= texture->height) yi[0] = texture->height - 1;
    yi[1] = py + 1; if (yi[1] >= texture->height) yi[1] = texture->height - 1;
    yi[2] = py;     if (yi[2] >= texture->height) yi[2] = texture->height - 1;
    yi[3] = py + 1; if (yi[3] >= texture->height) yi[3] = texture->height - 1;

    for (i = 0; i < 4; i++) {
        texel[i][0] = (float)texture->image[3 * (yi[i] * texture->width + xi[i]) + 0] * inv;
        texel[i][1] = (float)texture->image[3 * (yi[i] * texture->width + xi[i]) + 1] * inv;
        texel[i][2] = (float)texture->image[3 * (yi[i] * texture->width + xi[i]) + 2] * inv;
    }

    for (i = 0; i < 4; i++) {
        col[i] = w[0] * texel[0][i] 
               + w[1] * texel[1][i] 
               + w[2] * texel[2][i] 
               + w[3] * texel[3][i]; 
    }

    out[0] = col[0];
    out[1] = col[1];
    out[2] = col[2];
    out[3] = col[3];
}
