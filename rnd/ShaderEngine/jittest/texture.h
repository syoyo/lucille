#ifndef TEXTURE_H
#define TEXTURE_H

#ifdef __cplusplus
extern "C" {
#endif

typedef struct _texture_t
{
    int            width;
    int            height;

    unsigned char *image;

    char          *name;

} texture_t;


texture_t *texture_load(const char *name);
void       texture_map (float *out, texture_t *texture, float u, float v);


#ifdef __cplusplus
}
#endif

#endif  /* TEXTURE_H */
