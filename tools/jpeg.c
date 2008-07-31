/****************************************************************************
 *
 * Follows is code extracted from the simage library.  Original Authors:
 *
 *      Systems in Motion,
 *      <URL:http://www.sim.no>
 *
 *      Peder Blekken <pederb@sim.no>
 *      Morten Eriksen <mortene@sim.no>
 *      Marius Bugge Monsen <mariusbu@sim.no>
 *
 * The original COPYING notice
 *
 *      All files in this library are public domain, except simage_rgb.cpp which is
 *      Copyright (c) Mark J Kilgard <mjk@nvidia.com>. I will contact Mark
 *      very soon to hear if this source also can become public domain.
 *
 *      Please send patches for bugs and new features to: <pederb@sim.no>.
 *
 *      Peder Blekken
 *
 *
 **********************************************************************/

/*
 * Based on example code found in the libjpeg archive
 *
 */

#include <stdio.h>
#include <jpeglib.h>

#include <setjmp.h>
#include <string.h>
#include <assert.h>
#include <stdlib.h>

#define ERR_NO_ERROR 0
#define ERR_OPEN     1
#define ERR_MEM      2
#define ERR_JPEGLIB  3

static int jpegerror = ERR_NO_ERROR;

int
simage_jpeg_error(char * buffer, int buflen)
{
    switch (jpegerror)
    {
        case ERR_OPEN:
            strncpy(buffer, "JPEG loader: Error opening file", buflen);
            break;
        case ERR_MEM:
            strncpy(buffer, "JPEG loader: Out of memory error", buflen);
            break;
        case ERR_JPEGLIB:
            strncpy(buffer, "JPEG loader: Illegal jpeg file", buflen);
            break;
    }
    return jpegerror;
}


struct my_error_mgr
{
    struct jpeg_error_mgr pub;   /* "public" fields */

    jmp_buf setjmp_buffer;       /* for return to caller */
};

typedef struct my_error_mgr * my_error_ptr;

static void
my_error_exit (j_common_ptr cinfo)
{
    /* cinfo->err really points to a my_error_mgr struct, so coerce pointer */
    my_error_ptr myerr = (my_error_ptr) cinfo->err;

    /* Always display the message. */
    /* We could postpone this until after returning, if we chose. */
    /*(*cinfo->err->output_message) (cinfo);*/

    /* FIXME: get error messahe from jpeglib */

    /* Return control to the setjmp point */
    longjmp(myerr->setjmp_buffer, 1);
}

#if 0
int
simage_jpeg_identify(const char *,
const unsigned char *header,
int headerlen)
{
    static unsigned char jpgcmp[] = {'J', 'F', 'I', 'F' };
    if (headerlen < 4) return 0;
    if (memcmp((const void*)&header[6],
        (const void*)jpgcmp, 4) == 0) return 1;
    return 0;
}

#endif

static unsigned char*
copyScanline(unsigned char *currPtr, unsigned char *from, int cnt)
{
    memcpy((void*)currPtr, (void*)from, cnt);
    currPtr -= cnt;
    return currPtr;
}


unsigned char *
jpeg_load(const char *filename,
int *width,
int *height,
int *compos)
{
    int w;
    int h;
    unsigned char *currPtr;
    int format;
    unsigned char *buffer;
    /* This struct contains the JPEG decompression parameters and pointers to
     * working space (which is allocated as needed by the JPEG library).
     */
    struct jpeg_decompress_struct cinfo;
    /* We use our private extension JPEG error handler.
     * Note that this struct must live as long as the main JPEG parameter
     * struct, to avoid dangling-pointer problems.
     */
    struct my_error_mgr jerr;
    /* More stuff */
    FILE * infile;               /* source file */
    JSAMPARRAY rowbuffer;        /* Output row buffer */
    int row_stride;              /* physical row width in output buffer */

    jpegerror = ERR_NO_ERROR;

    /* In this example we want to open the input file before doing anything else,
     * so that the setjmp() error recovery below can assume the file is open.
     * VERY IMPORTANT: use "b" option to fopen() if you are on a machine that
     * requires it in order to read binary files.
     */

    if ((infile = fopen(filename, "rb")) == NULL)
    {
        printf("file [ %s ]not found\n", filename);
        jpegerror = ERR_OPEN;
        return NULL;
    }

    /* Step 1: allocate and initialize JPEG decompression object */

    buffer = NULL;

    /* We set up the normal JPEG error routines, then override error_exit. */
    cinfo.err = jpeg_std_error(&jerr.pub);
    jerr.pub.error_exit = my_error_exit;
    /* Establish the setjmp return context for my_error_exit to use. */
    if (setjmp(jerr.setjmp_buffer))
    {
        /* If we get here, the JPEG code has signaled an error.
         * We need to clean up the JPEG object, close the input file, and return.
         */
        jpegerror = ERR_JPEGLIB;
        jpeg_destroy_decompress(&cinfo);
        fclose(infile);
        if (buffer) free(buffer);
        return NULL;
    }
    /* Now we can initialize the JPEG decompression object. */
    jpeg_create_decompress(&cinfo);

    /* Step 2: specify data source (eg, a file) */

    jpeg_stdio_src(&cinfo, infile);

    /* Step 3: read file parameters with jpeg_read_header() */

    (void) jpeg_read_header(&cinfo, TRUE);
    /* We can ignore the return value from jpeg_read_header since
     *   (a) suspension is not possible with the stdio data source, and
     *   (b) we passed TRUE to reject a tables-only JPEG file as an error.
     * See libjpeg.doc for more info.
     */

    /* Step 4: set parameters for decompression */
    /* In this example, we don't need to change any of the defaults set by
     * jpeg_read_header(), so we do nothing here.
     */

    /* Step 5: Start decompressor */
    if (cinfo.jpeg_color_space == JCS_GRAYSCALE)
    {
        format = 1;
        cinfo.out_color_space = JCS_GRAYSCALE;
    }
    else                         /* use rgb */
    {
        format = 3;
        cinfo.out_color_space = JCS_RGB;
    }

    (void) jpeg_start_decompress(&cinfo);
    /* We can ignore the return value since suspension is not possible
     * with the stdio data source.
     */

    /* We may need to do some setup of our own at this point before reading
     * the data.  After jpeg_start_decompress() we have the correct scaled
     * output image dimensions available, as well as the output colormap
     * if we asked for color quantization.
     * In this example, we need to make an output work buffer of the right size.
     */
    /* JSAMPLEs per row in output buffer */
    row_stride = cinfo.output_width * cinfo.output_components;
    /* Make a one-row-high sample array that will go away when done with image */
    rowbuffer = (*cinfo.mem->alloc_sarray)
        ((j_common_ptr) &cinfo, JPOOL_IMAGE, row_stride, 1);
    w = cinfo.output_width;
    h = cinfo.output_height;
    buffer = currPtr = (unsigned char*)
        malloc(w*h*cinfo.output_components);

    /* Step 6: while (scan lines remain to be read) */
    /*           jpeg_read_scanlines(...); */

    /* Here we use the library's state variable cinfo.output_scanline as the
     * loop counter, so that we don't have to keep track ourselves.
     */

    /* flip image upside down */
    if (buffer)
    {
        currPtr = buffer + row_stride * (cinfo.output_height-1);

        while (cinfo.output_scanline < cinfo.output_height)
        {
            /* jpeg_read_scanlines expects an array of pointers to scanlines.
             * Here the array is only one element long, but you could ask for
             * more than one scanline at a time if that's more convenient.
             */
            (void) jpeg_read_scanlines(&cinfo, rowbuffer, 1);
            /* Assume put_scanline_someplace wants a pointer and sample count. */
            currPtr = copyScanline(currPtr, rowbuffer[0], row_stride);
        }
    }
    /* Step 7: Finish decompression */

    (void) jpeg_finish_decompress(&cinfo);
    /* We can ignore the return value since suspension is not possible
     * with the stdio data source.
     */

    /* Step 8: Release JPEG decompression object */

    /* This is an important step since it will release a good deal of memory. */
    jpeg_destroy_decompress(&cinfo);

    /* After finish_decompress, we can close the input file.
     * Here we postpone it until after no more JPEG errors are possible,
     * so as to simplify the setjmp error logic above.  (Actually, I don't
     * think that jpeg_destroy can do an error exit, but why assume anything...)
     */
    fclose(infile);

    /* At this point you may want to check to see whether any corrupt-data
     * warnings occurred (test whether jerr.pub.num_warnings is nonzero).
     */

    /* And we're done! */
    if (buffer)
    {
        *width = w;
        *height = h;
        *compos = format;
    }
    else
    {
        jpegerror = ERR_MEM;
    }
    return buffer;
}
