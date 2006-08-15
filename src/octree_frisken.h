#ifndef OCTREE_FRISKEN_H
#define OCTREE_FRISKEN_H

//-------------------------------------------------------------------------------
//  Sample source code for the point location and region location methods 
//  and three example neighbor searches, from the paper: 
//   
//      Sarah F. Frisken and Ronald N. Perry.
//      Simple and Efficient Traversal Methods for Quadtrees and Octrees
//      Journal of Graphics Tools, 7(3):1-11, 2002
//   
//  Copyright 2002 Mitsubishi Electric Research Laboratories.  
//  All Rights Reserved.
//
//  Permission to use, copy, modify and distribute this software and its 
//  documentation for educational, research and non-profit purposes, without fee, 
//  and without a written agreement is hereby granted, provided that the above 
//  copyright notice and the following three paragraphs appear in all copies.
//
//  To request permission to incorporate this software into commercial products 
//  contact MERL - Mitsubishi Electric Research Laboratories, 201 Broadway, 
//  Cambridge, MA 02139.

//
//  IN NO EVENT SHALL MERL BE LIABLE TO ANY PARTY FOR DIRECT, INDIRECT, SPECIAL, 
//  INCIDENTAL, OR CONSEQUENTIAL DAMAGES, INCLUDING LOST PROFITS, ARISING OUT OF 
//  THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF MERL HAS BEEN ADVISED 
//  OF THE POSSIBILITY OF SUCH DAMAGES.
//
//  MERL SPECIFICALLY DISCLAIMS ANY WARRANTIES, INCLUDING, BUT NOT LIMITED TO, 
//  THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE. 
//  THE SOFTWARE PROVIDED HEREUNDER IS ON AN "AS IS" BASIS, AND MERL HAS NO 
//  OBLIGATIONS TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR 
//  MODIFICATIONS.

//-------------------------------------------------------------------------------

/*
 * The code was adapted and modified for octree by Syoyo Fujita
 * $Id: octree_frisken.h,v 1.2 2004/01/30 04:43:57 syoyo Exp $
 */

#ifdef __cplusplus
extern "C" {
#endif

//-------------------------------------------------------------------------------
//  Generic octree cell. Note that the locational codes and the cell level are 
//  only used in neighbor searching; they are not necessary for point or region 
//  location.
//-------------------------------------------------------------------------------

typedef struct _otCell {
    unsigned int    xLocCode;   // X locational code
    unsigned int    yLocCode;   // Y locational code
    unsigned int    zLocCode;   // Z locational code
    unsigned int    level;      // Cell level in hierarchy (smallest cell has level 0)
    struct _otCell  *parent;    // Pointer to parent cell
    struct _otCell  *children;  // Pointer to first of 8 contiguous child cells
    void            *data;      // Application specific cell data
}   otCell;

//-------------------------------------------------------------------------------
//  Maximum octree depth and related constants
//-------------------------------------------------------------------------------

#define OT_N_LEVELS   16        // Number of possible levels in the quadtree
#define OT_ROOT_LEVEL 15        // Level of root cell (OT_N_LEVELS - 1)
#define OT_MAX_VAL    32768.0f  // For converting positions to locational codes 
                                // (OT_MAX_VAL = 2^OT_ROOT_LEVEL)
#define OT_INV_MAX_VAL 1.0 / OT_MAX_VAL

//-------------------------------------------------------------------------------
//  Macro to traverse a octree from a specified cell (typically the root cell) 
//  to a leaf cell by following the x, y  and z locational codes, xLocCode, 
//  yLocCode  and zLocCode. 
//  Upon entering, cell is the specified cell and nextLevel is one less 
//  than the level of the specified cell. Upon termination, cell is the leaf cell
//  and nextLevel is one less than the level of the leaf cell. 

//-------------------------------------------------------------------------------
#define OT_TRAVERSE(cell,nextLevel,xLocCode,yLocCode, zLocCode)                   \
{                                                                                 \
    while ((cell)->children) {                                                    \
        unsigned int childBranchBit = 1 << (nextLevel);                           \
        unsigned int childIndex = ((((xLocCode) & childBranchBit) >> (nextLevel)) \
        + (((yLocCode) & childBranchBit) >> (--(nextLevel)))                      \
        + (((zLocCode) & childBranchBit) >> (nextLevel - 1)));                    \
        (cell) = &(((cell)->children)[childIndex]);                               \
    }                                                                             \
}


//-------------------------------------------------------------------------------
//  Macro to traverse a octree from a specified cell to an offspring cell by 
//  following the x, y and z locational codes, xLocCode, yLocCode and yLocCode.
//  The offpring cell is either at a specified level or is a leaf cell if a leaf
//  cell is  reached before the specified level. Upon entering, cell is the
//  specified cell and nextLevel is one less than the level of the specified cell.
//  Upon termination, cell is the offspring cell and nextLevel is one less than the 
//  level of the offspring cell.
//-------------------------------------------------------------------------------

#define OT_TRAVERSE_TO_LEVEL(cell,nextLevel,xLocCode,yLocCode,zLocCode,level)     \
{                                                                                 \
    unsigned int n = (nextLevel) - (level) + 1;                                   \
    while (n--) {                                                                 \
        unsigned int childBranchBit = 1 << (nextLevel);                           \
        unsigned int childIndex = ((((xLocCode) & childBranchBit) >> (nextLevel)) \
        + (((yLocCode) & childBranchBit) >> (--(nextLevel)))                      \
        + (((zLocCode) & childBranchBit) >> (nextLevel - 1)));                    \
        assert(childIndex < 8);                                                   \
        (cell) = &(((cell)->children)[childIndex]);                               \
        if (!(cell)->children) break;                                             \
    }                                                                             \
}

//-------------------------------------------------------------------------------
//  Macro for traversing a octree to a common ancestor of a specified cell 
//  and its neighbor, whose x, y or y locational code differs from the cell's
//  corresponding x, y or z locational code by binaryDiff (determined by XOR'ing
//  the appropriate pair of x, y or z locational codes). Upon entering, cell is the
//  specified cell and cellLevel is the cell's level. Upon termination, cell is 
//  the common ancestor and cellLevel is the common ancestor's level.
//-------------------------------------------------------------------------------

#define OT_GET_COMMON_ANCESTOR(cell,cellLevel,binaryDiff)                         \
{                                                                                 \
    while ((binaryDiff) & (1 << (cellLevel))) {                                   \
        (cell) = (cell)->parent;                                                  \
        (cellLevel)++;                                                            \
    }                                                                             \
}

extern otCell *otLocateCell (otCell *root, float p[3]);
//extern otCell *otLocateLeftNeighbor (otCell *cell);
//extern otCell *otLocateRightNeighbor (otCell *cell);

#ifdef __cplusplus
}	/* extern "C" */
#endif

#endif
