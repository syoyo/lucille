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
 * $Id: octree_frisken.c,v 1.1.1.1 2004/01/06 13:57:09 syoyo Exp $
 */

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#include "octree_frisken.h"

//-------------------------------------------------------------------------------
//  Locate the leaf cell containing the specified point p, where p lies in 
//  [0,1)x[0,1).
//-------------------------------------------------------------------------------

otCell *otLocateCell (otCell *root, float p[3])
{
    //----Determine the x, y and z locational codes of the point's position. Refer 
    //----to [King2001] for more efficient methods for converting floating point 
    //----numbers to integers.

    unsigned int xLocCode = (unsigned int) (p[0] * OT_MAX_VAL); 
    unsigned int yLocCode = (unsigned int) (p[1] * OT_MAX_VAL); 
    unsigned int zLocCode = (unsigned int) (p[2] * OT_MAX_VAL); 


    //----Follow the branching patterns of the locational codes from the root cell
    //----to locate the leaf cell containing p

    otCell *cell = root;
    unsigned int nextLevel = OT_ROOT_LEVEL - 1;
    OT_TRAVERSE(cell,nextLevel,xLocCode,yLocCode,zLocCode);
    return(cell);
}

#if 0

//-------------------------------------------------------------------------------
//  Locate the smallest cell that entirely contains a rectangular region defined 
//  by its bottom-left vertex v0 and its top-right vertex v1, where v0 and v1 
//  lie in [0,1)x[0,1).
//-------------------------------------------------------------------------------

otCell *qtLocateRegion (otCell *root, float v0[2], float v1[2])
{
    //----Determine the x and y locational codes of the region boundaries. Refer 
    //----to [King2001] for more efficient methods for converting floating point 
    //----numbers to integers.

    unsigned int x0LocCode = (unsigned int) (v0[0] * OT_MAX_VAL); 
    unsigned int y0LocCode = (unsigned int) (v0[1] * OT_MAX_VAL); 
    unsigned int x1LocCode = (unsigned int) (v1[0] * OT_MAX_VAL); 
    unsigned int y1LocCode = (unsigned int) (v1[1] * OT_MAX_VAL); 


    //----Determine the XOR'ed pairs of locational codes of the region boundaries

    unsigned int xDiff = x0LocCode ^ x1LocCode;
    unsigned int yDiff = y0LocCode ^ y1LocCode;


    //----Determine the level of the smallest possible cell entirely containing 
    //----the region

    otCell *cell = root;
    unsigned int level = OT_ROOT_LEVEL;
    unsigned int minLevel = OT_ROOT_LEVEL;
    while (!(xDiff & (1 << level)) && level) level--;
    while (!(yDiff & (1 << minLevel)) && (minLevel > level)) minLevel--;
    minLevel++;


    //----Follow the branching patterns of the locational codes of v0 from the 

    //----root cell to the smallest cell entirely containing the region

    level = OT_ROOT_LEVEL - 1;
    OT_TRAVERSE_TO_LEVEL(cell,level,x0LocCode,y0LocCode,minLevel);
    return(cell);
}

#endif


//-------------------------------------------------------------------------------
//  Locate the left edge neighbor of the same size or larger than a specified 
//  cell. A null pointer is returned if no such neighbor exists.
//-------------------------------------------------------------------------------

otCell *otLocateLeftNeighbor (otCell *cell)
{
    //----No left neighbor if this is the left side of the octree

    if (cell->xLocCode == 0) return(0);
    else {
        //----Get cell's x, y and z locational codes and the x locational code of the
        //----cell's smallest possible left neighbor

        unsigned int xLocCode = cell->xLocCode;
        unsigned int yLocCode = cell->yLocCode;
        unsigned int zLocCode = cell->zLocCode;
        unsigned int xLeftLocCode = xLocCode - 0x00000001;
        

        //----Determine the smallest common ancestor of the cell and the cell's 
        //----smallest possible left neighbor

        unsigned int cellLevel, nextLevel;
        unsigned int diff = xLocCode ^ xLeftLocCode;
        otCell *pCell = cell;
        cellLevel = nextLevel = cell->level;
        OT_GET_COMMON_ANCESTOR(pCell,nextLevel,diff);
        

        //----Start from the smallest common ancestor and follow the branching 
        //----patterns of the locational codes downward to the smallest left
        //----neighbor of size greater than or equal to cell

        nextLevel--;
        OT_TRAVERSE_TO_LEVEL(pCell,nextLevel,xLeftLocCode,yLocCode,zLocCode,cellLevel);
        return(pCell);
    }
}


//-------------------------------------------------------------------------------
//  Locate the right edge neighbor of the same size or larger than a specified
//  cell. A null pointer is returned if no such neighbor exists.
//-------------------------------------------------------------------------------

otCell *otLocateRightNeighbor (otCell *cell)
{
    //----No right neighbor if this is the right side of the quadtree

    unsigned int binaryCellSize = 1 << cell->level;
    if ((cell->xLocCode + binaryCellSize) >= (1 << OT_ROOT_LEVEL)) return(0);
    else {
        //----Get cell's x and y locational codes and the x locational code of the
        //----cell's right neighbors

        unsigned int xLocCode = cell->xLocCode;
        unsigned int yLocCode = cell->yLocCode;
        unsigned int zLocCode = cell->zLocCode;
        unsigned int xRightLocCode = xLocCode + binaryCellSize;
        

        //----Determine the smallest common ancestor of the cell and the cell's 
        //----right neighbors 

        unsigned int cellLevel, nextLevel;
        unsigned int diff = xLocCode ^ xRightLocCode;
        otCell *pCell = cell;
        cellLevel = nextLevel = cell->level;
        OT_GET_COMMON_ANCESTOR(pCell,nextLevel,diff);
        

        //----Start from the smallest common ancestor and follow the branching 
        //----patterns of the locational codes downward to the smallest right
        //----neighbor of size greater than or equal to cell

        nextLevel--;
        OT_TRAVERSE_TO_LEVEL(pCell,nextLevel,xRightLocCode,yLocCode,zLocCode,cellLevel);
        return(pCell);
    }
}

#if 0

//-------------------------------------------------------------------------------

//  Locate the three leaf cell vertex neighbors touching the right-bottom vertex 

//  of a specified cell. bVtxNbr, rVtxNbr, and rbVtxNbr are set to null if the  

//  corresponding neighbor does not exist.

//-------------------------------------------------------------------------------

void qtLocateRBVertexNeighbors (otCell *cell, otCell **bVtxNbr, otCell **rVtxNbr,
otCell **rbVtxNbr)
{
    //----There are no right neighbors if this is the right side of the quadtree and 

    //----no bottom neighbors if this is the bottom of the quadtree

    unsigned int binCellSize = 1 << cell->level;
    unsigned int noRight = ((cell->xLocCode + binCellSize) >= (1 << OT_ROOT_LEVEL)) ? 1 : 0;
    unsigned int noBottom = (cell->yLocCode == 0) ? 1 : 0;


    //----Get cell's x and y locational codes and the x and y locational codes of 

    //----the cell's right and bottom vertex neighbors

    unsigned int xRightLocCode = cell->xLocCode + binCellSize;
    unsigned int xLocCode = xRightLocCode - 0x00000001;
    unsigned int yLocCode = cell->yLocCode;
    unsigned int yBottomLocCode = yLocCode - 0x00000001;
    unsigned int rightLevel, bottomLevel;
    unsigned int diff;
    otCell *commonRight, *commonBottom;


    //----Determine the right leaf cell vertex neighbor 

    if (noRight) *rVtxNbr = 0;
    else {
        //----Determine the smallest common ancestor of the cell and the cell's  

        //----right neighbor. Save this right common ancestor and its level for 

        //----determining the right-bottom vertex.

        unsigned int level = cell->level;
        diff = xLocCode ^ xRightLocCode;
        commonRight = cell;
        OT_GET_COMMON_ANCESTOR(commonRight,level,diff);
        rightLevel = level;
        

        //----Follow the branching patterns of the locational codes downward from 

        //----the smallest common ancestor to the right leaf cell vertex neighbor

        *rVtxNbr = commonRight;
        level--;
        OT_TRAVERSE_TO_LEVEL(*rVtxNbr,level,xRightLocCode,cell->yLocCode,0);
    } 


    //----Determine the bottom leaf cell vertex neighbor 

    if (noBottom) *bVtxNbr = 0;
    else {
        //----Determine the smallest common ancestor of the cell and the cell's

        //----bottom neighbor. Save this bottom common ancestor and its level for

        //----determining the right-bottom vertex.

        unsigned int level = cell->level;
        diff = yLocCode ^ yBottomLocCode;
        commonBottom = cell;
        OT_GET_COMMON_ANCESTOR(commonBottom,level,diff);
        bottomLevel = level;
        
        
        //----Follow the branching patterns of the locational codes downward from 

        //----the smallest common ancestor to the bottom leaf cell vertex neighbor

        *bVtxNbr = commonBottom;
        level--;
        OT_TRAVERSE_TO_LEVEL(*bVtxNbr,level,xLocCode,yBottomLocCode,0);
    }


    //----Determine the right-bottom leaf cell vertex neighbor 

    if (noRight || noBottom) *rbVtxNbr = 0;
    else {
        //----Follow the branching patterns of the locational codes downward from 

        //----the smallest common ancestor (the larger of the right common ancestor 

        //----and the bottom common ancestor) to the right-bottom leaf cell vertex 

        //----neighbor

        if (rightLevel >= bottomLevel) {
            *rbVtxNbr = commonRight;
            rightLevel--;
            OT_TRAVERSE_TO_LEVEL(*rbVtxNbr,rightLevel,xRightLocCode,yBottomLocCode,0);

        } else {
            *rbVtxNbr = commonBottom;
            bottomLevel--;
            OT_TRAVERSE_TO_LEVEL(*rbVtxNbr,bottomLevel,xRightLocCode,yBottomLocCode,0);
        }
    }
}
#endif
