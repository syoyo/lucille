#ifndef BVHVISUALIZER_H
#define BVHVISUALIZER_H

#include <cassert>
#include <vector>

#include "bvh.h"

class BVHVisualizer {

  public:

    BVHVisualizer() {
        this->nodeStack.clear();
    }

    void setBVH(ri_bvh_t *bvh) {

        assert(bvh != NULL);

        this->bvh = bvh;

        this->nodeStack.push_back( bvh->root );
    }

    void drawBVH();
    int  followLeft();
    int  followRight();
    int  followParent();
    


  private:

    ri_qbvh_node_t *getCurrentNode() {
        return this->nodeStack[this->nodeStack.size() - 1];
    }

    ri_bvh_t                      *bvh;

    std::vector<ri_qbvh_node_t *>  nodeStack;

};


#endif  // BVHVISUALIZER_H
