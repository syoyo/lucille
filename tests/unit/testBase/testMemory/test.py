from base_memory import *

import os, sys
import random

class TestMemoryAllocDoesNotReturnNull():

    def setup(self):
        pass 

    def teardown(self):
        pass

    def test(self):
        l = ri_mem_alloc(128)

        assert l != None

class TestMemoryFreeReturnsZero():

    def setup(self):
        self.mem = ri_mem_alloc(128) 

    def teardown(self):
        pass

    def test(self):
        ret = ri_mem_free(self.mem)

        assert (ret == 0), ret


class TestAlignedMallocWithRandomArg():

    def setup(self):
        pass

    def teardown(self):
        pass

    def test(self):

        nTests = 10000

        for i in range(nTests):
        
            sz    = random.randint(0, 1024*1024)
            align = random.randint(0, 1024)

            print "sz = %d, align = %d" % (sz, align)

            p = aligned_malloc(sz, align);

            assert int(p) % 16 == 0, "Not 16-byte aligned: addr(p) = %d" % int(p)

            aligned_free(p)

        


# class TestMemoryAllocWithLargeSizeWillReturnNull():
# 
#     def setup(self):
#         pass 
# 
#     def teardown(self):
#         pass
# 
#     def test(self):
#         try:
#             l = ri_mem_alloc(1024*1024*1024)
#         except Error:
#             print "OSError"
# 
#         pass

