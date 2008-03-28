from base_memory import *

import os, sys

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

        assert ret == 0



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

