from base_list import *

import os, sys

class TestListNewIsNotNull():

    def setup(self):
        pass 

    def teardown(self):
        pass

    def testListNew(self):
        l = ri_list_new()

        assert l != chr(0)


class TestListNextAfterListNewIsNull():

    def setup(self):
        self.lst = ri_list_new()

    def teardown(self):
        ri_list_free(self.lst)

    def test(self):
        l = ri_list_next(self.lst)

        assert l == None

class TestListNextAfterListAddIsNotNull():

    def setup(self):
        self.lst = ri_list_new()
        self.val = ri_mem_alloc(128)

    def teardown(self):
        ri_list_free(self.lst)

    def test(self):
        l = ri_list_append(self.lst, self.val)

        assert l != None
