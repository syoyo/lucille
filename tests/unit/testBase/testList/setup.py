import distutils
from distutils.core import setup, Extension

import os

srcPath = "../../../../src/base"

srcList = [ "list.i"
          , os.path.join(srcPath, "list.c") 
          , os.path.join(srcPath, "memory.c") 
          ]


setup(name = "base_list",
      version = "1.0",
      ext_modules = [Extension("_base_list", sources=srcList, include_dirs = [srcPath])])
