import distutils
from distutils.core import setup, Extension

import os

srcPath = "../../../../src/base"

srcList = [ "memory.i"
          , os.path.join(srcPath, "memory.c") 
          , os.path.join(srcPath, "list.c") 
          ]


setup(name = "base_memory",
      version = "1.0",
      ext_modules = [Extension("_base_memory", sources=srcList, include_dirs = [srcPath])])
