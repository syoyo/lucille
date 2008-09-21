#
# If you want to specify your C compiler, uncomment below line and set it.
#
#CC = 'gcc'

custom_cflags   = [ '-g', '-W', '-Wall']
custom_cxxflags = [ '-g', '-W', '-Wall']

#
# OpenMPI support(TODO)
#
#enable_openmpi = 0
#OPENMPI_CC = 'mpicc'
#OPENMPI_INC_PATH = '/usr/local/include'
#OPENMPI_LIB_PATH = '/usr/local/lib'
#OPENMPI_LIB_NAME = '/usr/local/lib'


#
# Build target 
#
# 'debug'   : debug compile(-g).
# 'release' : release compile(-O2). default
# 'speed'   : Maximum optimization. experimental
build_target = 'debug'

#
# SSE option. default = 1
#
enable_sse = 1

#
# Specify floating point precision.
#
# 0 : use float
# 1 : use double(default)
#
use_double = 1

#
# LLVM settings
#
#use_llvm = 1
LLVM_CC     = 'llvm-gcc'
LLVM_AR     = 'llvm-ar'
LLVM_LD     = 'llvm-ld'
LLVM_RANLIB = 'llvm-ranlib'
LLVM_LINK   = 'llvm-ld'

#
# 64bit
#
enable_64bit = 0


#
# compression support
#
#with_zlib = 1
ZLIB_INC_PATH = '/usr/include'
ZLIB_LIB_PATH = '/usr/lib'
ZLIB_LIB_NAME = 'z'


#
# If you want to support jpeg image as a texture, enable this.
#
with_jpeglib = 1
JPEGLIB_INC_PATH = '/usr/local/include'
JPEGLIB_LIB_PATH = '/usr/local/lib'
JPEGLIB_LIB_NAME = 'jpeg'
