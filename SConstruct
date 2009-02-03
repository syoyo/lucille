# vim: set noexpandtab
import os, sys

#
# Options for scons
#
#SetOption('implicit_cache', 1) # cache dependency


#
# Local options
#
opts = Options('custom.py')
opts.Add('CC', 'C compiler')
opts.Add('custom_cflags', 'User defined CFLAGS')
opts.Add('custom_cxxflags', 'User defined CXXFLAGS')
opts.Add(EnumOption('build_target', 'Build target', 'release',
                    allowed_values=('debug', 'release', 'fast')))

opts.Add('use_llvm'		, 'Use LLVM', 0)
opts.Add('enable_sse'		, 'Enable SSE(valid for x86 processor)', 1)

#opts.Add('enable_muda'		, 'Enable MUDA(valid for x86/SSE processor)', 0)
#opts.Add('MUDA_INC_PATH'	, 'Include path to MUDA')

opts.Add('use_double'		, 'Use double precision', 0)
opts.Add('enable_64bit'		, 'Compile for 64bit environment', 0)

opts.Add('with_zlib'		, 'Use zlib for compression facility', 0)

opts.Add('with_jpeglib'		, 'Use jpeglib to load/save jpeg image', 0)
opts.Add('JPEGLIB_INC_PATH'	, 'Include path to libjpeg')
opts.Add('JPEGLIB_LIB_PATH'	, 'Lib path to libjpeg')
opts.Add('JPEGLIB_LIB_NAME'	, 'Library name of libjpeg')

opts.Add('with_x11'		, 'Use X11')
opts.Add('X11_LIB_PATH'		, 'Lib path to libX11')

opts.Add('LLVM_CC'		, 'LLVM C frontend')
opts.Add('LLVM_AR'		, 'LLVM ar')
opts.Add('LLVM_LD'		, 'LLVM ld')
opts.Add('LLVM_RANLIB'		, 'LLVM ranlib')
opts.Add('LLVM_LINK'		, 'LLVM link')

# Installer options
opts.Add('install_prefix'	, 'Prefix to install dir')

AddOption('--prefix',
          dest='prefix',
          type='string',
          nargs=1,
          action='store',
          metavar='DIR',
          help='installation prefix')

#
# path
#
path = os.environ['PATH']


#
# Get env
#
env = Environment(options = opts, ENV={'PATH' : path}, PREFIX = GetOption('prefix'))

#
# Add custom CFLAGS
#
env.Append(CFLAGS   = env['custom_cflags'])
env.Append(CXXFLAGS = env['custom_cxxflags'])

#
# build mode
#
if env['build_target'] == 'debug':
	env.Append(CFLAGS   = ['-g'])
	env.Append(CXXFLAGS = ['-g'])

if env['build_target'] == 'release':
	env.Append(CFLAGS    = ['-g', '-O2'])
	env.Append(CXXFLAGS  = ['-g', '-O2'])
	env.Append(LINKFLAGS = ['-g'])

#
# LLVM
#
if env['use_llvm'] == 1:
	# Replace with llvm toolchain
	env['CC']      = env['LLVM_CC']
	env.Append(CFLAGS = ['-emit-llvm'])
	env['AR']     = env['LLVM_AR']
	env['LD']     = env['LLVM_LD']
	env['RANLIB'] = env['LLVM_RANLIB']
	env['LINK']   = env['LLVM_LINK']

#
# Floating point precision
#
if env['use_double'] == 1:
	env.Append(CPPDEFINES = ['ENABLE_DOUBLE_PRECISION'])


#
# MUDA(TODO)
#
#if env['enable_muda'] == 1:
#	env.Append(CPPDEFINES = ['WITH_MUDA'])
#	env.Append(CFLAGS     = ['-msse2'])       # gcc only
#	env.Append(CPPPATH    = [env['MUDA_INC_PATH']])

#
# 64bit
#
if env['enable_64bit'] == 1:
	env.Append(CPPDEFINES = ['__64bit__'])
	env.Append(CFLAGS     = ['-m64'])         # gcc only
	env.Append(LINKFLAGS  = ['-m64'])         # gcc only

#
# compression
#
if env['with_zlib'] == 1:
	env.Append(CPPDEFINES = ['WITH_ZLIB'])
	env.Append(CPPPATH = [env['ZLIB_INC_PATH']])
		
#
# JPEG support
#
if env['with_jpeglib'] == 1:
	env.Append(CPPDEFINES = ['WITH_JPEGLIB'])
	env.Append(CPPPATH = [env['JPEGLIB_INC_PATH']])

#
# Platform specific settings
#

platform = sys.platform
byteorder = sys.byteorder

#
# Is Intel Mac?
#
if platform == 'darwin' and byteorder == 'little':
	# Multi-threading using pthread
	env.Append(CPPDEFINES = ['WITH_PTHREAD'])

	env.Append(CPPDEFINES = ['__x86__'])

	if env['with_x11']:
		env.Append(CPPDEFINES = ['WITH_X11'])

	if env['enable_sse'] == 1:
		env.Append(CPPDEFINES = ['WITH_SSE'])

#
# I don't know how to determine what kind of processor is running under
# linux environment.
# Assume x86 if byteorder is little.
#
if platform == 'linux2' and byteorder == 'little':
	env.Append(CPPDEFINES = ['__x86__'])

	if env['enable_sse'] == 1:
			env.Append(CPPDEFINES = ['WITH_SSE'])

#
# Common settings for linux target.
#
if platform == 'linux2':
	# Multi-threading using pthread
	env.Append(CPPDEFINES = ['WITH_PTHREAD'])

	env.Append(CPPDEFINES = ['LINUX'])

	if env['with_x11']:
		env.Append(CPPDEFINES = ['WITH_X11'])

#
# SSE handling
#
if env['enable_sse'] == 1:
	if env['CC'] in ['gcc', 'llvm-gcc']:
		env['CFLAGS'].append('-msse2')

SConscript(['src/SConscript'], exports='env')
