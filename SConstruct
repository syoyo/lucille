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
opts.Add(EnumOption('build_target', 'Build target', 'release',
                    allowed_values=('debug', 'release', 'fast')))
opts.Add('use_llvm', 'Use LLVM', 0)
opts.Add('enable_sse', 'Enable SSE(valid for x86 processor)', 1)
opts.Add('use_double', 'Use double precision', 0)
opts.Add('enable_64bit', 'Compile for 64bit environment', 0)
opts.Add('with_zlib', 'Use zlib for compression facility', 0)
opts.Add('LLVM_CC',     'LLVM C frontend')
opts.Add('LLVM_AR',     'LLVM ar')
opts.Add('LLVM_LD',     'LLVM ld')
opts.Add('LLVM_RANLIB', 'LLVM ranlib')
opts.Add('LLVM_LINK',   'LLVM link')


#
# path
#
path = os.environ['PATH']


#
# Get env
#
env = Environment(options = opts, ENV= {'PATH' : path})


#
# build mode
#
if env['build_target'] == 'debug':
	env.Append(CFLAGS = ['-g'])

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
# 64bit
#
if env['enable_64bit'] == 1:
	env.Append(CPPDEFINES = ['__64bit__'])
	env.Append(CFLAGS = ['-m64'])	# gcc only

#
# compression
#
if env['with_zlib'] == 1:
	env.Append(CPPDEFINES = ['WITH_ZLIB'])
	env.Append(CPPPATH = [env['ZLIB_INC_PATH']])
		

#
# Platform specific settings
#

platform = sys.platform
byteorder = sys.byteorder

#
# Is Intel Mac?
#
if platform == 'darwin' and byteorder == 'little':
	env.Append(CPPDEFINES = ['__x86__'])

	if env['enable_sse'] == 1:
		env.Append(CPPDEFINES = ['WITH_SSE'])

#
# I don't know how to determine CPU is x86 in linux.
# Assume x86 if byteorder is little.
#
if platform == 'linux2' and byteorder == 'little':
	env.Append(CPPDEFINES = ['__x86__'])

	if env['enable_sse'] == 1:
			env.Append(CPPDEFINES = ['WITH_SSE'])


#
# SSE handling
#
if env['enable_sse'] == 1:
	if env['CC'] in ['gcc', 'llvm-gcc']:
		env['CFLAGS'].append('-msse2')

SConscript(['src/SConscript'], exports='env')
