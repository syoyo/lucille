import os, sys

#
# Options for scons
#
#SetOption('implicit_cache', 1) # cache dependency


#
# Local options
#
opts = Options('custom.py')
opts.Add('CC', 'c comp')

env = Environment(options = opts)

platform = sys.platform
byteorder = sys.byteorder

# Is Intel Mac?
if platform == 'darwin' and byteorder == 'little':
	env.Append(CPPDEFINES = '__x86__')

SConscript(['src/SConscript'], exports='env')
