#!/usr/bin/env python

import os, sys
import subprocess

# config
LLVM_AS                 = "llvm-as" 
LLVM_LINLK              = "llvm-link" 
LLVM_OPT                = "opt"
RSL2LLVM                = "../../HaskellRSLCompiler/lslc" 
OUTPUT_SHADER_MODULE    = "shader.bc"
SHADER_LL               = "output.ll"
SHADERENV_LL            = "shader_env.ll"
SHADERLIB_BC            = "shaderlib.bc noise.bc"

def get_bc_name(fname):

    basename = os.path.splitext(fname)[0]

    return basename + ".bc"

def compile(fname):

    basename = os.path.splitext(fname)[0]

    # Compile RSL into LLVM IR
    cmd  = RSL2LLVM
    cmd += " "
    cmd += fname

    print cmd
    ret = subprocess.call(cmd, shell=True)
    if ret is not 0:
        print "Failed to compilation"
        sys.exit(1)

    # Assemble IR into BC
    cmd  = LLVM_AS
    cmd += " -f "
    cmd += " "
    cmd += SHADER_LL
    print cmd
    ret = subprocess.call(cmd, shell=True)

    # Assemble runtime support lib into LLVM BC
    cmd  = LLVM_AS
    cmd += " -f "
    cmd += " "
    cmd += SHADERENV_LL

    print cmd
    ret = subprocess.call(cmd, shell=True)

    # Link
    outname = basename + ".bc"
    cmd  = LLVM_LINLK
    cmd += " -f "
    cmd += " -o " + outname
    cmd += " " + get_bc_name(SHADERENV_LL)
    cmd += " " + get_bc_name(SHADER_LL)
    cmd += " " + SHADERLIB_BC

    print "Linking shader modules..." 
    print cmd
    ret = subprocess.call(cmd, shell=True)

    # Opt
    outname = basename + ".bc"
    cmd  = LLVM_OPT
    cmd += " -f "
    cmd += " -std-compile-opts "
    cmd += " -o " + outname
    cmd += " " + outname

    print "Optimizing shader modules..." 
    print cmd
    ret = subprocess.call(cmd, shell=True)


def usage():
    print "slc.py <input.sl>"

def update_runtimelib():
    cmd = "make gen"
    print cmd
    ret = subprocess.call(cmd, shell=True)
    
def main():
    if len(sys.argv) < 2:
        usage()
        sys.exit(1);

    update_runtimelib()
    compile(sys.argv[1])


if __name__ == '__main__':
    main()
