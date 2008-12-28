#!/usr/bin/env python

import os, sys
import subprocess

# config
LLVM_AS    = "llvm-as" 
LLVM_LINLK = "llvm-link" 
RSL2LLVM   = "../HaskellRSLCompiler/lslc" 

def compile(fname):

    # Compile RSL into LLVM IR
    cmd  = RSL2LLVM
    cmd += " "
    cmd += fname

    ret = subprocess.call(cmd, shell=True)

    # Link

def usage():
    print "slc.py <input.sl>"

def main():
    if len(sys.argv) < 2:
        usage()
        sys.exit(1);

    compile(sys.argv[1])


if __name__ == '__main__':
    main()
