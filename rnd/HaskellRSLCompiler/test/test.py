#!/usr/bin/env python

import os, sys
import subprocess
import re

errLineRE = re.compile("Parse err")

slfilepath = "../../../shaders"

slfiles = [
    "matte.sl"
  , "null.sl"
  , "constant.sl"
  , "mirror.sl"
  , "metal.sl"
  , "checker.sl"
  , "turbulence.sl"
  , "granite.sl"
  , "normdir.sl"
  , "ambientocclusion.sl"
  , "paintedplastic.sl"
  , "plastic.sl"
  , "shinymetal.sl"
  , "show_st.sl"
  , "show_xyz.sl"
  , "texturemapping.sl"
  , "turbulence2.sl"
  , "weird.sl"
  , "whitted.sl"
  , "wood.sl"
]

def run_test(fname):

    failed = False

    if os.access("output.ll", os.F_OK):
        os.remove("output.ll")

    cmd = "../lslc "
    cmd += fname

    p = subprocess.Popen(cmd, shell=True, stderr=subprocess.PIPE, stdout=subprocess.PIPE)

    lines = []
    for l in p.stdout:
        lines.append(l[:-1])
    for l in p.stderr:
        lines.append(l[:-1])
 
    for l in lines:
        if errLineRE.match(l):
            failed = True

    if not os.access("output.ll", os.F_OK):
        failed = True

    if failed:
        print "=================================================="
        print "Test failed: ", fname
        print "=================================================="
        for l in lines:
            print l
    else:
        print "Test %s OK" % fname

def main():
    
    for sl in slfiles:
        
        fname = os.path.join(slfilepath, sl)

        run_test(fname)

if __name__ == '__main__':
    main()
