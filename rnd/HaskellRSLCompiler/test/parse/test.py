#!/usr/bin/env python

import os, sys
import subprocess
import re
import glob

errlog = []

def run(f):

    cmd = "../../lslc"

    p = subprocess.Popen([cmd, f], stdout=subprocess.PIPE, stderr=subprocess.PIPE, close_fds=True)

    outs = [l for l in p.stdout]
    errs = [l for l in p.stderr]

    errline = re.compile("TODO")

    failed = False
    for l in errs:
        if errline.search(l):
            failed = True


    if failed:
        print "[FAIL] ", f
        errlog.append("==== [" + f + "] ====")
        for l in errs:
            errlog.append(l[:-1])
        errlog.append("=====================")
        errlog.append("\n")
    else:
        print "[OK  ] ", f

def main():
    
    for f in glob.glob("*.sl"):
        run(f)

    f = open("errlog.log", "w")
    for l in errlog:
        print >>f, l

if __name__ == '__main__':
    main()
