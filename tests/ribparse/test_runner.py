#!/usr/bin/env python

import os, sys
import glob
import subprocess
import re

def do_it():

    for f in glob.glob("*.rib"):

        bin  = "../../src/lsh/lsh"

        p = subprocess.Popen([bin, f], stdout=subprocess.PIPE, stderr=subprocess.PIPE, close_fds=True)

        outs = [l for l in p.stdout]
        errs = [l for l in p.stderr]

        if len(errs) > 0:
            print "Test failed: %s" % f

            print "==== STDOUT ===="
            for l in outs:
                print l,

            print "==== STDERR ===="
            for l in errs:
                print l,

            sys.exit(1)

        #
        # Check output
        #
        firstline = open(f, "r").readline()

        if firstline[0] == "#":

            checkcmd = firstline[1:]
            if checkcmd[0] == "|":  # pipe

                arg = ""
                for l in outs:
                    arg += l

                cmd = "echo '" + arg + "'"
                cmd += checkcmd
                
                ret = subprocess.call(cmd, shell=True)
        
                if ret == 1:    # Unexpected
        
                    print "==== CMD ===="
                    print cmd

                    print "==== STDOUT ===="
                    for l in outs:
                        print l,

                    print "Test " + f + " ... Failed"
                    sys.exit(1)

        print "Test " + f + " ... OK"


if __name__ == '__main__':
    do_it() 

