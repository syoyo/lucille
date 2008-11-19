#!/usr/bin/env python

import sys
import re

def main():
    
    if len(sys.argv) < 2:
        printf("Usage: expected.py <expected_string>")
        sys.exit(0)

    expected = sys.argv[1]

    inputstr = "" 

    for l in sys.stdin:
        inputstr += l

    if re.search(expected, inputstr):
        sys.exit(0)    # OK
    else:
        sys.exit(1)    # Unexpected
 
if __name__ == "__main__":
    main()
