import math
import struct
import random

def xcombinations(items, n):
    if n==0: yield []
    else:
        for i in xrange(len(items)):
            for cc in xcombinations(items[:i]+items[i+1:],n-1):
                yield [items[i]]+cc

def xuniqueCombinations(items, n):
    if n==0: yield []
    else:
        for i in xrange(len(items)):
            for cc in xuniqueCombinations(items[i+1:],n-1):
                yield [items[i]]+cc
            
def xselections(items, n):
    if n==0: yield []
    else:
        for i in xrange(len(items)):
            for ss in xselections(items, n-1):
                yield [items[i]]+ss

def xpermutations(items):
    return xcombinations(items, len(items))

def b2f(b):
    return struct.unpack('f',struct.pack('I',b))[0]

def random_f32():
    """
    Returns [0.0, 1.0)
    >>> random_f32() < 1.0
    True
    """

    return float(random.random())

def corner_values_f32():
    """
    Return list of corner values(e.g. NaN, 0.0, ...)
    """

    d = {}
    d["kPositiveZero" ] = b2f(0x00000000)
    d["kNegativeZero" ] = b2f(0x80000000)
    d["kPositiveInf"  ] = b2f(0x7F800000)
    d["kNegativeInf"  ] = b2f(0xFF800000)
    d["kPositiveQNaN" ] = b2f(0x7FFFFFFF)
    d["kNegativeQNaN" ] = b2f(0xFFFFFFFF)
    d["kPositiveSNaN" ] = b2f(0x7FBFFFFF)
    d["kNegativeSNaN" ] = b2f(0xFFBFFFFF)

    return d

    

if __name__ == '__main__':

    import sys, os

    print random_f32()
    print corner_values_f32()

    for val in xpermutations([1, 2, 3]):
        print val

