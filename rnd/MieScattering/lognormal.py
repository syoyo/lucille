import math

def w():

    c     = 0.6

    for wf in range(0, 10):

        if wf < 2.0:
            r43_fat =  -0.2528 * wf * wf + 1.419 * wf
        else:
            r43_fat =  1.456 * math.pow(wf, 0.36)

        r_vs_fat = r43_fat / (c * c + 1.0)

        print r43_fat


def casein():

    mean = 43
    sigma = 43

    r = 1.0

    alpha = math.log(mean) - 0.5 * math.log(sigma * sigma / (mean * mean) + 1)
    beta  = math.sqrt(math.log(sigma * sigma / (mean * mean) + 1))

    while r < 150.0:

        k0 = 1 / (r * beta * math.sqrt(2.0 * math.pi))
        k1 = (math.log(r) - alpha) / beta
        n = k0 * math.exp(-0.5 * k1 * k1) 
        
        print n

        r += 1.0


def main():
    
    c     = 0.6
    wf    = 3.0;

    if wf < 2.0:
        r43_fat =  -0.2528 * wf * wf + 1.419 * wf
    else:
        r43_fat =  1.456 * math.pow(wf, 0.36)

    r_vs_fat = r43_fat / (c * c + 1.0)      # [um]
        
    mean  = r_vs_fat
    sigma = c * mean

    print "mean ", mean
    print "sigma ", sigma

    alpha = math.log(mean) - 0.5 * (sigma * sigma / (mean * mean) + 1)
    beta  = math.sqrt(math.log(sigma * sigma / (mean * mean) + 1))

    r = 0.005   # [um]
    while (r < 10.0):
        k0 = 1 / (r * beta * math.sqrt(2.0 * math.pi))
        # print k0
        k1 = (math.log(r) - alpha) / beta
        # print k1
        n = k0 * math.exp(-0.5 * k1 * k1) 
        print r, " ", n

        r += 0.01
    

if __name__ == "__main__":
    # w()
    # main()
    casein()
