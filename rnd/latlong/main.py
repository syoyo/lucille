import os, sys
import math

import wx
import Image
import Numeric

def usage():
    print "Usage: main.py <image>"
    sys.exit(0)

def array2image(a):
    if a.typecode() == Numeric.UnsignedInt8:
        mode = "L"
    elif a.typecode() == Numeric.Float32:
        mode = "F"
    else:
        raise ValueError, "unsupported image mode"
    return Image.fromstring(mode, (a.shape[1], a.shape[0]), a.tostring())

def pil2Image(pil):
    image = wx.EmptyImage(pil.size[0], pil.size[1])
    image.SetData(pil.convert('RGB').tostring())
    return image

def xyz2thetaphi(x, y, z):

    theta = math.acos(z)                # [0, pi]

    if math.fabs(theta) < 1.0e-6:
        phi = 0.0
    else:
        phi = math.atan2(y, x)          # [-pi, pi]
        
    phi += math.pi

    return (theta, phi)


class MainWindow(wx.Frame):
    def __init__(self,parent,id,title, pil):
        wx.Frame.__init__(self, parent, wx.ID_ANY, title, size = (400,400))
        wx.EVT_PAINT(self, self.OnPaint)

        self.bmp = pil2Image(pil).ConvertToBitmap()
        self.Show(True)

    def OnPaint(self, evt):
        dc = wx.PaintDC(self)
        dc.DrawBitmap(self.bmp, 0, 0, True)


def scale(f):
    i = int(f * 255.5)
    if i < 0: i = 0
    if i > 255: i = 255

    return i

def genImage():

    dims = (256,256)
    # arr = Numeric.zeros(dims,Numeric.UInt8)

    im = Image.new('RGB', dims)

    for p in xrange(dims[1]):
        for t in xrange(dims[0]):

            u = math.pi * t / dims[0]
            v = 2.0 * math.pi * p / dims[1]

            x = math.sin(u) * math.cos(v)
            y = math.sin(u) * math.sin(v)
            z = math.cos(u)

            (theta, phi) = xyz2thetaphi(x, y, z)

            im.putpixel( (p, t), (scale(0.5 * x + 0.5), scale(0.5 * y + 0.5), scale(0.5 * z + 0.5)) )
            
    # return array2image(arr)

    return im

def main():

    if len(sys.argv) < 2:
        im = genImage()
    else:
        im = Image.open(sys.argv[1])

    app = wx.PySimpleApp()
    frame = MainWindow(None, -1, "Imaging", pil=im)
    app.MainLoop()
        


if __name__ == '__main__':
    main()
