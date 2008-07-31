/*
 * Simple texture mapping shader.
 *
 * Syoyo Fujita
 *
 * $Id: texturemapping.sl,v 1.1 2004/06/20 12:22:36 syoyo Exp $
 */
surface
texturemapping(string texturename = "")
{
    Ci = Os * ( Cs * color texture(texturename));	/* Ci = texcol */
    Oi = Os;
}
      
