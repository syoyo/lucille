surface
shinymetal(float Ka = 1;
           float Ks = 1;
           float Kr = 1;
           float roughness = .1;
           string texturename = "";)
{
    normal Nf = faceforward(normalize(N), I);
    vector V = -normalize(I);
    vector D = reflect(I, normalize(Nf));
    D = vtransform("current", "world", D);
    Oi = Os;
    Ci = Os * Cs * (Ka * ambient()
                 +  Ks * specular(Nf, V, roughness)
                 +  Kr * color environment(texturename, D));
}
      
