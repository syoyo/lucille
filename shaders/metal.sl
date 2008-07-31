surface
metal(float Ka = 1;
      float Ks = 1;
      float roughness = .1;)
{
    normal Nf = faceforward(normalize(N), I);
    vector V = -normalize(I);
    Oi = Os;
    Ci = Os * Cs * (Ka * ambient() + Ks * specular(Nf, V, roughness));
}
      
