surface
paintedplastic( float Ka = 1;
                float Kd = .5;
                float Ks = .5;
                float roughness = .1;
                color specularcolor = 1;
                string texturename = "";)
{
    normal Nf = faceforward(normalize(N), I);
    vector V = -normalize(I);
    Oi = Os;
    Ci = Os * ( Cs * color texture(texturename) *
              ( Ka * ambient() + Kd * diffuse(Nf))
            + specularcolor * Ks * specular(Nf, V, roughness));
}
      
