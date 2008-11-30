/* Beta-0.3.0: support functions for MOSAIC shaders for Blender integration */

//Adjust texture st with xy offset and size
void stAdjust(output float sAdj,tAdj;float s,t,texOfSz[4])
{
	sAdj = (s*texOfSz[2]-(texOfSz[2]-1)/2)+texOfSz[0];
	tAdj = (t*texOfSz[3]-(texOfSz[3]-1)/2)-texOfSz[1];
}

//Returns different attenuations of light depth
float Attenuation(float Ldepth,LinAtten,SqrAtten,QuadAtten,Distance,SphereAtten;)
{
	float attenuation = 1;
	attenuation = Distance/(LinAtten*Ldepth+SqrAtten*Ldepth*2+QuadAtten*Ldepth*Ldepth+Distance);
	if (SphereAtten > 0) attenuation *= 1-Ldepth/Distance;
	return clamp(attenuation,0,1);
}

//Simulate SSS through Cl with blurred shadows
color sssdiffuse(point P;normal N;float SSSfact,Frfact,Bkfact,Rfact,Gfact,Bfact;color SSScol,SSStex)
{
	color C = 0;
	color SSS = 0;
	illuminance(P,N,PI)
	{
		lightsource("SSSDepth",SSS); //Get shadow info
		float f = normalize(L).N; //Figure front direction
		float b = normalize(-L).N; //Figure back direction
			C += SSS*SSScol*smoothstep(-1,1,f)*Frfact*SSSfact; //Lightside sss
			C += SSS*SSScol*SSStex*color(Rfact,Gfact,Bfact)*smoothstep(-1,1,b)*Bkfact*SSSfact; //Darkside sss
	}
	return C;
}

