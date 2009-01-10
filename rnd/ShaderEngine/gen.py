"""
From the description of data field, emit llvm struct definition and C struct.
"""

from string import Template

ll_filename       = "shader_env.ll"
c_header_filename = "shader_env.h"

shader_env_def = [
  #  Type    , Name,  Output?
    ("color" , "Ci",  True )
  , ("color" , "Oi",  True )
  , ("color" , "Cs",  False)
  , ("vector", "I" ,  False)
  , ("normal", "N" ,  False)
  , ("normal", "Ng",  False)
  , ("vector", "E" ,  False)
  , ("vector", "P" ,  False)
  , ("float" , "u" ,  False)
  , ("float" , "v" ,  False)
  , ("float" , "s" ,  False)
  , ("float" , "t" ,  False)
  , ("int"   , "x" ,  False)        # LSE only. raster pos.
  , ("int"   , "y" ,  False)        # LSE only. raster pos.
  , ("int"   , "z" ,  False)        # LSE only. raster pos.
  , ("int"   , "w" ,  False)        # LSE only. raster pos.
  , ("vector", "L" ,  False)        # Hack
  , ("color" , "Cl",  False)
  , ("color" , "Ol",  False)
  ]

def gen_llvm_code(fname):

    ty_table = {
        "vector" : "<4 x float>"
      , "normal" : "<4 x float>"
      , "color"  : "<4 x float>"
      , "float"  : "float"
      , "int"    : "i32"
    }

    # emit struct def
    s = "%struct.ri_shader_env_t = type { "

    for (n, (ty, name, isoutput)) in enumerate(shader_env_def):
        llty = ty_table[ty]
        s += "%s" % llty 
        if n != len(shader_env_def) - 1:
            s += ", "
        else:
            s += " "
        
    s += "}\n"

    s += "@genv = internal global %struct.ri_shader_env_t zeroinitializer, align 32\n"

    # emit getter method
    for (n, (ty, name, isoutput)) in enumerate(shader_env_def):

        llty = ty_table[ty]

        d         = {}
        d["n"]    = n
        d["ty"]   = llty
        d["name"] = name

        ss = """
define ${ty} @rsl_get${name}() nounwind {
    %tmp = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 ${n}
    %tmp1 = load ${ty}* %tmp
    ret ${ty} %tmp1
}
"""

        s += Template(ss).substitute(d)

        if isoutput:

            ss = """
define void @rsl_set${name}(${ty} %val) nounwind {
    %tmp = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 ${n}
    store ${ty} %val, ${ty}* %tmp
    ret void
}
"""
            s += Template(ss).substitute(d)

    # emit shader_env struct copy method
    s += "\n"
    s += "define void @set_shader_env(%struct.ri_shader_env_t* %env) {\n"

    for (n, (ty, name, isoutput)) in enumerate(shader_env_def):

        d = {}
        d["n"] = n
        d["ty"]   = ty_table[ty]

        ss = """
    %srcaddr${n} = getelementptr %struct.ri_shader_env_t* %env, i32 0, i32 ${n}
    %tmp${n}     = load ${ty}* %srcaddr${n} 
    %dstaddr${n} = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 ${n}
    store ${ty} %tmp${n}, ${ty}* %dstaddr${n} 

"""
        s += Template(ss).substitute(d)

    s += "    ret void\n}\n"

    s += "\n"
    s += "define void @get_shader_env(%struct.ri_shader_env_t* %env) {\n"

    for (n, (ty, name, isoutput)) in enumerate(shader_env_def):

        d = {}
        d["n"] = n
        d["ty"]   = ty_table[ty]

        ss = """
    %srcaddr${n} = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 ${n}
    %tmp${n}     = load ${ty}* %srcaddr${n} 
    %dstaddr${n} = getelementptr %struct.ri_shader_env_t* %env, i32 0, i32 ${n}
    store ${ty} %tmp${n}, ${ty}* %dstaddr${n} 

"""
        s += Template(ss).substitute(d)

    s += "    ret void\n}\n"
 
    f = open(fname, "w")
    print >>f, s
    f.close()

def gen_c_header(fname):

    ty_table = {
        "vector" : "float4"
      , "normal" : "float4"
      , "color"  : "float4"
      , "float"  : "float"
      , "int"    : "int"
    }

    s = """
#ifndef SHADER_ENV_H
#define SHADER_ENV_H

#ifdef __cplusplus
extern "C" {
#endif

typedef float float4[4];

"""

    s += "typedef struct _ri_shader_env_t {\n"

    for (ty, name, isoutput) in shader_env_def:
        cty = ty_table[ty]
    
        s += "    %s %s;\n" % (cty, name);

    s += "} ri_shader_env_t;\n"


    s += """

#ifdef __cplusplus
}
#endif

#endif
"""

    print s

    f = open(fname, "w")
    print >>f, s
    f.close()

if __name__ == "__main__":
    gen_llvm_code(ll_filename)
    gen_c_header(c_header_filename)
