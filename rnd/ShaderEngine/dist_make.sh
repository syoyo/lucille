#!/bin/sh

mkdir -p ShaderEngine
cp README.binary.txt llvm-as llvm-link clang slengine matte.sl wood.sl compile.py opt lslc muda512.ppm shader_env.ll noise.bc shaderlib.bc ShaderEngine
tar -c ShaderEngine > ShaderEngine.tar
gzip ShaderEngine.tar
