Haskell + parsec version of RSL compiler.
=========================================

  Dec 5th, 2008

What is this?
-------------

This is a prototype implementation of RenderMan Shading Language compiler written in Haskell + parsec.

The compiler will be merged with lucille core and be used as a practical RSL shader compiler in the next release of lucille.

Compilation phases
------------------

 * Parse RSL program source with parsec and build untyped AST(Abstract Syntax Tree)
 * Apply syntax fixing, semantic checking and typing for Untyped AST and build a Typed, verified AST.
 * (Optional) Do some symbolic optimization(e.g. SPMD -> SIMD)
 * Emit LLVM IR code from the AST.
   * No LLVM library or language binding are used to emit LLVM IR.

 * Optimize the shader and compile it into native binary using LLVM.

Structure of source codes 
-------------------------

::

  Main.hs             main program

  RSL/
    AST.hs            Defines data strucutre for RSL AST.
    Parser.hs         RSL parser with Parsec.
    PPrint.hs         Pretty printer for AST. 
    Typer.hs          Typer for RSL AST.

TODO
----

 * C preprocessor. Use mcpp?
 * LLVM IR codegen
 * Syntax fixing
 * Typer

References
----------

 * Haskell, Functional programming language:
   www.haskell.org

 * Parsec, Monadic parser combinator:
   http://www.haskell.org/haskellwiki/Parsec
   (Parsec is included in GHC, A Haskell compiler)

 * LLVM, The compiler infrastructure:
   http://www.llvm.org/
  
 * RenderMan Shading Language specification:
   https://renderman.pixar.com/products/rispec/index.htm


Author
------

 * Syoyo Fujita
   syoyo@lucillerender.org
