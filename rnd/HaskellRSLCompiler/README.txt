Haskell + parsec version of RSL compiler.
=========================================

  Started: Dec 9th, 2008
  Updated: Mar 10th, 2009

What is this?
-------------

This is a prototype implementation of RenderMan Shading Language compiler written in Haskell + parsec.

The compiler will be merged with lucille core and be used as a practical RSL shader compiler in the next release of lucille.


Requirements
-------------

 * Haskell libraries
   - fgl, functional graph libraray.
     http://hackage.haskell.org/cgi-bin/hackage-scripts/package/fgl

   - wl-pprint, pretty printing library.
     http://hackage.haskell.org/cgi-bin/hackage-scripts/package/wl-pprint-1.0

 * mcpp: A portable C preprocessor with Validation Suite
   http://mcpp.sourceforge.net/index.html

Compilation phases
------------------

 * Preprocess RSL source file by mcpp.
 * Parse RSL program source with parsec and build partially-typed AST(Abstract Syntax Tree)
 * Apply syntax fixing, semantic checking and typing for partially-typed AST, and build a completely-typed and verified AST.
 * (Optional) Do some symbolic optimization(e.g. SPMD -> SIMD)
 * Emit LLVM IR code from typed AST.
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
    Sema.hs           Description of RSL syntax and semantic . 
    Typer.hs          Typer for RSL AST.
    CodeGenLLVM.hs    Code generator for LLVM IR from RSL AST.
    CFG.hs            Call Flow Graph for RSL AST.
                      Do some higher level optimization.


TODO
----

 * Syntax fixing
 * More sophisticated type system.
 * Optimization with CFG.


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

 * Types and Programming Languages,
   Benjamin C Pierce, MIT Press 2002.
 
 * Write Yourself a Scheme in 48 Hours
   http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours 

 * Chopstics: C parser in parsec.
   http://www.cse.unsw.edu.au/~rubenz/repos/Chopsticks/Parser.hs


Author
------

 * Syoyo Fujita ( syoyo@lucillerender.org )
