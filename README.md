Tgls — Thin bindings to OpenGL {3,4} and OpenGL ES {2,3} for OCaml
-------------------------------------------------------------------------------
Release %%VERSION%%

Tgls is a set of independent OCaml libraries providing thin bindings
to OpenGL libraries. It has support for core OpenGL 3.{2,3} and
4.{0,1,2,3,4} and OpenGL ES 2 and 3.{0,1}.

Tgls depends on [ocaml-ctypes][1] and the C OpenGL library of your
platform. It is distributed under the BSD3 license.
          
[1]: https://github.com/ocamllabs/ocaml-ctypes

Home page: http://erratique.ch/software/tgls  
Contact: Daniel Bünzli `<daniel.buenzl i@erratique.ch>`


## Installation

Tgls can be installed with `opam`:

    opam install tgls

If you don't use `opam` consult the [`opam`](opam) file for
build instructions and a complete specification of the dependencies.


## Supported OpenGL versions 

Tgls provides four libraries and corresponding `ocamlfind` packages:

* `tgl3`, supports all functions and enumerants to program with a
   core OpenGL 3.2 or OpenGL 3.3 context.

* `tgl4`, supports all functions and enumerants to program with a
   core OpenGL 4.0 to 4.5 context.

* `tgles2`, supports all functions and enumerants to program with an
   OpenGL ES 2.0 context.

* `tgles3`, supports all functions and enumerants to program with an
   OpenGL ES 3.0 to 3.1 context.

Compatibility contexts are not supported. For extensions, most of them
only add few entry points and/or enumerants, as such it seems the
easiest way to access them is to manually use [ocaml-ctypes][1] and
the appropriate constants (the tools in [support](support/) could be
enhanced to support them but it's not planned to do so).


## Documentation

The documentation and API reference is automatically generated by
`ocamldoc` from the interfaces. It can be consulted [online][3] and
there is a generated version in the `doc` directory of the
distribution.

[3]: http://erratique.ch/software/tgls/doc/


## Sample programs

If you installed `tgls` with `opam` sample programs are located in the
directory `opam config var tgls:doc`. Their source has a comment on
how to compile them.

In the distribution sample programs are located in the `test`
directory of the distribution. You'll need [tsdl][4] to compile them.

They can be built with:

    ocamlbuild -use-ocamlfind tests.otarget

The resulting binaries are in `_build/test` :

- `linkgl{3,4,es2,es3}.native`, tests that the C OpenGL library is 
  correctly linked, the executables should exit with 0. 
- `trigl{3,4,es2,es3}.native`, opens a window and draws a tricolor 
  triangle, needs [tsdl][4].

The C file [`assert_sizes.c`](test/assert_sizes.c) is a program that
should exit with 0 on your platform to ensure the bindings will
work correctly. 
  
[4]: http://erratique.ch/software/tsdl 
