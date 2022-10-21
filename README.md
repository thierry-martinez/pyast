# Python versioned abstract syntax trees and interface to the Python parser

This library provides versioned abstract syntax tree for all Python
versions from Python 2.5 to Python 3.11.

## Related work

The library [`pyre-ast`](https://github.com/grievejia/pyre-ast) is
another OCaml library for parsing Python files using the Python
interpreter itself.
However, `pyast` emphasizes compability between Python versions,
enabling OCaml programs to be written with the version of the AST of
their choice and still be compatible with the interpreter installed on
the system, whatever its version.

## Installation

The library is not packaged in the official opam repository yet.
It can be pinned with the following command.
```
opam pin add https://github.com/thierry-martinez/pyast.git
```

Otherwise, the command `dune build @install && dune install` performs
the installation (or, equivalently, `make install`).

## Usage

All Python versions are represented as a module, from `Pyast.V2_5` to
`Pyast.V3_11_0`, which is aliased to `Pyast.Latest`: it is recommended
to use preferably `Pyast.Latest`, unless there is a reason to target a
particular verison of Python.
The API documentation can be generated with `opam build @doc`
(or, equivalently, `make doc`): the documentation will be
generated in `_build/default/_doc/_html/`.

`utop` can be convenient to discover the API. For instance,

```
% dune utop
# Py.initialize ();;
# Pyast.Latest.parse "x = 0";;
- : Pyast.V3_11_0.mod_ =
Pyast.Latest.Ast.Module
 {Pyast.Latest.Ast.body =
   [{Pyast.Latest.Ast.desc =
      Pyast.Latest.Ast.Assign
       {Pyast.Latest.Ast.targets =
         [{Pyast.Latest.Ast.desc =
            Pyast.Latest.Ast.Name
             {Pyast.Latest.Ast.id = "x"; ctx = Pyast.Latest.Ast.Store};
           lineno = 1; col_offset = 0; end_lineno = None;
           end_col_offset = None}];
        value =
         {Pyast.Latest.Ast.desc =
           Pyast.Latest.Ast.Constant
            {Pyast.Latest.Ast.value =
              Some (Pyast.V3_11_0.Num (Pyast.V3_11_0.Int 0));
             kind = None};
          lineno = 1; col_offset = 4; end_lineno = None; end_col_offset = None};
        type_comment = None};
     lineno = 1; col_offset = 0; end_lineno = None; end_col_offset = None}];
  type_ignores = []}
```
Parsing is delegated to the Python interpreter which is executed through bindings that are provided by [`py.ml`](https://github.com/thierry-martinez/pyml#pyml-ocaml-bindings-for-python "pyml: OCaml bindings for Python").
All the modules `Pyast.V*` mentioned above provide the following functions:

* `val parse_ast : Py.Object.t -> Ast.mod_`, which builds an OCaml
  representation from an abstract syntax tree already parsed in Python
  (typically with `ast.parse`).

* `val parse : ?⁠filename:string -> ?⁠mode:Pyast__.Pyparse.compile_mode
  -> string -> Ast.mod_`, which parses a string in a given `mode`,
  using `filename` in error messages (`filename` does not have to
  point to an existing file and can be an arbitrary string).

* `val parse_file : ?⁠mode:Pyast__.Pyparse.compile_mode -> string ->
  Ast.mod_`, which parses a file given its name.

Note that `Pyast.V*.Ast` is included in `Pyast.V*`, and in particular,
`Pyast.V*.Ast.mod_` and `Pyast.V*.mod_` both denote the type of a
Python module.

The abstract syntax trees are automatically generated from
[the ASDL grammar which is used in the CPython implementation](https://github.com/python/cpython/blob/main/Parser/Python.asdl "Python.asdl grammar file").
Conversions can be performed between AST versions
with the `Pyast.V*.To` functors: for instance, the module
`Pyast.V2_5.To (Pyast.V3_11_0)` provides a module containing functions
for converting all the non-terminal symbols of the grammar, and in
particular a function
`val mod_: Pyast.V2_5.mod_ -> Pyast.V3_11_0.mod_`.
These functions are automatically called by the functions `parse*`
mentioned above to convert the abstract syntax tree parsed by the
version of the Python interpreter available to the targeted version.
(The module corresponding to the version of the Python interpreter
is available through the function `Pyast.get_current_version`,
and more generally `Pyast.get_version` returns the first-class module
corresponding to a given version at run-time.)
