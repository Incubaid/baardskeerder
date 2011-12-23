=============
Baardskeerder
=============
Baardskeerder is a so-called Copy-on-Write B-tree_ (aka Append-only B-tree),
a fully-persistent datastructure which can be implemented using nothing but
append writes towards the storage medium on which the data is stored.

A CoW B-tree implements a fully-persistent_ B-tree datastructure by copying
paths from leaf to root whenever a given key is added (or altered) inside the
tree.

This is a technology preview, so keep this in mind whenever thinking about
using this in a *real-world* application: the codebase is still in flux, the
on-disk database format will most certainly change (without backwards
compatibility) before the first version is released,...

.. _B-tree: http://en.wikipedia.org/wiki/B-tree
.. _fully-persistent: http://en.wikipedia.org/wiki/Persistent_data_structure

Getting Started
===============
Baardskeerder uses several features only provided by the Linux kernel, so it
can't (for now) be used on other platforms. Some features require a very recent
(>= 3.0) kernel and require specific file-system support, currently only
provided by `ext4`, `XFS` and `OCFS2`.

It's written in OCaml_, so you'll need a recent OCaml compiler on your system
(we develop using OCaml 3.12, earlier versions might or might not work). The
build system uses `ocamlbuild` and `ocamlfind`.

Finally, a working C compiler and up-to-date system header files are required.

To build the library, test executable and benchmark tool, execute `make` in the
`src` tree. Run `make install` to install the library on your system.

To render the documentation, you'll need Sphinx_ and its dependencies installed.
Execute `make html` in the `doc` tree to generate HTML output. Execute
`make help` to see a list of other output formats available.

.. _OCaml: http://caml.inria.fr/ocaml/
.. _Sphinx: http://sphinx.pocoo.org/

License
=======
Baardskeerder is available under the LGPL-3 license. See COPYING_ for more
information.

.. _COPYING: https://raw.github.com/Incubaid/baardskeerder/master/COPYING
