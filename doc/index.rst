.. Baardskeerder documentation master file, created by
   sphinx-quickstart on Thu Sep 29 14:52:44 2011.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

Welcome to Baardskeerder's documentation!
=========================================

Baardskeerder is a so-called Copy-on-Write B-tree (aka Append-only B-tree), a
fully-persistent datastructure which can be implemented using nothing but
append writes towards the storage medium on which the data is stored.

General Idea
------------
A CoW B-tree implements a fully-persistent B-tree_ datastructure by copying
paths from leaf to root whenever a given key is added (or altered) inside the
tree.

Fully-persistent is *not* related to persistent storage: it implies, whenever
you got a reference to a valid root of a tree, this reference will remain valid
and contain the same data, even when updates to the tree are made later on.
Check the `Wikipedia article`_ for more details.

An implementation will, for every update, calculate which internal nodes need
to be updated (starting at the new leaf, going up to the root node), then
append the new leaf and all changed internal nodes to a storage medium. Every
internal node points to other internal nodes inserted earlier in time, or a
leaf node, all of them at a lower offset ('on its left') in the storage
container.

.. _B-tree: http://en.wikipedia.org/wiki/B-tree
.. _Wikipedia article: http://en.wikipedia.org/wiki/Persistent_data_structure

Pros and Cons
-------------
Pros
~~~~
- Corruption Safety
- Transactions and Built-In Transaction Log

Cons
~~~~
- Space Overhead
- Time Overhead
- Compaction

Contents:

.. toctree::
   :maxdepth: 2

   details
   disk_format
   compaction

Indices and tables
==================

* :ref:`genindex`
* :ref:`modindex`
* :ref:`search`

