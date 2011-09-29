Baardskeerder Database File Format
==================================
This is a preliminary proposal on the Baardskeerder on-disk database format.
The format attempts to find a balance between performance, robustness,
reliability and future-proofness.

Whenever an implementation can't be backwards-compatible, the version
identifier stored in metadatablocks (see below) should be bumped. Within a
database version, any implementation should be backwards-compatible.

Whenever an implementation comes by a node type identifier or option flag it
doesn't know about, an error should be reported, and operations can't be
continued. As such, forward-compatibility is possible though not guaranteed.

Basic Overview
--------------
The database format contains 4 types of fields: a metadata page, a leaf node,
a branch node, or a root pointer node.

In the first 2 blocks of the file (where we consider a block to be
filesystem-dependent and fixed-size, e.g. 4096 bytes), 2 versions of metadata
are written. After the metadata blocks, serialized leaf, branch or root nodes
are appended.

Nodes
-----
Metadata
~~~~~~~~
A metadata block contains some database metadata, and is extended to fill
exactly one filesystem block of size B.

The metadata block starts with a fixed magic denoting a Baardskeerder database.
After this magic, a 1-byte version number is added. This should be 0x01 in the
initial version(s). This version number should only be changed if invasive
layout changes are made, since nodes themselves contain encoding versioning
information as well.

Next follows a 64bit LSB unsigned integer, which contains the offset of the
last valid root node at the time the block was written. If no root node was
available (i.e. the database was empty), this should be 0.

Then follows a 32bit LSB unsigned int and 16bit LSB unsigned short, containing
the seconds and microseconds of the UNIX timestamp at which the metadata block
was generated.

Now follows another (though different) fixed magic.

Finally, a CRC32 checksum of all above data is appended using 4-byte LSB
encoding.

Metadata Role
+++++++++++++
The metadata is used to speed up startup time. At regular intervals, a new
metadata struct is written to the first 2 blocks in the file. Note the
metadata should be calculated twice, since we want the timestamps to differ.

Whenever a database is loaded, the metadata blocks are read, and their checksum
is validated. If both checksums are invalid, a heavyweight full recovery,
scanning the complete database file, is required, and the operation shouldn't
continue. If a single metadata block is valid, this one is used to retrieve the
offset of the last valid root node. If both metadata blocks are valid, the one
with the highest timestamp (i.e. the last one written) is used.

Writing metadata consists of:

* Syncing the database file to disk
* Write metadata block 1
* Sync metadata block 1 to disk
* Write metadata block 2
* Sync metadata block 2 to disk

The database needs to be synced before writing metadata block 1 (and 2)! We
can't piggyback a metadata sync on a data sync (write MD1; sync; write MD2;
sync), since write order isn't fixed on sync (as such a valid MD1 block could
be persisted to disk, pointing to an invalid data offset).

Diagram
+++++++

::

    +----------------------------------------------------------------------------------------------+
    | BaArDsKeErDeR | 0x01 | 0xabcdef0123456789 | 0x4e834081 | 0xb64d | bAaRdSkEeRdEr | 0xcc3232cc |
    |----------------------------------------------------------------------------------------------|
    | magic length  | 1    | 8                  | 4          | 2      | magic length  | 4          |
    +----------------------------------------------------------------------------------------------+

Leaf Nodes
~~~~~~~~~~
Leaf nodes contain values stored in the tree. Similar to branch and root nodes,
they start with a 32 (or 24?) bit LSB unsigned integer denoting the length of
the node, not including its own size, but including the checksum size. 

Then follows the identifier of leaf nodes version 1 (0x01) in a single byte.
After this first byte, another byte acts as a bitmap for settings (when using
compression, this could e.g. act to flag whether a value is compressed). In the
first version, this will always be 0.

Then follows the actual value bytes. Finally, a 32 bit LSB integer is appended
containing the CRC32 checksum of all the previous bytes (including the size
field!).

Diagram
~~~~~~~
::

    +----------------------------------------------------+
    | 0x00000016 | 0x01 | 0x00 | value_data | 0xCC3232CC |
    |----------------------------------------------------|
    | 4          | 1    | 1    | length     | 4          |
    +----------------------------------------------------+

Branch Nodes
------------
Branch nodes contain internal branches of the tree. Similar to leaf nodes and
root nodes, they start with a 32 (or 24?) bit LSB unsigned integer denoting the
length of the node, not including its own size, but including the checksum
size.

Then follows the identifier of branch nodes version 1 (0x02) in a single byte.
After this first byte, another byte acts as a bitmap for settings. In the first
version, this will always be 0.

Then follows the number of (key, pointer) pairs stored inside the branch node,
encoded as an 8-bit LSB unsigned integer.

Next come all (key, pointer) pairs. These are encoded as follows: first, a
32-bit LSB unsigned integer denotes the length of a key, followed by the key
bytes themselves. After these, the offset pointer is encoded as a 64-bit LSB
unsigned integer.

Finally, a 4-byte CRC32 checksum of all this data (including the length
specifier) is appended.

TODO Specify version 2 of this encoding, which allows for common prefix
elimination.
TODO Encode value length in (key, offset, length) tuples, so splice(2) can be
used efficiently?

Diagram
~~~~~~~
::

    +------------------------------------------------------------------------------------------------------------------------------------+
    | 0x00000032 | 0x02 | 0x00 | 0x02 | 0x00000003 | abc    | 0xabcdef0123456789 | 0x00000002 | de     | 0x9876543210fedcba | 0xCC3232CC |
    |------------------------------------------------------------------------------------------------------------------------------------|
    | 4          | 1    | 1    | 1    | 4          | length | 8                  | 4          | length | 8                  | 4          |
    +------------------------------------------------------------------------------------------------------------------------------------+

Root Nodes
----------
There are 2 ways to encode root nodes:

* Introduce a specific node type, containing nothing but a pointer to a root
  branch node
* Use one of the bitmap flags in a normal branch node to denote it can be used
  as a root node.

If the latter is a suitable solution, we don't need a specific encoding.
