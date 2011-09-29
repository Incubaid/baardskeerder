Baardskeerder Database File Format
==================================
This is a preliminary proposal on the Baardskeerder on-disk database format for
entry slabs. The format attempts to find a balance between performance,
robustness, reliability and future-proofness.

Whenever an implementation can't be backwards-compatible, the version
identifier stored in metadatablocks (see below) should be bumped. Within a
database version, any implementation should be backwards-compatible.

Whenever an implementation comes by an entry type identifier or option flag it
doesn't know about, an error should be reported, and operations can't be
continued. As such, forward-compatibility is possible though not guaranteed.

Basic Overview
--------------
The database format contains 4 types of fields: a metadata page, a leaf entry,
a node entry, or a commit entry.

In the first 2 blocks of the file (where we consider a block to be
filesystem-dependent and fixed-size, e.g. 4096 bytes), 2 versions of metadata
are written. After the metadata blocks, serialized leaf, node or commit
entries are appended.

Whenever in this document a reference is made to a '64-bit file offset', this
should be interpreted as one 8-bit LSB unsigned integer spindle ID, and 56-bit
LSB unsigned integer offset in the file used on the specified spindle.

Entries
-------
Metadata
~~~~~~~~
A metadata block contains some database metadata, and is extended to fill
exactly one filesystem block of size B.

The metadata block starts with a fixed magic denoting a Baardskeerder database.
After this magic, a 1-byte version number is added. This should be 0x01 in the
initial version(s). This version number should only be changed if invasive
layout changes are made, since entries themselves contain encoding versioning
information as well.

Now follows an 8-bit LSB unsigned integer, containing the spindle ID of the
storage file. This should never change, and within one database no two storage
files should have the same spindle ID.

Next follows a 64bit LSB unsigned integer, which contains the offset of the
last valid commit entry at the time the block was written. If no commit entry
was available (i.e. the database was empty), this should be 0.

Unlike other offsets, this should be a real offset, no spindle ID should be
encoded.

Then follows a 32bit LSB unsigned int which works as a counter. It can safely
overflow. Whenever a metadata block is updated, the counter value is
incremented with 2.

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
offset of the last valid commit entry. If both metadata blocks are valid, the
one with the highest counter (i.e. the last one written, except for overflow
scenarios) is used. If one counter is significantly less than the other (i.e.
overflow occurred), the lowest is used.

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

    +--------------------------------------------------------------------------------------------+
    | BaArDsKeErDeR | 0x01 | 0x02 | 0xabcdef0123456789 | 0x4e834081 | bAaRdSkEeRdEr | 0xcc3232cc |
    |--------------------------------------------------------------------------------------------|
    | magic length  | 1    | 1    | 8                  | 4          | magic length  | 4          |
    +--------------------------------------------------------------------------------------------+

Leaf Entries
~~~~~~~~~~~~
Leaf entries contain values stored in the tree. Similar to node and commit
entries, they start with a 32 (or 24?) bit LSB unsigned integer denoting the
length of the entry, not including its own size, but including the checksum
size.

Then follows the identifier of leaf entries version 1 (0x01) in a single byte.
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

Node Entries
--------------
Node entries contain internal nodes of the tree. Similar to leaf entries
and commit entries, they start with a 32 (or 24?) bit LSB unsigned integer
denoting the length of the entry, not including its own size, but including
the checksum size.

Then follows the identifier of node entries version 1 (0x02) in a single
byte. After this first byte, another byte acts as a bitmap for settings. In
the first version, this will always be 0.

Then follows the number of (key, pointer) pairs stored inside the node entry,
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

Commit Entries
--------------
Whenever a slab has been written to all spindles, a commit entry should be
created. The commit entry starts with a 32 bit LSB unsigned integer containing
the length of the entry, similar to other entry formats. Then follows a single
byte denoting a commit entry (0x03 in the initial version).

Next comes an always-incrementing integer using var-int encoding. The value is
encoded in 32 bit LSB unsigned integers. The most significant bit denotes
whether there's more data to be added (1 if there is, 0 in the final 32 bit
value), and the lowest-value 32 bit integer comes first.

As such (in case the value encoding would be done in 8 bit integers, for
demonstration purposes) this data::

    0b01000000 == 64 * (128 ** 0) == 64
    0b01111111 == 127 * (128 ** 0) == 127
    0b10000000 0b00000000 == 0 * (128 ** 0) + 0 * (128 ** 1) == 0
    0b10000000 0b00000001 == 0 * (128 ** 0) + 1 * (128 ** 1) == 128
    0b10000010 0b00000101 == 2 * (128 ** 0) + 5 * (128 ** 1) == 642

Next comes a 64 bit LSB unsigned integer contains the offset of the node
entry containing the root of the B-tree.

Finally, 4-byte CRC32 checksum of all this data (including the length
specifier) is appended.

Diagram
~~~~~~~
::

    +-----------------------------------------------------------------------------+
    | 0x00000032 | 0x03 | 0x80000002 0x00000001 | 0xabcdef0123456789 | 0xCC3232CC |
    |-----------------------------------------------------------------------------|
    | 4          | 1    | variable              | 64                 | 4          |
    +-----------------------------------------------------------------------------+
