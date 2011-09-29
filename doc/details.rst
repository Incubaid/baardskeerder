Details
=======

Implementation Considerations
-----------------------------
Statistics
~~~~~~~~~~
Implement statistics collection and reporting from day 1 in the implementation.

Consistency / Safety Guarantees
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Decide on when/where to add f(data)sync(2) calls etc. Make this configurable?

Optimization Considerations
---------------------------
Common Prefix Elimination in Branches
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Value Compression
~~~~~~~~~~~~~~~~~
Pro: Storage decrease
Pro: Reduce IO blocks read
Con: Can't use splice(2)

Use fallocate(2)
~~~~~~~~~~~~~~~~
Pre-allocate extents before writing into them. Might have some performance
benefit and reduce fragmentation. Transaction abort using ftruncate(2) would
still work.

Random Thoughts
---------------
Metadata Pages
~~~~~~~~~~~~~~
Keep metadata (incl. pointer to some recent valid root) in 2 pages at
beginning of file (cfr. CouchDB), which are synced (e.g. using
sync_file_range(2)) safely.

Include Checksums
~~~~~~~~~~~~~~~~~
Enable consistency checks.

Store Value Length Inside Branches
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Storing pointers to leaf nodes as (offset, length) tuples removes a read operation (used to retrieve value length otherwise), and allows for splice(2)'d operations without any read(2) inside the leaf node.

Pro: Reduce # IO calls
Con: Some space increase. Using 24-bit lengths allows for 16MB values, is this sufficient? Consider Rice/Golomb coding?

Multi-Device Storage
~~~~~~~~~~~~~~~~~~~~
When using multiple storage devices, interleave leaf/branch writes across several devices (heads), making sure the amount of data written to each device is spread equally.

Pro: Performance
Con: Single disk failure results in complete database loss (not sure this is a real 'con', though -> same goes for single-disk databases)
