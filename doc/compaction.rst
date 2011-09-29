Compaction
==========
Hole-Punching Approach
----------------------
This approach uses the ability of (some) filesystems to punch holes in existing
files (make them sparse), freeing up storage space. This is only available in
new Linux kernels (for ext4, this requires at least a 3.0 kernel). For
efficiency reasons, the filesystem should support the FIEMAP ioctl as well (or
the SEEK_DATA and SEEK_HOLE lseek options as added in Linux 3.1, although
almost no filesystems support these efficiently yet).

Step 0
~~~~~~
Start by creating a handle R to the root of the oldest version of the tree you
want to preserve. We can only  compact if no readers are using any older roots.
Newer roots (added once compaction was initiated) are fine.

Step 1
~~~~~~
We need to mark which nodes in the file need to be maintained. Create a bloom
filter of appropriate size (TODO or a more suitable data structure. A skiplist
could be used, since we can store the offsets (see later) in reverse sorted
order easily, or an IntSet implemented efficiently by e.g. using patricia trees
which can store the offsets of in-use nodes. False positives are OK.

Walk over the tree starting at root R. For every node encountered during this
promenade, add its offset to the filter.

While adding these offsets, also keep track of the lowest/smallest offset
(unless the set implementation allows to look up a minimal element
efficiently).

Next, create an empty list, the 'punch list' (PL).

Step 2
~~~~~~
Since we know the lowest offset at which useful data is available from step 1,
we can punch a hole from the  beginning of the file (after the metadata pages)
until this offset. Prepend this range to PL.

Optimization
++++++++++++

Punching a hole over this range could be expensive (to be investigated!) if
the range is mostly sparse already. We could enhance this by only punching
pages which are still allocated. Use the FIEMAP ioctl or lseek with
SEEK_HOLE/SEEK_DATA calls to figure out which pages are still in use, then
prepend these pages (in order) to PL as separate ranges.

Step 3
~~~~~~
Start the algorithm (from step 4) using the page in which the leftmost node is
located, with startOfPageEmpty being true and currentNode being the leftmost
node.

Step 4
~~~~~~
If currentNode is R (it shouldn't ever be higher), we're done.

Retrieve the offsets of all nodes in the current page, starting at currentNode.
Now check whether all these offsets are in the filter.

If all offsets are not in the bloom filter, and the leftmost node on the page
is at offset 0, retrieve the offset the rightmost node stored on the page (RO)
and the length of the node data (RL), then prepend a range from the beginning
of the page until (RO + RL) to PL. Recur into step 4 using (TODO)

If all offsets are not in the bloom filter, and startOfPageEmpty equals true,
retrieve the offset the rightmost node stored on the page (RO) and the length
of the node data (RL), then prepend a range from the beginning of the page
until (RO + RL) to PL.

If at least one offset is in the bloom filter, find the offset of the rightmost
node on the page (RO). If this is not in the bloom filter, find the length of
the rightmost node (RL). If the rightmost node spans multiple page (i.e. (RO +
RL) `div` pageSize > RO `div` pageSize), prepend a range starting at the
beginning of the next node, of length (RL - (pageSize - (RO `mod` pageSize)))
to PL, then seek to (RO + RL), and find the first non-zero byte in the file.
This is the beginning of a node (an implementation should assert this).

Unless this offset is larger than or equal to R (the position of the root at
which we started), recur into step 4 using this next node as currentNode, and
startOfPageEmpty being true. Otherwise, jump to step 5.

If the rightmost node offset is in the bloom filter, calculate the offset of
the next node. Use this next node as currentNode and set startOfPageEmpty to
false, then recur into step 4.

Optimization
++++++++++++
The FIEMAP mapping information or SEEK_DATA can be used to optimize the search
for the first non-null byte.

Step 5
~~~~~~
We reached a location >= the location of R, and are almost at the end of this
algorithm. All that's left to do is punching some holes.

Reverse PL, and rewrite it by merging adjecent ranges into larger ranges.
Finally, map over the rewritten PL and punch a hole in the database file for
every range encountered.

::

    collapse :: PunchList -> PunchList
    collapse [] = []
    collapse [a] = [a]
    collapse ((ao, al) : b@(bo, bl) : rest)
        | ao + al == bo = collapse ((ao, al + bl) : rest)
        | otherwise = (ao, al) : collapse (b : rest)

    test_collapse :: Bool
    test_collapse = 
        collapse [(0, 10), (11, 5), (16, 4), (20, 4), (30, 5), (35, 5), (41, 2)]
            == [(0, 10), (11, 13), (30, 10), (41, 2)]

Notes
~~~~~
Compatibility
+++++++++++++
This method depends on some rather bleeding-edge filesystem features.

Execution
+++++++++
This algorithm can be executed in parallel with other operations (even in
different process spaces), as long as no references exist to any root older
than R.

Multiple compactions can not run simultaneously.

Rewriting Approach
------------------
TODO
