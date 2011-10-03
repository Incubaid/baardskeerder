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

Prelude
+++++++

.. code-block:: haskell

    import Prelude hiding (null)
    import Data.ByteString (ByteString)
    import Data.Int (Int32)
    import Data.IntSet (IntSet)
    import qualified Data.IntSet as IS
    import System.Posix (COff, Fd)

Considerations
++++++++++++++
In a database file, a commit entry marks the root of a B-Tree. Any commit or
node entry points to other entries in the file, but these will always be at a
lower offset (in this document, we'll refer to lower offsets as 'on the left',
and accordingly higher offsets will be 'on the right').

Every version of the tree is based on the previous version, which is stored
somewhere on its left. The differences between the previous version are stored
in-between the commit entry of the previous version, and the commit entry of
the current version.

All entries between two successive commit entry are referenced by the tree
rooted at the rightmost commit entry (these entries are its slab).

We assume a linear version history (no version branches).

A tree rooted at a given commit entry can only reference entries referenced by
its parent version (the version whose commit entry is closest on its left). The
easiest way to see is to consider how it'd know about entries unless it got
their offset from its parent tree.

Algorithm
+++++++++
We can compact a database file starting at a given commit entry, which is the
oldest entry whose tree will be retained. No older versions should be traversed
(including traversal during compaction). New versions can be created safely.

We'll need a couple of datatypes and utility actions:

.. code-block:: haskell

    type Key = ByteString
    type Value = ByteString

    type Offset = COff
    type Length = Int32

    data Entry = Commit Offset Length Offset
               | Leaf Offset Length Value
               | Node Offset Length [(Key, Offset)]

    getEntry :: Fd -> Offset -> IO Entry
    getEntry f o = undefined

    punch :: Fd -> Offset -> Length -> IO ()
    punch f o l = undefined

We'll need to pass a little bit of state between recursive calls. Here's the
definition:

.. code-block:: haskell

    data CompactState = CS Offset IntSet

Finally, we can implement compaction. The idea is we start at a root node
(pointed to by a commit entry), and add all offsets referred to to a set. Once
this is done, we can retrieve the largest offset from the set (this is the
offset of the rightmost entry still referenced) and punch a hole from
immediately after this rightmost entry until (but not including) the offset of
the entry we're currently handling.

Once this is done, we pop the rightmost offset from the set, and recurse using
this offset as starting point.

Whenever the set is empty, we're done and we can punch a hole from the
beginning of the file until the current offset.

.. code-block:: haskell

    fileStart :: Offset
    fileStart = 4096

    compact :: Fd -> Offset -> IO ()
    compact f o = do
        c <- getEntry f o
        case c of
            Commit _ _ r -> compact' f $ CS o (IS.singleton $ fromIntegral r)
            otherwise -> error "compact: not a commit entry"

    compact' :: Fd -> CompactState -> IO ()
    compact' f (CS n os) = do
        -- At this stage, s should never be empty
        let (h, os') = IS.deleteFindMax os
            h' = fromIntegral h
        e <- getEntry f h'
        (e', os'') <- case e of
            Leaf o l _ -> return (o + fromIntegral l, os')
            Node o l rs ->
                return (o + fromIntegral l,
                    foldr (IS.insert . fromIntegral) os' $ map snd rs)
            otherwise -> error "compact': invalid entry type"

        punch f e' $ fromIntegral (n - e')

        if (IS.null os'')
        then punch f fileStart $ fromIntegral (h' - fileStart)
        else compact' f (CS h' os'')

Optimizations
+++++++++++++
- Use the FIEMAP ioctl to punch holes only in ranges which are still backed by
  an extent

- Whenever an offset is added to the set in contained in the state, instruct
  the kernel to readahead the page on which this offset resides

- Only punch holes aligned to a page, and using a multiple of the page size as
  length

- When using getEntry, use some API which doesn't actually fetch a value when
  handling a leaf entry: all we need is the entry offset and length

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
