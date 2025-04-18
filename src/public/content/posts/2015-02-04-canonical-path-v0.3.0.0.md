Recently there was the third major release of `CanonicalPath` library introducing some good stuff, but unfortunately breaking API (just a tiny little bit).

The new version is available on [Github](https://github.com/d12frosted/CanonicalPath), [Hackage](https://hackage.haskell.org/package/system-canonicalpath) and [Stackage](https://www.stackage.org/package/system-canonicalpath).

<!--more-->

- Remove `UnsafePath` data type. It was a synonym to `FilePath` and was confusing me sometimes, so I decided to cut it off.
- Rename `cpathToText` to `toText'`.
- Remove `pathToText` and `textToPath`.
- Export `toText` and `fromText` from `Filesystem.Path.CurrentOS`.
- Export `fromPrelude` and `toPrelude` functions.
- Improve performance (path canonicalization now is performed 1.6x faster than before). Now that I have tests and criterion, I can measure performance.
- Improve `canonicalPath` error messages. Most important - now it respects errors from `System.Directory.canonicalizePath`.
- Add tests.
- Add Travis support.
- Update documentation.

As always - suggestions and pull requests are welcome!
