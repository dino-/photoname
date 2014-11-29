# photoname


## Synopsis

Rename photo image files based on EXIF shoot date (Haskell)


## Description

photoname is a command-line utility for renaming/moving photo image
files. The new folder location and naming are determined by the
photo shoot date information contained within the file's EXIF tags.


## Getting source

- Download the cabalized source package [from Hackage](http://hackage.haskell.org/package/photoname)
- Download the cabalized source tarball [from here](http://ui3.info/d/proj/photoname/photoname-3.0.1.tar.gz)
- epub-tools is available for Arch Linux [from the AUR](https://aur.archlinux.org/packages/photoname/)
- Get the source with darcs: `$ darcs get http://hub.darcs.net/dino/photoname`
- If you're just looking, [browse the source](http://hub.darcs.net/dino/photoname)

And once you have it, building the usual way:

>     $ cabal configure --enable-tests
>     $ cabal build
>     $ cabal test
>     $ cabal install


## Installing

Build and install with cabal-install:
  `$ cabal update ; cabal install photoname`


## Contact

Dino Morelli <[dino@ui3.info](mailto:dino@ui3.info)>

Past Debian 64-bit packaging work was performed by Magnus Therning
