# photoname


## Synopsis

Rename photo image files based on EXIF shoot date (Haskell)


## Description

photoname is a command-line utility for renaming/moving photo image
files. The new folder location and naming are determined by the
photo shoot date information contained within the file's EXIF tags.


## Getting binaries

photoname is available for Linux in AppImage form [from github](https://github.com/dino-/photoname/releases)


## Getting source

- Get the source with git: `$ git clone https://github.com/dino-/photoname.git`
- Download the cabalized source package [from Hackage](http://hackage.haskell.org/package/photoname)

And once you have it, building the usual way:

    $ stack build
    $ stack run
    $ stack test --ta all
    $ stack clean

If you're just looking, [browse the source](https://github.com/dino-/photoname)


## Building for release

For Linux builds, I recommend using the excellent
[hsinstall](https://github.com/dino-/hsinstall) utility which will simplify
creation of an [AppImage](https://appimage.org/) executable.


## Contact

Dino Morelli <[dino@ui3.info](mailto:dino@ui3.info)>

Past Debian 64-bit packaging work was performed by Magnus Therning
