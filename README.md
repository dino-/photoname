# photoname


## Synopsis

Rename photo image files based on EXIF shoot date (Haskell)


## Description

photoname is a command-line utility for renaming/moving photo image
files. The new folder location and naming are determined by the
photo shoot date information contained within the file's EXIF tags.


## Getting source

- Download the cabalized source package [from Hackage](http://hackage.haskell.org/package/photoname)
- epub-tools is available for Arch Linux [from the AUR](https://aur.archlinux.org/packages/photoname/)
- Get the source with git: `$ git clone https://github.com/dino-/photoname.git`
- If you're just looking, [browse the source](https://github.com/dino-/photoname)

And once you have it, building the usual way:

    $ stack build
    $ stack test
    $ stack clean


## Installing

Build and install with the included `install.hs` script:
  `# ./util/install.hs --prefix=/some/dir`


## Contact

Dino Morelli <[dino@ui3.info](mailto:dino@ui3.info)>

Past Debian 64-bit packaging work was performed by Magnus Therning
