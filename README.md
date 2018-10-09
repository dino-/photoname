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
    $ stack exec photoname
    $ stack test
    $ stack clean

If you're just looking, [browse the source](https://github.com/dino-/photoname)


## Building for release

The preferred method of building photoname for release is as an
[AppImage](https://appimage.org/)

You will need the AppDir tool [linuxdeploy](https://github.com/linuxdeploy/linuxdeploy)
and the AppImage plugin [linuxdeploy-plugin-appimage](https://github.com/linuxdeploy/linuxdeploy-plugin-appimage).
These need to be set executable and can be on your PATH if you wish.

First, prep the AppDir with one provided for this project like this:

    $ cp -r util/resources/AppDir .

Next, build and install with the included `install.hs`. It will default to
installing in the AppDir directory we created in the last step.

    $ ./util/install.hs

Finally, run `linuxdeploy` to complete the process of creating the AppImage

    $ linuxdeploy-x86_64.AppImage --appdir=AppDir --output=appimage

You should now have a binary named `photoname-x86_64.AppImage`.

This binary could be renamed to something like `photoname-3.4.AppImage` if you
wish the version number to be explicit in the filename.

These steps will likely be automated in the future.


## Contact

Dino Morelli <[dino@ui3.info](mailto:dino@ui3.info)>

Past Debian 64-bit packaging work was performed by Magnus Therning
