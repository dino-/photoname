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


## Building for release

The preferred method of building photoname for release is as an
[AppImage](https://appimage.org/)

You will need the AppDir tool [linuxdeploy](https://github.com/linuxdeploy/linuxdeploy)
and the AppImage plugin [linuxdeploy-plugin-appimage](https://github.com/linuxdeploy/linuxdeploy-plugin-appimage)
to be on your PATH.

First, prep the AppDir with one provided for this project like this:

    $ cp -r util/resources/AppDir .

Next, build and install with the included `install.hs`. It will default to
installing in the AppDir directory we created in the last step.

    $ ./util/install.hs

Finally, run `linuxdeploy` to complete the process of creating the AppImage

    $ linuxdeploy-x86_64.AppImage --appdir=AppDir --output=appimage

You should now have a binary named `photoname-x86_64.AppImage`.

If desired, this could be renamed to something like `photoname-3.4.AppImage` if
it's desireable to carry the version number.

These steps will likely be automated in the future.


## Contact

Dino Morelli <[dino@ui3.info](mailto:dino@ui3.info)>

Past Debian 64-bit packaging work was performed by Magnus Therning
