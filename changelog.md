5.0 (2020-11-26)

  * Updated Stackage resolver
  * Moved copyright date up to 2021
  * Moved Photoname.Opts module into the app src tree
  * Removed the useless tests for program help
  * Switched to optparse-applicative for arg parsing
  * cabalized projects no longer need a Setup.hs
  * Added the -a|--artist option
  * Changed lookup order of EXIF date tags
  * Added ability to parse the filename for date and time
  * Fixed broken smoke tests
  * Switched test suite to use Tasty
  * Attempt to copy if the hard link fails
  * Added new --copy switch and behavior
  * Moved AppImage desktop and icon into hsinstall tree
  * Added new --prefix switch and behavior
  * Output is now handled via hslogger
  * Now using newtype wrappers nearly everywhere


4.1 (2019-03-30)

  * Removed some AppImage desktop file categories


4.0 (2019-03-26)

  * All imports are now explicit
  * Added tests for several missing cases
  * Removed --old-style switch and behavior
  * Removed unnecessary Control.Applicative imports
  * Switched EXIF library from from exif to hsexif
  * Removed unused old-locale dep
  * Removed License and Author comments from source code
  * Moved copyright up to 2019
  * Redesign source code directory layout
  * Removed outdated TODO.md and doc/dev/notes files
  * Updated build to use hpack and hsinstall


3.4 (2018-10-09)

  * Added a new --version switch
  * Now supporting building to an AppImage
  * Moved copyright up to 2018


3.3 (2017-12-27)

  * A variety of build-related fixes were made


3.2 (2016-10-02)

  * Switched the build from cabal to stack
  * Added hsinstall installation script
  * Update copyright year
  * Various updates to the cabal file


3.1 (2015-06-28)

  * Removed ambiguous import of `System.Locale.defaultTimeLocale`
  * Updated to use `Data.Version` to fish the version out of cabal info
  * Replaced deprecated ErrorT with ExceptT
  * Added pragma for Control.Applicative import
  * Updated README and cabal file with current info
  * Moved copyright year up to 2015


3.0.1 (2012-11-01)

  * Updated test suite to build cleanly (without warnings) and
    run against modern cabal versions
  * Updated cabal file to modern cabal standards. Unit testing is
    now cabal-driven and we now fully support cabal sdist.
  * Fixed a problem where try is no longer available. Now importing
    Control.Exception to bring it into scope.
  * Adjusted usage to explain a change in how we find the EXIF
    date stamps
  * Moved copyright year up to 2012


3.0.0 (2011-05-08)

  * New naming scheme that uses date/time only, no serial. This
    is a BIG change but the old naming scheme is still available.
  * Parent directory is now a switch
  * Subdirectory path creation of year and day can be suppressed
  * User can specify an arbitrary suffix string for the new names
  * User can now put switches into a config file


2.3.0 (2010-01-25)

  * Problem fixed that had recently come up with the Exif
    library. Specifically, files with no EXIF data were causing
    an IO error to be thrown and processing to halt. (thanks to
    Josh Hoyt)
  * Other minor updates to things like copyright info and version
    number


2.2 (2009-06-07)

  * No changes in how the software behaves
  * Updated build-depends to be explicit about base version
    requirement
  * Minor update to BSD3 copyright notices to bring them up to 2009
  * Debian binary package of v2.2 available


2.1 (2008-10-11)

  * More big changes to the transformers used. No longer using
    WriterT. Now using ErrorT for error handling.
  * Changed to record-style data for the arg parsing.


2.0 (2008-03-05)

  * Upgraded to build under GHC 6.8.2
  * Major internal changes. Now using ReaderT and WriterT
  * Changed version numbering scheme to be more conventional. Recent
    versions of cabal were not amused with the strange version
    string.
  * This project now added to HackageDB


003 (2007-09-13)

  * Repairs for a build problem with GHC > 6.6 specifically related
    to depending on FilePath instead of filepath and now requiring
    import of Data.Time.Format


002 (2007-04-28)

  * Initial release
