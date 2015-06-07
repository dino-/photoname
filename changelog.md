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