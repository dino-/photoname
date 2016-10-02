- Allow user to specify permissions for newly-created dirs and
  files. Bonus points for parsing the octal codes everyone knows
  and loves.
- Add a `--force` switch to disregard existing links.
- Deal gracefully with unknown dates. This means not just putting
  pictures into the 0000/0000-00-00 directory, but making sure that
  the `_###` serial number part is incremented from the highest one
  in there.