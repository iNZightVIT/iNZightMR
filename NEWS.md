# iNZightMR 2.3.0

- refactor `calcmissing()` to return an object with print methods, instead of bizarre and confusing `print` and `final` arguments

# iNZightMR 2.2.7

- add argument 'split_string' to `substringsplit()` to allow splitting at spaces (words) or some other character

# iNZightMR 2.2.6

- `plotcombn()` prints a useful message if data contains no missing values (#26)

# iNZightMR 2.2.5

- specify `stringsAsFactors = TRUE` for upcoming R 4.0.0
- rownames are suppressed when printing output of calcmissing()

# iNZightMR 2.2.4

**Release date**: 25 February 2020

- Minor changes to pass CRAN checks
- Improved documentation
- Suppress printing of rownames in `calcmissing()` (but keep Total)

# iNZightMR 2.2-3

**Release date**: 27 October 2017

- Add LICENCE
- Fix bug in label orders in subsetted barplots

# iNZightMR 2.2-2

**Release date**: 06 June 2017

- Comment functions
- Pass CRAN checks

# iNZightMR 2.2-1

**Release date**: 19 November 2016

- Fix subsetting (slot 2) bug where numeric to categorical conversion had the wrong level ordering

# iNZightMR 2.2

**Release date**: 22 January 2016

- Mostly just changes to the `NAMESPACE`, which involved renaming of some functions.

# iNZightMR 2.1.1-1

**Release date**: 26 November 2015

- fix a bug where bar plot labels were printed alphabetically instead
  of in the order the bars themselves were drawn.

# iNZightMR 2.1.1

**Release date**: 19 October 2015

- remove `iNZightPlots` from imports, as this was causing installation issues

# iNZightMR 2.1

**Release date**: 10 October 2015

- fix a bug that gave an "Inverse" error message

# iNZightMR 2.0

**Release date**: 26 May 2015

- the new version of iNZightMR uses the latest iNZightPlots (>2.0) instead of its own

# iNZightMR 1.1

No user-level changes, however released to coincide with major update of iNZightVIT.

# iNZightMR 1.0

First release of new package.
Contains multiple response subset used for the `iNZight` package.
