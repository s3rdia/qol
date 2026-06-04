# Analyze R Scripts And Print Out Statistics

`code_statistics()` reads in a folder or entire folder structure, grabs
all 'R' script files and scans the contents for different patterns. It
then outputs a small report which shows of which parts the code
consists.

## Usage

``` r
code_statistics(paths, recursive = FALSE, output_per = "overall")
```

## Arguments

- paths:

  A full folder path in which there are 'R' scripts to be analyzed. Can
  be a single path or a vector of paths.

- recursive:

  FALSE by default. If TRUE scans the provided paths recursive for sub
  folders and the 'R' scripts within them.

- output_per:

  "overall" by default. Determines how detailed the results are printed.
  Valid options are "overall", "folder" or "file".

## Value

Returns the results as a data frame.

## Examples

``` r
# Run this function on a directory, actually containing R script files
# to see some results.
code_statistics(tempdir())
```
