# Remove Objects From Memory By Name And Type

Remove objects by name or type to free up memory. Uses the base
[`rm()`](https://rdrr.io/r/base/rm.html) function but provides more
flexible ways to remove objects.

## Usage

``` r
free_memory(names = NULL, types = NULL, envir = .GlobalEnv)
```

## Arguments

- names:

  Object names to be removed.

- types:

  Object types to be removed.

- envir:

  The environment to remove the objects from.

## Value

Returns removed objects.

## Details

`free_memory()` is based on the 'SAS' function Proc Datasets. Among
other things this procedure is able to remove datasets from memory. But
not only by writing out the full file name, it is capable of looking for
datasets starting with, ending with or containing a certain text.
Additionally certain object types can be removed.

## Examples

``` r
# Example data frame
my_data <- dummy_data(1000)
```
