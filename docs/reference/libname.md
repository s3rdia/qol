# Check If Path Exists And Retrieve Files

`libname()` checks if a given path exists and writes a message in the
console accordingly. Optional all files from the given path can be
retrieved as a named character vector.

## Usage

``` r
libname(path, get_files = FALSE)
```

## Arguments

- path:

  A folder path.

- get_files:

  FALSE by default. If TRUE returns a named character vector containing
  file paths.

## Value

Returns the given file path or a named character vector containing file
paths.

## Examples

``` r
my_path   <- libname("C:/My_Path/")
file_list <- libname("C:/My_Path/", get_files = TRUE)
```
