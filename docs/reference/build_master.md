# Build a Master Script From Folder

`build_master()` reads a given folder structure, which contains scripts,
and builds a master file. The file which gets written depends on the
selected layout.

## Usage

``` r
build_master(dir, master_name = "Master", author = "", layout = "compact")
```

## Arguments

- dir:

  The folder structure which contains the scripts to build upon.

- master_name:

  The file name which should be written.

- author:

  Authors name to be put in the header.

- layout:

  The layout which should be written to the master file. Available are:

  - "compact": The header and rebuilt master section followed by one
    script block which runs all scripts. (default)

  - "full": The whole folder structure with an overview, a section which
    runs all scripts and single blocks for every folder and script.

## Value

Returns the script as character vector and saves it as master file.

## Details

The function works with folder structures that look like this:

root/

     subfolder1/

         script1.R

         script2.R

         ....R

     subfolder2/

         script3.R

         script4.R

         ....R

     .../

         ....R

## See also

RStudio helper functions:
[`run_script()`](https://s3rdia.github.io/qol/reference/run_scripts.md),
[`run_folder()`](https://s3rdia.github.io/qol/reference/run_scripts.md),
[`run_project()`](https://s3rdia.github.io/qol/reference/run_scripts.md),
[`run_project_parallel()`](https://s3rdia.github.io/qol/reference/run_scripts.md)

## Examples

``` r
# Example export file paths
# NOTE: These tempfiles are only for the examples. In reality you just call the
# main function and put in your desired path and name directly.
temp_file <- tempfile(fileext = ".Rmd")

# Example master
build_master(dir         = dirname(temp_file),
             master_name = basename(temp_file))

# Example compact master
build_master(dir         = dirname(temp_file),
             master_name = basename(temp_file),
             layout      = "full")

# Manual cleanup for example
unlink(temp_file)
```
