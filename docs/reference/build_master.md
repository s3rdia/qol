# Build a Master Script From Folder

`build_master()` reads a given folder structure, which contains scripts,
and builds a master script as a markdown file.

## Usage

``` r
build_master(
  dir,
  master_name = "Master",
  author = "",
  with_structure = TRUE,
  with_run_all = TRUE,
  with_run_folder = TRUE
)
```

## Arguments

- dir:

  The folder structure which contains the scripts to build upon.

- master_name:

  The file name which should be written.

- author:

  Authors name to be put in the header.

- with_structure:

  Whether the folder structure as tree should be written to the master
  script.

- with_run_all:

  Whether a section, which let's the user run all scripts, should be
  written to the master script.

- with_run_folder:

  Whether a section, which let's the user run all scripts from a
  specific folder, should be written to the master script.

## Value

Returns the script as character vector and saves it as markdown file.

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

## Examples

``` r
# Example export file paths
# NOTE: These tempfiles are only for the examples. In reality you just call the
# main function and put in your desired path and name directly.
temp_file <- tempfile(fileext = ".rstheme")
file_name <- basename(tools::file_path_sans_ext(temp_file))

# Example master
build_master(dir         = dirname(temp_file),
             master_name = file_name)

# Manual cleanup for example
unlink(temp_file)
```
