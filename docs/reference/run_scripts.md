# Run 'R' Scripts Via File Dialog

Opens an RStudio file selection dialog to pick a single script/folder or
a whole folder structure and runs it after confirmation. The dialog
starts in the folder of the currently opened script in RStudio.

`run_script()`: Runs the selected script.

`run_folder()`: Runs all scripts in the selected folder.

`run_project()`: Runs all scripts found in a folder structure, including
all scripts within subfolders, in sequential order.

`run_project_parallel()`: Runs all scripts found in a folder structure,
including all scripts within subfolders. Folders, which are marked with
an "!" at the end of their folder name, are run as background jobs in
parallel, all other folders are run sequentially.

## Usage

``` r
run_script(path = NULL)

run_folder(path = NULL)

run_project(path = NULL)

run_project_parallel(path = NULL)
```

## Arguments

- path:

  The folder in which the file selection dialog should start. If left
  out, the folder of the currently opened script in RStudio is used.

## Value

Returns TRUE or FALSE.

## Details

Example structure for a larger project

Project folder

- Subfolder1

  - Script1.R

  - Script2.R

  - ...

- Subfolder2

  - ...

- ...

- Master.Rmd

If you open "Master.Rmd" in such a structure (the file can be completely
empty) and simply run one of the commands in the console, a selection
window opens which directly shows the path where the "Master.Rmd" file
is located. Depending on the command, you can then select and run either
a single script, all scripts in a folder, or all scripts in all folders
of the project at once.

Note: If the master file has the extension "\*.R", it is also picked up
when running the project folder with `run_project()`. If it also
contains code which runs files in the subfolders, things will be
executed twice. Therefore it is better to use a different file type or
to not add any code in it.

## See also

Build master file:
[`build_master()`](https://s3rdia.github.io/qol/reference/build_master.md)
