# Set Global Print Option

`set_print()`: Set the print option globally for the tabulation and
export to Excel functions.

`get_print()`: Get the globally stored print option.

`set_monitor()`: Set the monitor option globally for the heavier
functions which are able to show how they work internally.

`get_monitor()`: Get the globally stored monitor option.

`set_na.rm()`: Set the na.rm option globally for each function which can
remove NA values.

`get_na.rm()`: Get the globally stored na.rm option.

`set_print_miss()`: Set the print_miss option globally for each function
which can display missing categories.

`get_print_miss()`: Get the globally stored print_miss option.

`set_output()`: Set the output option globally for each function that
can output results to "console", "text", "excel" or "excel_nostyle".

`get_output()`: Get the globally stored output option.

`set_titles()`: Set the titles globally for each function that can print
titles above the output table.

`get_titles()`: Get the globally stored titles.

`set_footnotes()`: Set the footnotes globally for each function that can
print footnotes above the output table.

`get_footnotes()`: Get the globally stored footnotes.

## Usage

``` r
set_print(...)

get_print()

set_monitor(...)

get_monitor()

set_na.rm(...)

get_na.rm()

set_print_miss(...)

get_print_miss()

set_output(...)

get_output()

set_titles(...)

get_titles()

set_footnotes(...)

get_footnotes()
```

## Arguments

- ...:

  `set_footnotes()`: Put in the footnotes that should appear below
  tables.

## Value

`set_print()`: Changed global print option.

`get_print()`: TRUE or FALSE.

`set_monitor()`: Changed global monitor option.

`get_monitor()`: TRUE or FALSE.

`set_na.rm()`: Changed global na.rm option.

`get_na.rm()`: TRUE or FALSE.

`set_print_miss()`: Changed global print_miss option.

`get_print_miss()`: TRUE or FALSE.

`set_output()`: Changed global output option.

`get_output()`: Current output option as character.

`set_titles()`: Changed global titles.

`get_titles()`: Current titles as character.

`set_footnotes()`: Changed global footnotes.

`get_footnotes()`: Current footnotes as character.

## Examples

``` r
set_print(FALSE)
set_print(TRUE)

get_print()

set_monitor(TRUE)
set_monitor(FALSE)

get_monitor()

set_na.rm(TRUE)
set_na.rm(FALSE)

get_na.rm()

set_print_miss(TRUE)
set_print_miss(FALSE)

get_print_miss()

set_output("excel")

get_output()

set_titles("This is title number 1 link: https://cran.r-project.org/",
           "This is title number 2",
           "This is title number 3")

get_titles()

set_footnotes("This is title number 1 link: https://cran.r-project.org/",
           "This is title number 2",
           "This is title number 3")

get_footnotes()
```
