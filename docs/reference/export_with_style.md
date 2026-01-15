# Export Data Frame With Style

`export_with_style()` prints a data frame as an individually styled
'Excel' table. Titles, footnotes and labels for variable names can
optionally be added.

## Usage

``` r
export_with_style(
  data_frame,
  titles = .qol_options[["titles"]],
  footnotes = .qol_options[["footnotes"]],
  var_labels = .qol_options[["var_labels"]],
  workbook = NULL,
  style = .qol_options[["excel_style"]],
  output = .qol_options[["output"]],
  print = .qol_options[["print"]],
  monitor = .qol_options[["monitor"]]
)
```

## Arguments

- data_frame:

  A data frame to print.

- titles:

  Specify one or more table titles.

- footnotes:

  Specify one or more table footnotes.

- var_labels:

  A list in which is specified which label should be printed for which
  variable instead of the variable name.

- workbook:

  Insert a previously created workbook to expand the sheets instead of
  creating a new file.

- style:

  A list of options can be passed to control the appearance of 'Excel'
  outputs. Styles can be created with
  [`excel_output_style()`](https://s3rdia.github.io/qol/reference/excel_output_style.md).

- output:

  The following output formats are available: excel and excel_nostyle.

- print:

  TRUE by default. If TRUE prints the output, if FALSE doesn't print
  anything. Can be used if one only wants to catch the output workbook.

- monitor:

  FALSE by default. If TRUE outputs two charts to visualize the
  functions time consumption.

## Value

Returns a formatted 'Excel' workbook.

## Details

`export_with_style()` is based on the 'SAS' procedure Proc Print, which
outputs the data frame as is into a styled table.

## See also

Creating a custom table style:
[`excel_output_style()`](https://s3rdia.github.io/qol/reference/excel_output_style.md),
[`modify_output_style()`](https://s3rdia.github.io/qol/reference/modify_output_style.md),
[`number_format_style()`](https://s3rdia.github.io/qol/reference/number_format_style.md),
[`modify_number_formats()`](https://s3rdia.github.io/qol/reference/modify_number_formats.md).

Global style options:
[`set_style_options()`](https://s3rdia.github.io/qol/reference/style_options.md),
[`set_variable_labels()`](https://s3rdia.github.io/qol/reference/style_options.md),
[`set_stat_labels()`](https://s3rdia.github.io/qol/reference/style_options.md).

Functions that can handle styles:
[`frequencies()`](https://s3rdia.github.io/qol/reference/frequencies.md),
[`crosstabs()`](https://s3rdia.github.io/qol/reference/crosstabs.md),
[`any_table()`](https://s3rdia.github.io/qol/reference/any_table.md).

## Examples

``` r
# Example data frame
my_data <- dummy_data(1000)

# Define style
set_style_options(column_widths = c(2, 15, 15, 15, 9))

# Define titles and footnotes. If you want to add hyperlinks you can do so by
# adding "link:" followed by the hyperlink to the main text.
set_titles("This is title number 1 link: https://cran.r-project.org/",
           "This is title number 2",
           "This is title number 3")

set_footnotes("This is footnote number 1",
              "This is footnote number 2",
              "This is footnote number 3 link: https://cran.r-project.org/")

# Print styled data frame
my_data |> export_with_style()

# Retrieve formatted workbook for further usage
wb <- my_data |> export_with_style()

# To save a table as xlsx file you have to set the path and filename in the
# style element
# Example files paths
table_file <- tempfile(fileext = ".xlsx")

# Note: Normally you would directly input the path ("C:/MyPath/") and name ("MyFile.xlsx").
set_style_options(save_path  = dirname(table_file),
                  file       = basename(table_file),
                  sheet_name = "MyTable")

my_data |> export_with_style()

# Manual cleanup for example
unlink(table_file)

# Global options are permanently active until the current R session is closed.
# There are also functions to reset the values manually.
reset_style_options()
reset_qol_options()
close_file()
```
