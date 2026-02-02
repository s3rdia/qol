# Display Cross Table of Two Variables

`crosstabs()` produces a cross table of two variables. Statistics can be
weighted sums, unweighted frequencies or different percentages.

## Usage

``` r
crosstabs(
  data_frame,
  rows,
  columns,
  show_total = TRUE,
  statistics = "sum",
  formats = c(),
  by = c(),
  weight = NULL,
  titles = .qol_options[["titles"]],
  footnotes = .qol_options[["footnotes"]],
  style = .qol_options[["excel_style"]],
  output = .qol_options[["output"]],
  na.rm = .qol_options[["na.rm"]],
  print_miss = .qol_options[["print_miss"]],
  print = .qol_options[["print"]],
  monitor = .qol_options[["monitor"]]
)
```

## Arguments

- data_frame:

  A data frame in which are the variables to tabulate.

- rows:

  The variable that appears in the table rows.

- columns:

  The variable that appears in the table columns.

- show_total:

  TRUE by default. Whether to print row and column totals or not.

- statistics:

  The user requested statistics.Available functions:

  - "sum" -\> Weighted and unweighted sum

  - "freq" -\> Unweighted frequency

  - "pct_row" -\> Weighted and unweighted row percentages

  - "pct_column" -\> Weighted and unweighted column percentages

  - "pct_total" -\> Weighted and unweighted percentages compared to the
    grand total

- formats:

  A list in which is specified which formats should be applied to which
  variables.

- by:

  Compute tables stratified by the expressions of the provided
  variables.

- weight:

  Put in a weight variable to compute weighted results.

- titles:

  Specify one or more table titles.

- footnotes:

  Specify one or more table footnotes.

- style:

  A list of options can be passed to control the appearance of 'Excel'
  outputs. Styles can be created with
  [`excel_output_style()`](https://s3rdia.github.io/qol/reference/excel_output_style.md).

- output:

  The following output formats are available: console (default), text,
  excel and excel_nostyle.

- na.rm:

  FALSE by default. If TRUE removes all NA values from the variables.

- print_miss:

  FALSE by default. If TRUE outputs all possible categories of the
  grouping variables based on the provided formats, even if there are no
  observations for a combination.

- print:

  TRUE by default. If TRUE prints the output, if FALSE doesn't print
  anything. Can be used if one only wants to catch the output data
  frame.

- monitor:

  FALSE by default. If TRUE, outputs two charts to visualize the
  functions time consumption.

## Value

Returns a data tables containing the results for the cross table.

## Details

`crosstabs()` is based on the 'SAS' procedure Proc Freq, which provides
efficient and readable ways to perform cross tabulations.

To create a cross table you only need to provide a variable for the rows
and columns. Nothing special about this. The real power comes into play,
when you output your tables as a fully styled 'Excel' workbook. Setting
up a custom, reusable style is as easy as setting up options like:
provide a color for the table header, set the font size for the row
header, should borders be drawn for the table cells yes/no, and so on.

You can not only output sums and frequencies, but also different
percentages, all set up in separate, evenly designed tables. For just a
quick overview, rather than fully designed tables, you can also just
output the tables in ASCII style format.

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

Creating formats:
[`discrete_format()`](https://s3rdia.github.io/qol/reference/formats.md)
and
[`interval_format()`](https://s3rdia.github.io/qol/reference/formats.md).

Functions that can handle formats and styles:
[`frequencies()`](https://s3rdia.github.io/qol/reference/frequencies.md),
[`any_table()`](https://s3rdia.github.io/qol/reference/any_table.md).

Additional functions that can handle styles:
[`export_with_style()`](https://s3rdia.github.io/qol/reference/export_with_style.md)

Additional functions that can handle formats:
[`summarise_plus()`](https://s3rdia.github.io/qol/reference/summarise_plus.md),
[`recode()`](https://s3rdia.github.io/qol/reference/recode.md),
[`recode_multi()`](https://s3rdia.github.io/qol/reference/recode.md),
[`transpose_plus()`](https://s3rdia.github.io/qol/reference/transpose_plus.md),
[`sort_plus()`](https://s3rdia.github.io/qol/reference/sort_plus.md)

## Examples

``` r
# Example data frame
my_data <- dummy_data(1000)

# Define titles and footnotes. If you want to add hyperlinks you can do so by
# adding "link:" followed by the hyperlink to the main text.
set_titles("This is title number 1 link: https://cran.r-project.org/",
           "This is title number 2",
           "This is title number 3")

set_footnotes("This is footnote number 1",
              "This is footnote number 2",
              "This is footnote number 3 link: https://cran.r-project.org/")

# Output cross tables
my_data |> crosstabs(age, sex)
my_data |> crosstabs(age, sex,
                     weight = "weight")

# Also works with characters
my_data |> crosstabs("age", "sex")
my_data |> crosstabs("age", "sex",
                     weight = "weight")

# Applying formats
age. <- discrete_format(
    "Total"          = 0:100,
    "under 18"       = 0:17,
    "18 to under 25" = 18:24,
    "25 to under 55" = 25:54,
    "55 to under 65" = 55:64,
    "65 and older"   = 65:100)

sex. <- discrete_format(
    "Total"  = 1:2,
    "Male"   = 1,
    "Female" = 2)

my_data |> crosstabs(age, sex,
                     formats   = list(age = age., sex = sex.))

# Split cross table by expressions of another variable
my_data |> crosstabs(age, sex, by = education)

# Compute different stats
my_data |> crosstabs(age, sex,
                     statistics = c("sum", "freq", "pct_row", "pct_column", "pct_total"))

# Get a list with two data tables for further usage
result_list <- my_data |> crosstabs(age, sex,
                                    formats = list(age = age., sex = sex.))

# Output in text file
my_data |> crosstabs(age, sex, output = "text")

# Output to Excel
my_data |> crosstabs(age, sex, output = "excel")

# Individual styling can also be passed directly
my_style <- excel_output_style(header_back_color = "0077B6",
                               font              = "Times New Roman")

my_data |> crosstabs(age, sex, output = "excel", style = my_style)

# To save a table as xlsx file you have to set the path and filename in the
# style element
# Example files paths
table_file <- tempfile(fileext = ".xlsx")

# Note: Normally you would directly input the path ("C:/MyPath/") and name ("MyFile.xlsx").
set_style_options(save_path  = dirname(table_file),
                  file       = basename(table_file),
                  sheet_name = "MyTable")

my_data |> crosstabs(age, sex, output = "excel")

# Manual cleanup for example
unlink(table_file)

# Global options are permanently active until the current R session is closed.
# There are also functions to reset the values manually.
reset_style_options()
reset_qol_options()
close_file()
```
