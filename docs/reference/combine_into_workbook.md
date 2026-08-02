# Combine Multiple Tables Into One Workbook

Combines any number of tables created with
[`any_table()`](https://s3rdia.github.io/qol/reference/any_table.md)
into one workbook and styles them according to their meta information.

## Usage

``` r
combine_into_workbook(
  ...,
  file = NULL,
  output = "excel",
  style = excel_output_style(),
  table_of_contents = FALSE,
  toc_header = "Table of Contents",
  toc_sheet_name = "Contents",
  subheaders = list(),
  subheader_colors = c(),
  subheader_underline = FALSE,
  colored_tabs = FALSE,
  print = .qol_options[["print"]],
  monitor = .qol_options[["monitor"]]
)
```

## Arguments

- ...:

  Provide any number of result lists output by
  [`any_table()`](https://s3rdia.github.io/qol/reference/any_table.md).

- file:

  If NULL, opens the output as temporary file. If a filename with path
  is specified, saves the output to the specified path.

- output:

  The following output formats are available: excel and excel_nostyle.

- style:

  A list of options can be passed to control the appearance of the table
  of contents. Styles can be created with
  [`excel_output_style()`](https://s3rdia.github.io/qol/reference/excel_output_style.md).

- table_of_contents:

  Whether to create a table of contents.

- toc_header:

  The main header.

- toc_sheet_name:

  The table of contents sheet name.

- subheaders:

  A list of custom subheaders. The entry names are the actual subheaders
  to be displayed, while the values are the sheet names at which the
  subheaders start.

- subheader_colors:

  Subheader background colors. These colors will also be used to color
  the tabs, if the option is activated.

- subheader_underline:

  FALSE by default. If TRUE underlines the subheaders.

- colored_tabs:

  FALSE by default. If TRUE colors the tabs according to the subheader
  colors.

- print:

  TRUE by default. If TRUE prints the output, if FALSE doesn't print
  anything. Can be used if one only wants to catch the combined
  workbook.

- monitor:

  FALSE by default. If TRUE outputs two charts to visualize the
  functions time consumption.

## Value

A fully styled workbook containing the provided tables.

## See also

Creating a custom table style:
[`excel_output_style()`](https://s3rdia.github.io/qol/reference/excel_output_style.md),
[`modify_output_style()`](https://s3rdia.github.io/qol/reference/modify_output_style.md),
[`number_format_style()`](https://s3rdia.github.io/qol/reference/number_format_style.md),
[`modify_number_formats()`](https://s3rdia.github.io/qol/reference/modify_number_formats.md).

Global style options:
[`set_style_options()`](https://s3rdia.github.io/qol/reference/style_options.md),
[`set_labels()`](https://s3rdia.github.io/qol/reference/style_options.md).

Other global options:
[`set_titles()`](https://s3rdia.github.io/qol/reference/qol_options.md),
[`set_footnotes()`](https://s3rdia.github.io/qol/reference/qol_options.md),
[`set_print()`](https://s3rdia.github.io/qol/reference/qol_options.md),
[`set_monitor()`](https://s3rdia.github.io/qol/reference/qol_options.md),
[`set_na.rm()`](https://s3rdia.github.io/qol/reference/qol_options.md),
[`set_print()`](https://s3rdia.github.io/qol/reference/qol_options.md),
[`set_print_miss()`](https://s3rdia.github.io/qol/reference/qol_options.md),
[`set_output()`](https://s3rdia.github.io/qol/reference/qol_options.md).

Standalone table of contents:
[`create_table_of_contents()`](https://s3rdia.github.io/qol/reference/create_table_of_contents.md).

Creating formats:
[`discrete_format()`](https://s3rdia.github.io/qol/reference/formats.md)
and
[`interval_format()`](https://s3rdia.github.io/qol/reference/formats.md).

Functions that can handle formats and styles:
[`frequencies()`](https://s3rdia.github.io/qol/reference/frequencies.md),
[`crosstabs()`](https://s3rdia.github.io/qol/reference/crosstabs.md).

Additional functions that can handle styles:
[`export_with_style()`](https://s3rdia.github.io/qol/reference/export_with_style.md)

Additional functions that can handle formats:
[`summarise_plus()`](https://s3rdia.github.io/qol/reference/summarise_plus.md),
[`recode.()`](https://s3rdia.github.io/qol/reference/recode.md),
[`recode_multi()`](https://s3rdia.github.io/qol/reference/recode.md),
[`transpose_plus()`](https://s3rdia.github.io/qol/reference/transpose_plus.md),
[`sort_plus()`](https://s3rdia.github.io/qol/reference/sort_plus.md)

## Examples

``` r
# Example data frame
my_data <- dummy_data(1000)
my_data[["person"]] <- 1

# Formats
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

education. <- discrete_format(
    "Total"            = c("low", "middle", "high"),
    "low education"    = "low",
    "middle education" = "middle",
    "high education"   = "high")

# Define style
set_style_options(column_widths = c(2, 15, 15, 15, 9))

# Define titles and footnotes. If you want to add hyperlinks you can do so by
# adding "link:" followed by the hyperlink to the main text.
set_titles("This is title number 1 link: https://cran.r-project.org/",
           "This is title number 2 cell: W22",
           "This is title number 3 file: C:/MyFolder/MyFile.docx",
           "This is title number 4")
set_footnotes("This is footnote number 1 cell: W22",
              "This is footnote number 2 file: C:/MyFolder/MyFile.docx",
              "This is footnote number 3 link: https://cran.r-project.org/",
              "This is footnote number 4")

# Catch the output and additionally use the options:
# print = FALSE and output = "excel_nostyle".
# This skips the styling and output part, so that the function runs faster.
set_print(FALSE)
set_output("excel_nostyle")
set_style_options(sheet_name = "big table")

tab1 <- my_data |> any_table(rows       = c("sex + age", "sex", "age"),
                             columns    = c("year", "education + year"),
                             values     = weight,
                             statistics = c("sum", "pct_group"),
                             pct_group  = c("sex", "age", "education", "year"),
                             formats    = list(sex = sex., age = age.,
                                               education = education.),
                             na.rm      = TRUE,
                             print      = FALSE)

set_style_options(sheet_name = "age_sex")

tab2 <- my_data |> any_table(rows       = "age",
                             columns    = "sex",
                             values     = weight,
                             statistics = "sum",
                             formats    = list(sex = sex., age = age.),
                             na.rm      = TRUE,
                             print      = FALSE)

set_style_options(sheet_name = "data")

tab3 <- my_data |> export_with_style(print = FALSE)

# Every of the above tabs is a list, which contains the data table, an unstyled
# workbook and the meta information needed for the individual styling. These
# tabs can be input into the following function, which reads the meta information,
# styles each table individually and combines them as separate sheets into a single workbook.
combine_into_workbook(tab1, tab2, tab3)

# Add an automatically generated table of contents with custom styling
combine_into_workbook(tab1, tab2, tab3,
                      table_of_contents = TRUE,
                      subheaders        = list("First Subheader"  = "big table",
                                               "Second Subheader" = "data"),
                      subheader_colors  = c("FF0000", "00FF00", "0000FF"),
                      colored_tabs      = TRUE,
                      style             = excel_output_style(toc_header_font_size    = 20,
                                                             toc_subheader_font_size = 16))

# Reset the global options afterwards
set_print(TRUE)
set_output("excel")
```
