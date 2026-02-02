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
  print = TRUE,
  monitor = FALSE
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

- print:

  TRUE by default. If TRUE prints the output, if FALSE doesn't print
  anything. Can be used if one only wants to catch the combined
  workbook.

- monitor:

  FALSE by default. If TRUE outputs two charts to visualize the
  functions time consumption.

## Value

A fully styled workbook containing the provided tables.

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
           "This is title number 2",
           "This is title number 3")
set_footnotes("This is footnote number 1",
              "This is footnote number 2",
              "This is footnote number 3 link: https://cran.r-project.org/")

# Catch the output and additionally use the options:
# pint = FALSE and output = "excel_nostyle".
# This skips the styling and output part, so that the function runs faster.
# The styling is done later on.
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
                             na.rm      = TRUE)

set_style_options(sheet_name = "age_sex")

tab2 <- my_data |> any_table(rows       = "age",
                             columns    = "sex",
                             values     = weight,
                             statistics = "sum",
                             formats    = list(sex = sex., age = age.),
                             na.rm      = TRUE)

set_style_options(sheet_name = "edu_year")

tab3 <- my_data |> any_table(rows       = "education",
                             columns    = "year",
                             values     = weight,
                             statistics = "pct_group",
                             formats    = list(education = education.),
                             na.rm      = TRUE)

# Every of the above tabs is a list, which contains the data table, an unstyled
# workbook and the meta information needed for the individual styling. These
# tabs can be input into the following function, which reads the meta information,
# styles each table individually and combines them as separate sheets into a single workbook.
combine_into_workbook(tab1, tab2, tab3)

# Reset the global options afterwards
set_print(TRUE)
set_output("excel")
```
