# Sort Data Frame Rows With Some Additions

Sort data frame rows by the provided variables. `sort_plus()` is also
able to preserve the current order of certain variables and only sort
other variables within this order. As another option one can sort a
variable with the help of formats, which can be used to e.g. sort a
character variable in another than alphabetical order without creating a
temporary variable just for sorting.

## Usage

``` r
sort_plus(
  data_frame,
  by,
  preserve = NULL,
  order = "ascending",
  formats = c(),
  na.last = TRUE
)
```

## Arguments

- data_frame:

  A data frame to summarise.

- by:

  A variable vector which contains the variables to sort by.

- preserve:

  A vector containing all variables which current order should be
  preserved.

- order:

  A vector containing the sorting order for each variable.
  'ascending'/'a' or 'descending'/'d' can be used. If there are less
  orders given than by variables provided, the last given sorting order
  will be used for the additional by variables.

- formats:

  A list in which is specified which formats should be used to sort
  certain variables.

- na.last:

  TRUE by default. Specifies whether NA values should come last or
  first.

## Value

Returns a sorted data table.

## Details

`sort_plus()` is just very loosely based on the 'SAS' procedure Proc
Sort. It tries to keep the simplicity, but with some added features.

## See also

Creating formats:
[`discrete_format()`](https://s3rdia.github.io/qol/reference/formats.md)
and
[`interval_format()`](https://s3rdia.github.io/qol/reference/formats.md).

Functions that also make use of formats:
[`frequencies()`](https://s3rdia.github.io/qol/reference/frequencies.md),
[`crosstabs()`](https://s3rdia.github.io/qol/reference/crosstabs.md),
[`any_table()`](https://s3rdia.github.io/qol/reference/any_table.md),
[`recode()`](https://s3rdia.github.io/qol/reference/recode.md),
[`recode_multi()`](https://s3rdia.github.io/qol/reference/recode.md),
[`transpose_plus()`](https://s3rdia.github.io/qol/reference/transpose_plus.md).

## Examples

``` r
# Example formats
education. <- discrete_format(
    "1" = "low",
    "2" = "middle",
    "3" = "high")

# Example data frame
my_data <- dummy_data(1000)

# Simple sorting
sort_df1 <- my_data |> sort_plus(by = c(state, sex, age))
sort_df2 <- my_data |> sort_plus(by    = c(state, sex, age),
                                 order = c("ascending", "descending"))

# Character variables will normally be sorted alphabetically. With the help
# of a format this variable can be sorted in a completely different way.
sort_df3 <- my_data |> sort_plus(by      = education,
                                 formats = list(education = education.))

# Preserve the order of the character variable, otherwise it couldn't stay in
# it's current order.
sort_df4 <- sort_df3 |> sort_plus(by       = age,
                                  preserve = education)
```
