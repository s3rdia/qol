# Recode New Variables With Formats

Instead of writing multiple if-clauses to recode values into a new
variable, you can use formats to recode a variable into a new one.

## Usage

``` r
recode(data_frame, ...)

recode_multi(data_frame, ...)
```

## Arguments

- data_frame:

  A data frame which contains the the original variables to recode.

- ...:

  `recode()` Pass in the original variable name that should be recoded
  along with the corresponding format container in the form: variable =
  format.

  In `recode_multi()` multiple variables can be recoded in one go and
  multilabels can be applied. This overwrites the original variables and
  duplicates rows if multilabels are applied. In occasions were you want
  to use format containers to afterwards perform operations with other
  packages, you can make use of this principle with this function.

## Value

`recode()`: Returns a vector with recoded values.

`recode_multi()`: Returns a data frame with the newly recoded variable.

## Details

`recode()` is based on the 'SAS' function put(), which provides an
efficient and readable way, to generate new variables with the help of
formats.

When creating a format you can basically write code like you think: This
new category consists of these original values. And after that you just
apply these new categories to the original values to create a new
variable. No need for multiple if_else statements.

## See also

Creating formats:
[`discrete_format()`](https://s3rdia.github.io/qol/reference/formats.md)
and
[`interval_format()`](https://s3rdia.github.io/qol/reference/formats.md).

Functions that also make use of formats:
[`frequencies()`](https://s3rdia.github.io/qol/reference/frequencies.md),
[`crosstabs()`](https://s3rdia.github.io/qol/reference/crosstabs.md),
[`any_table()`](https://s3rdia.github.io/qol/reference/any_table.md),
[`summarise_plus()`](https://s3rdia.github.io/qol/reference/summarise_plus.md),
[`transpose_plus()`](https://s3rdia.github.io/qol/reference/transpose_plus.md),
[`sort_plus()`](https://s3rdia.github.io/qol/reference/sort_plus.md)

## Examples

``` r
# Example formats
age. <- discrete_format(
    "under 18"       = 0:17,
    "18 to under 25" = 18:24,
    "25 to under 55" = 25:54,
    "55 to under 65" = 55:64,
    "65 and older"   = 65:100)

# Example data frame
my_data <- dummy_data(1000)

# Call function
my_data[["age_group1"]] <- my_data |> recode(age = age.)

# Formats can also be passed as characters
my_data[["age_group2"]] <- my_data |> recode(age = "age.")

# Multilabel recode
sex. <- discrete_format(
    "Total"  = 1:2,
    "Male"   = 1,
    "Female" = 2)

income. <- interval_format(
    "Total"              = 0:99999,
    "below 500"          = 0:499,
    "500 to under 1000"  = 500:999,
    "1000 to under 2000" = 1000:1999,
    "2000 and more"      = 2000:99999)

# recode_multi() can not only apply multiple recodings, but it can also
# apply multilabels.
# NOTE: Recoding will always be in place. When applying multilabels the
#       result data frame will have more observations than before.
multi_data <- my_data |> recode_multi(sex = sex., income = income.)
```
