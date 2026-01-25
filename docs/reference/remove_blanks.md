# Remove Blanks

Removes leading and trailing blanks or both. Can also remove all blanks
from a character or normalize multiple blanks to single ones.

## Usage

``` r
remove_blanks(data_frame, variable, which = "all")
```

## Arguments

- data_frame:

  A data frame which contains the character variables from which blanks
  should be removed.

- variable:

  Variable name of the one from which to remove blanks.

- which:

  "all" by default. Can be "leading", "trailing", "trim", "normalize" or
  "all". Determines which blanks should be removed

## Value

Returns a character vector with removed blanks.

## See also

Other character manipulating functions:
[`concat()`](https://s3rdia.github.io/qol/reference/concat.md),
[`sub_string()`](https://s3rdia.github.io/qol/reference/sub_string.md)

## Examples

``` r
# Example data frame
my_data <- dummy_data(100)
my_data[["blanks"]] <- " This  is  a  test "

# Remove blanks
my_data[["leading"]]   <- my_data |> remove_blanks(blanks, which = "leading")
my_data[["trailing"]]  <- my_data |> remove_blanks(blanks, which = "trailing")
my_data[["trim"]]      <- my_data |> remove_blanks(blanks, which = "trim")
my_data[["all"]]       <- my_data |> remove_blanks(blanks, which = "all")
my_data[["normalize"]] <- my_data |> remove_blanks(blanks, which = "normalize")
```
