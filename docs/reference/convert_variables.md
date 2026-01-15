# Convert Variables

`convert_numeric()` converts all given variables to numeric if possible.
If a variable contains none numerical values (not including NAs), the
variable will not be converted.

`convert_factor()` converts all given variables to factor.

## Usage

``` r
convert_numeric(data_frame, variables)

convert_factor(data_frame, variables)
```

## Arguments

- data_frame:

  A data frame containing variables to convert.

- variables:

  Variables from the data frame which should be converted.

## Value

`convert_numeric()` returns the same data frame with converted variables
where possible.

`convert_factor()` returns the same data frame with converted variables.

## Examples

``` r
# Convert variables in a data frame to numeric where possible
test_df <- data.frame(var_a = c(1, 2, 3, NA, 4, 5),
                      var_b = c(1, 2, "Hello", NA, 4, 5))

convert_df <- test_df |> convert_numeric(c("var_a", "var_b"))

# Convert variables in a data frame to factor
test_df <- data.frame(var_a = c(1, 2, 3, 4, 5),
                      var_b = c("e", "c", "a", "d", "b"))

convert_df <- test_df |> convert_factor("var_b")
```
