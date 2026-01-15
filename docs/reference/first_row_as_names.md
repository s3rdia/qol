# Set First Data Frame Row As Variable Names

Sets the first row of a data frame as variable names and deletes it. In
case of NA, numeric values or empty characters in the first row, the old
names are kept.

## Usage

``` r
first_row_as_names(data_frame)
```

## Arguments

- data_frame:

  A data frame for which to set new variable names.

## Value

Returns a data frame with renamed variables.

## Examples

``` r
# Example data frame
my_data <- data.frame(
              var1 = c("id", 1, 2, 3),
              var2 = c(NA, "a", "b", "c"),
              var3 = c("value", 1, 2, 3),
              var4 = c("", "a", "b", "c"),
              var5 = c(1, 2, 3, 4))

my_data <- my_data |> first_row_as_names()
```
