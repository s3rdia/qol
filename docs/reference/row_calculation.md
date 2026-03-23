# Perform Row Wise Calculations

Perform row wise calculations on numeric variables.

## Usage

``` r
row_calculation(data_frame, statistics, ..., round_digits = NULL)
```

## Arguments

- data_frame:

  A data frame in which are the values to be calculated.

- statistics:

  Available functions: "sum", "freq", "mean", "median", "mode", "min",
  "max".

- ...:

  Variable names of the value variables.

- round_digits:

  The number of decimal places the values should be rounded to.

## Value

Returns a numeric vector.

## Examples

``` r
# Example data frame
my_data <- data.frame(var1 = 1:5,
                      var2 = c(6, 7, 8, NA, 10),
                      var3 = 11:15)

# Calculate new variables
my_data[["sum"]]  <- my_data |> row_calculation("sum",  var1, var2, var3)
my_data[["mean"]] <- my_data |> row_calculation("mean", var1:var3)
```
