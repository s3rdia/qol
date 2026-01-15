# Order Columns by Variable Name Patterns

Order variables in a data frame based on a pattern rather than whole
variable names. E.g. grab every variable that contains "sum" in it's
name and order them together so that they appear next to each other.

## Usage

``` r
setcolorder_by_pattern(data_frame, pattern)
```

## Arguments

- data_frame:

  The data frame to be ordered.

- pattern:

  The pattern which is used for ordering the data frame columns.

## Value

Returns a reordered data frame with the ordered variables at the end.

## Examples

``` r
# Example data frame
my_data <- dummy_data(1000)

# Summarise data
all_nested <- my_data |>
    summarise_plus(class      = c(year, sex),
                   values     = c(weight, income),
                   statistics = c("sum", "pct_group", "pct_total", "sum_wgt", "freq"),
                   weight     = weight,
                   nesting    = "deepest",
                   na.rm      = TRUE)

# Set a different column order
new_order <- all_nested |> setcolorder_by_pattern(c("pct", "freq", "sum"))
```
