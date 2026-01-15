# Replace Statistic From Variable Names

Remove the statistic name from variable names, so that they get back
their old names without extension.

## Usage

``` r
remove_stat_extension(data_frame, statistics)
```

## Arguments

- data_frame:

  The data frame in which there are variables to be renamed.

- statistics:

  Statistic extensions that should be removed from the variable names.

## Value

Returns a data frame with renamed variables.

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

# Remove statistic extension
new_names <- all_nested |> remove_stat_extension("sum")
```
