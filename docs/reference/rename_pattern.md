# Replace Patterns Inside Variable Names

Replace a certain pattern inside a variable name with a new one. This
can be used if there are multiple different variable names which have a
pattern in common (e.g. all end in "\_sum" but start different), so that
there don't have to be multiple rename variable calls.

## Usage

``` r
rename_pattern(data_frame, old_pattern, new_pattern)
```

## Arguments

- data_frame:

  The data frame in which there are variables to be renamed.

- old_pattern:

  The pattern which should be replaced in the variable names.

- new_pattern:

  The pattern which should be set in place for the old one.

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

# Rename variables by repacing patterns
new_names <- all_nested |>
    rename_pattern("pct", "percent") |>
    rename_pattern("_sum", "")
```
