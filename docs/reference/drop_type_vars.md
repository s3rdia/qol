# Drop automatically generated Variables

If
[`summarise_plus()`](https://s3rdia.github.io/qol/reference/summarise_plus.md)
is used with the nested options "all" or "single", three variables are
automatically generated: TYPE, TYPE_NR and DEPTH. With this functions
these variables are dropped.

## Usage

``` r
drop_type_vars(data_frame)
```

## Arguments

- data_frame:

  The data frame with automatically generated variables.

## Value

Returns a data frame without the variables TYPE, TYPE_NR and DEPTH.

## Examples

``` r
# Example format
sex. <- discrete_format(
    "Total"  = 1:2,
    "Male"   = 1,
    "Female" = 2)

# Example data frame
my_data <- dummy_data(1000)

# Call function
all_possible <- my_data |>
    summarise_plus(class      = c(year, sex),
                   values     = c(income, probability),
                   statistics = c("sum", "mean", "freq"),
                   formats    = list(sex = "sex."),
                   weight     = weight,
                   nesting    = "all",
                   na.rm      = TRUE) |>
    drop_type_vars()
```
