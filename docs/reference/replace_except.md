# Replace Patterns While Protecting Exceptions

Replaces a provided pattern with another, while protecting exceptions.
Exceptions can contain the given pattern, but won't be changed during
replacement.

## Usage

``` r
replace_except(vector, pattern, replacement, exceptions = NULL)
```

## Arguments

- vector:

  A vector containing the texts, where a pattern should be replaced.

- pattern:

  The pattern that should be replaced.

- replacement:

  The new pattern, which replaces the old one.

- exceptions:

  A character vector containing exceptions, which should not be altered.

## Value

Returns a vector with replaced pattern.

## Examples

``` r
# Vector, where underscores should be replaced
underscores <- c("my_variable", "var_with_underscores", "var_sum", "var_pct_total")

# Extensions, where underscores shouldn't be replaced
extensions <- c("_sum", "_pct_group", "_pct_total", "_pct_value", "_pct", "_freq_g0",
                "_freq", "_mean", "_median", "_mode", "_min", "_max", "_first",
                "_last", "_p1", "_p2", "_p3", "_p4", "_p5", "_p6", "_p7", "_p8", "_p9",
                "sum_wgt", "_sd", "_variance", "_missing")

# Replace
new_vector <- underscores |> replace_except("_", ".", extensions)
```
