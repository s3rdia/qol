# Add Extensions to Variable Names

Renames variables in a data frame by adding the desired extensions to
the original names. This can be useful if you want to use pre summarised
data with
[`any_table()`](https://s3rdia.github.io/qol/reference/any_table.md),
which needs the value variables to have the statistic extensions.

## Usage

``` r
add_extension(data_frame, from, extensions, reuse = "none")
```

## Arguments

- data_frame:

  The data frame in which variables should gain extensions to their
  name.

- from:

  The position of the variable inside the data frame at which to start
  the renaming.

- extensions:

  The extensions to add.

- reuse:

  "none" by default, meaning only the provided extensions will be set.
  E.g. if there are two extensions provided, two variables will be
  renamed. If "last", the last provided extension will be used for every
  following variable until the end of the data frame. If "repeat", the
  provided extensions will be repeated from the first one for every
  following variable until the end of the data frame.

## Value

Returns a data frame with extended variable names.

## Examples

``` r
# Example data frame
my_data <- dummy_data(10)

# Add extensions to variable names
new_names1 <- my_data |> add_extension(5, c("sum", "pct"))
new_names2 <- my_data |> add_extension(5, c("sum", "pct"), reuse = "last")
new_names3 <- my_data |> add_extension(5, c("sum", "pct"), reuse = "alternate")
```
