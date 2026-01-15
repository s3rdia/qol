# Add Empty Variables In A Given Range

Add empty variables to a data frame in the provided range. Basically
does in a data frame, what paste0("age", 1:10) does for a vector.

## Usage

``` r
add_variable_range(data_frame, var_range)
```

## Arguments

- data_frame:

  A data frame to add variables to.

- var_range:

  A range of variables to add, provided in the form:
  var_name1:var_name10.

## Value

Returns a data frame with added variables.

## Examples

``` r
# Example data frames
my_data <- dummy_data(100)

# Add variable range
my_data <- my_data |> add_variable_range(status1:status12)
```
