# Get Variable Names Which Are Not Part Of The Given Vector

If you have stored variable names inside a character vector, this
function gives you the inverse variable name vector.

## Usage

``` r
inverse(data_frame, var_names)
```

## Arguments

- data_frame:

  The data frame from which to take the variable names.

- var_names:

  A character vector of variable names.

## Value

Returns the inverse vector of variable names compared to the given
vector.

## Examples

``` r
# Example data frame
my_data <- dummy_data(1000)

# Get variable names
var_names <- c("year", "age", "sex")
other_names <- my_data |> inverse(var_names)

# Can also be used to just get all variable names
all_names <- my_data |> inverse()
```
