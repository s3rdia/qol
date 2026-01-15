# Get Integer Length

Get the number of digits of an integer variable.

## Usage

``` r
get_integer_length(variable)
```

## Arguments

- variable:

  The integer variable from which to get the length.

## Value

Returns a vector with the number of digits places.

## Examples

``` r
# Example data frame
my_data <- dummy_data(100)

my_data[["age_length"]] <- get_integer_length(my_data[["age"]])
```
