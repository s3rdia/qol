# Get All Variable Names Between Two Variables

Get all the variable names inside a data frame between two variables
(including the provided ones) as a character vector

## Usage

``` r
vars_between(data_frame, from, to)
```

## Arguments

- data_frame:

  The data frame from which to take the variable names.

- from:

  Starting variable of variable range.

- to:

  Ending variable of variable range.

## Value

Returns a character vector of variable names.

## Examples

``` r
# Example data frame
my_data <- dummy_data(1000)

# Get variable names
var_names <- my_data |> vars_between(state, income)

# Get variable names in reverse order
vars_reverse <- my_data |> vars_between(income, state)

# If you only provide "from" or "to" you get all variable names from a point to
# the end or from the beginning to a given point.
vars_from <- my_data |> vars_between(state)
vars_to   <- my_data |> vars_between(to = state)

# Or just get all variable names
vars_all <- my_data |> vars_between()
```
