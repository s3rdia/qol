# Error Handling

`resolve_intersection()`: Compares if two vectors have intersecting
values. If TRUE, removes the intersection values from the base vector

`part_of_df()`: Check if variable names are part of a data frame. If
not, remove them from the given vector.

`remove_doubled_values()`: Remove values from a vector that appear more
than once.

`check_weight()`: Check if a weight variable was provided. If TRUE,
check whether it can be used else add a temporary weight variable.

## Usage

``` r
resolve_intersection(base, vector_to_check, check_only = FALSE)

part_of_df(data_frame, var_names, check_only = FALSE)

remove_doubled_values(var_names)

check_weight(data_frame, var_names)
```

## Arguments

- base:

  The base vector from which to remove any intersecting values.

- vector_to_check:

  The vector for which intersections should be checked.

- check_only:

  Returns a list of invalid entries instead of a vector. Additionally it
  doesn't throw a warning.

- data_frame:

  A data frame in which to look up variable names.

- var_names:

  A character vector of variable names.

## Value

Returns a vector or list.

## Examples

``` r
# Resolve intersection between two vectors
vec1 <- c("a", "b", "c", "d")
vec2 <- c("e", "f", "a", "g")

vec1 <- resolve_intersection(vec1, vec2)

# Check if variables are part of a data frame
my_data   <- dummy_data(100)
var_names <- c("year", "state", "age", "test")

var_names <- my_data |> part_of_df(var_names)

# Remove doubled values
var_names <- c("year", "state", "state", "age")

var_names <- remove_doubled_values(var_names)

# Check the provided weight variable
var_names <- my_data |> check_weight("weight")
```
