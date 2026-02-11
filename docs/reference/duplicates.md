# Check For Duplicate Variable Names

Checks for duplicate variable names in a data frame, e.g. AGE, age and
Age.

Counts the number of duplicated variables in a data frame. If a variable
appears three times, e.g. AGE, age and Age, the variable count will be
one.

## Usage

``` r
get_duplicate_var_names(data_frame)

get_duplicate_var_count(data_frame)
```

## Arguments

- data_frame:

  The data frame which variable names to check for duplicates.

## Value

`get_duplicate_var_names()`: Returns a vector of duplicate variable
names.

`get_duplicate_var_count()`: Returns the count of duplicated variables
as a numeric value.

## Examples

``` r
# Example data frame
my_data <- data.frame(age    = 1,
                      AGE    = 2,
                      Age    = 3,
                      sex    = 1)

dup_var_names <- my_data |> get_duplicate_var_names()

dup_var_count <- my_data |> get_duplicate_var_count()
```
