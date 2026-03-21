# Filter Data Frame With Direct View

Filter observations and variables and directly view the result on
screen.

## Usage

``` r
where.(data_frame, condition = NULL, keep = NULL)
```

## Arguments

- data_frame:

  A data frame on which to apply filters.

- condition:

  The condition on which to filter observations.

- keep:

  The Variables to keep in the result data frame.

## Value

Returns a filtered data frame.

## See also

The following functions can make use of the
[`do_if()`](https://s3rdia.github.io/qol/reference/do_if.md) filter
variables:

Conditions:
[`if.()`](https://s3rdia.github.io/qol/reference/if_else.md),
[`else_if.()`](https://s3rdia.github.io/qol/reference/if_else.md),
[`else.()`](https://s3rdia.github.io/qol/reference/if_else.md)

Filter Data Frame: `where.()`

## Examples

``` r
# Example data frame
my_data <- dummy_data(1000)

# Get a quick filtered view
my_data |> where.(sex == 1 & age < 25,
                  c(sex, age, household_id, education))
```
