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

## Examples

``` r
# Example data frame
my_data <- dummy_data(1000)

# G         et a quick filtered view
my_data |> where.(sex == 1 & age < 25,
                  c(sex, age, household_id, education))
```
