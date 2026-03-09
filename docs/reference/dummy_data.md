# Dummy Data

The dummy data frame contains a few randomly generated variables like
year, sex, age, income and weight to test out functionalities. It can be
generated with the desired number of observations.

## Usage

``` r
dummy_data(
  no_obs = 25000,
  insert_na = TRUE,
  monitor = .qol_options[["monitor"]]
)
```

## Arguments

- no_obs:

  Number of observations.

- insert_na:

  TRUE by default. Inserts random NA values into variables.

- monitor:

  FALSE by default. If TRUE outputs two charts to visualize the
  functions time consumption.

## Value

Returns a dummy data table.

## Examples

``` r
my_data <- dummy_data(1000)
```
