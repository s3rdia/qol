# Dummy Data

The dummy data frame contains a few randomly generated variables like
year, sex, age, income and weight to test out functionalities. It can be
generated with the desired number of observations.

## Usage

``` r
dummy_data(no_obs, monitor = .qol_options[["monitor"]])
```

## Arguments

- no_obs:

  Number of observations.

- monitor:

  FALSE by default. If TRUE outputs two charts to visualize the
  functions time consumption.

## Value

Returns a dummy data table.

## Examples

``` r
my_data <- dummy_data(1000)
```
