# Dummy Data

The dummy data frame contains a few randomly generated variables like
year, sex, age, income and weight to test out functionalities. It can be
generated with the desired number of observations.

## Usage

``` r
dummy_data(
  no_obs = 25000,
  insert_na = TRUE,
  wide = FALSE,
  monitor = .qol_options[["monitor"]]
)
```

## Arguments

- no_obs:

  Number of observations.

- insert_na:

  TRUE by default. Inserts random NA values into variables.

- wide:

  FALSE by default. If TRUE returns the data in wide format where each
  row represents one household and person-level variables are suffixed
  with the person number (e.g. sex1, sex2, age1, age2).

- monitor:

  FALSE by default. If TRUE outputs two charts to visualize the
  functions time consumption.

## Value

Returns a dummy data table.

## Examples

``` r
# Long format dataset
my_data <- dummy_data(1000)

# Wide format dataset
# NOTE: Here 1000 is not the number of households (rows in the data frame),
#       but the number of persons in the data frame.
my_data_wide <- dummy_data(1000, wide = TRUE)
```
