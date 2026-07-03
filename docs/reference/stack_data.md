# Stack Multiple Data Frames

Stacks multiple data frames and matches column names.

## Usage

``` r
stack_data(..., id = FALSE, compress = NULL, guessing_rows = 100)
```

## Arguments

- ...:

  Put in multiple data frames to stack them in the provided order.

- id:

  Adds an ID column to indicate the different data frames.

- compress:

  No compression by default. If compression receives any value,
  `stack_data()` will convert character variables to numeric or integer
  where possible. If set to "factor" all non numeric character variables
  will be converted to factors.

- guessing_rows:

  100 by default. `stack_data()` takes a sample of rows to determine of
  which type variables are.

## Value

Returns a stacked data frame.

## Examples

``` r
# Example data frames
my_data1 <- dummy_data(100)
my_data2 <- dummy_data(100)
my_data3 <- dummy_data(100)
my_data4 <- dummy_data(100)
my_data5 <- dummy_data(100)

# Stack data frames
stacked_df <- stack_data(my_data1,
                         my_data2,
                         my_data3,
                         my_data4,
                         my_data5)
```
