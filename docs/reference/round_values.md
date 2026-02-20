# Round Values With Half Rounded Up

This function rounds values according to DIN 1333 (round half up).

## Usage

``` r
round_values(values, digits = 0)
```

## Arguments

- values:

  Numeric values to round.

- digits:

  The number of decimal places the values should be rounded to.

## Value

Returns rounded values

## Examples

``` r
round_numbers1 <- round_values(c(-0.5, -0.4, 0.1, 0.49, 0.5, 1.5, 2.5, 3.2))
round_numbers2 <- round_values(c(-0.5, -0.49, 0.17, 0.499, 0.51, 1.549, 2.51, 3.25),
                               digits = 1)
```
