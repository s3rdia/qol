# Compute New Variables

Compute new variables without having to write the name of the data frame
multiple times. It can handle all kinds of operations like simple value
assignment or calculations, but also can make use of other functions.

In addition it can be used in a conditional
[`do_if()`](https://s3rdia.github.io/qol/reference/do_if.md) block.

## Usage

``` r
compute(data_frame, ..., monitor = .qol_options[["monitor"]])
```

## Arguments

- data_frame:

  A data frame in which to compute new variables.

- ...:

  The calculations that should be executed.

- monitor:

  FALSE by default. If TRUE, outputs two charts to visualize the
  functions time consumption.

## Value

Returns a data frame with newly computed variables.

## See also

The following functions can make use of the
[`do_if()`](https://s3rdia.github.io/qol/reference/do_if.md) filter
variables:

Conditions:
[`if.()`](https://s3rdia.github.io/qol/reference/if_else.md),
[`else_if.()`](https://s3rdia.github.io/qol/reference/if_else.md),
[`else.()`](https://s3rdia.github.io/qol/reference/if_else.md)

Filter Data Frame:
[`where.()`](https://s3rdia.github.io/qol/reference/where..md)

Create new Variables: `compute()`

## Examples

``` r
# Example data frame
my_data <- dummy_data(1000)

# Simple assignment
assign_df <- my_data |> compute(new_var1 = 1,
                                new_var2 = "Hello")

# Simple calculation
sum_df <- my_data |> compute(new_sum = age + sex)

# Using functions
mean_df <- my_data |> compute(new_mean = collapse::fmean(age))

# Using qol functions
qol_df <- my_data |> compute(row_sum = row_calculation("sum", state, age, sex))

# Use compute() as a do-over-loop. In this kind of loop all vectors will be
# advanced one iteration at a time in parallel.
money    <- c("income", "expenses", "balance")
new_vars <- c("var1", "var2", "var3")
multi    <- c(1, 2, 3)

do_over_df <- my_data |> compute(new_vars = multi * money)

# You can also do all at once
all_df <- my_data |> compute(new_var1 = 1,
                             new_var2 = "Hello",
                             new_sum  = age + sex,
                             new_mean = mean(age),
                             row_sum  = row_calculation("sum", state, age, sex),
                             new_vars = multi * money)

# compute() can be used in a do_if() situation and is aware of overarching
# conditions.
age_west. <- discrete_format("under 18"     = 0:17,
                             "18 and older" = 18:100)

age_east. <- discrete_format("under 65"     = 0:64,
                             "65 and older" = 65:100)

do_if_df <- my_data |>
    do_if(state < 11) |>
          compute(region    = "West",
                  age_group = recode(age = age_west.)) |>
    else_do() |>
          compute(region    = "East",
                  age_group = recode(age = age_east.)) |>
    end_do()
```
