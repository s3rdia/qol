# Lock In A Condition

Creates a filter variable based on the given condition. This variable
can be accessed by
[`if.()`](https://s3rdia.github.io/qol/reference/if_else.md),
[`else_if.()`](https://s3rdia.github.io/qol/reference/if_else.md),
[`else.()`](https://s3rdia.github.io/qol/reference/if_else.md) and
[`where.()`](https://s3rdia.github.io/qol/reference/where..md), enabling
these functions to work with an overarching condition. This function can
also be used to nest multiple overarching conditions.

`else_do()`: Checks for existing filter variables and reverses the
condition of the last filter variable.

`end_do()`: Drops the last filter variable.

`end_all_do()`: Drops all filter variables.

## Usage

``` r
do_if(data_frame, condition)

else_do(data_frame)

end_do(data_frame)

end_all_do(data_frame)
```

## Arguments

- data_frame:

  A data frame on which to apply filters.

- condition:

  The condition lock in.

## Value

Returns a data frame with a new filter variable.

## See also

The following functions can make use of the `do_if()` filter variables:

Conditions:
[`if.()`](https://s3rdia.github.io/qol/reference/if_else.md),
[`else_if.()`](https://s3rdia.github.io/qol/reference/if_else.md),
[`else.()`](https://s3rdia.github.io/qol/reference/if_else.md)

Filter Data Frame:
[`where.()`](https://s3rdia.github.io/qol/reference/where..md)

## Examples

``` r
# Example data frame
my_data <- dummy_data(1000)

# Create a simple do-if-block
do_if_df <- my_data |>
    do_if(state < 11) |>
          if.(age < 18, new_var = 1) |>
        else.(          new_var = 2) |>
    else_do() |>
          if.(age < 18, new_var = 3) |>
        else.(          new_var = 4) |>
    end_do()

# do_if() can also be nested
do_if_df <- my_data |>
    do_if(state < 11) |>
        do_if(sex == 1) |>
              if.(age < 18, new_var = 1) |>
            else.(          new_var = 2) |>
        else_do() |>
              if.(age < 18, new_var = 3) |>
            else.(          new_var = 4) |>
        end_do() |>
    else_do() |>
        do_if(sex == 1) |>
              if.(age < 18, new_var = 5) |>
            else.(          new_var = 6) |>
        else_do() |>
              if.(age < 18, new_var = 7) |>
            else.(          new_var = 8) |>
        end_do() |>
    end_do()

# NOTE: Close the do-if-blocks with end_do() to remove the temporary logical
#       filter variables.

# Probably a logical filter variable is exactly what you want. In this case
# just run do_if() without closing the block.
logic_filter_df <- my_data |> do_if(state < 11)
```
