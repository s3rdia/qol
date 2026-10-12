# Lock In A Condition

Locks in a condition and stores it as a hidden attribute of the data
frame. This condition can be accessed by
[`if.()`](https://s3rdia.github.io/qol/reference/if_else.md),
[`else_if.()`](https://s3rdia.github.io/qol/reference/if_else.md),
[`else.()`](https://s3rdia.github.io/qol/reference/if_else.md),
[`where.()`](https://s3rdia.github.io/qol/reference/where..md) and
[`compute.()`](https://s3rdia.github.io/qol/reference/compute..md),
enabling these functions to work with an overarching condition. This
function can also be used to nest multiple overarching conditions.

`else_do()`: Checks for existing do_if() conditions and reverses the
condition of the last one.

`end_do()`: Drops the last stored condition.

`end_all_do()`: Drops all stored conditions.

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

Returns a data frame with the condition stored as an attribute.

## See also

The following functions can make use of the `do_if()` conditions:

Conditions:
[`if.()`](https://s3rdia.github.io/qol/reference/if_else.md),
[`else_if.()`](https://s3rdia.github.io/qol/reference/if_else.md),
[`else.()`](https://s3rdia.github.io/qol/reference/if_else.md)

Filter Data Frame:
[`where.()`](https://s3rdia.github.io/qol/reference/where..md)

Create new Variables:
[`compute.()`](https://s3rdia.github.io/qol/reference/compute..md)

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

# NOTE: Close the do-if-blocks with end_do() to remove the stored conditions
#       again.
```
