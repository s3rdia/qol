# Concatenate Multiple Variables With Padding

Concatenate multiple variables inside a data frame into a new variable.
An automatic or individual padding can be applied. The padding character
can be chosen freely.

The function can also be used to give a single variable a padding.

## Usage

``` r
concat(data_frame, ..., padding_char = NULL, padding_length = NULL)
```

## Arguments

- data_frame:

  A data frame which contains the the variables to concatenate.

- ...:

  The names of the variables to concatenate.

- padding_char:

  A single character which will be used to fill up the empty places.

- padding_length:

  A numeric vector containing the individual padding length per
  variable.

## Value

Returns a character vector.

## See also

Other character manipulating functions: Coming soon...

## Examples

``` r
# Example data frame
my_data <- dummy_data(100)

# Concatenate variables as provided
my_data[["id1"]] <- my_data |> concat(household_id, state, age)

# Concatenate variables with leading zeros. Each variable will
# receive an individual padding length according to their
# longest value.
my_data[["id2"]] <- my_data |> concat(household_id, state, age,
                                      padding_char = "0")

# Concatenate variables with individual character and lengths.
my_data[["id2"]] <- my_data |> concat(household_id, state, age,
                                      padding_char   = "_",
                                      padding_length = c(5, 3, 4))

# Padding a single variable in place
my_data[["state"]] <- my_data |> concat(state, padding_char = "0")
```
