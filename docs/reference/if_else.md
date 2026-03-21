# If - Else if - Else Statements

These functions make if statements more readable. Especially if an if
block becomes bigger it can be hard to read with multiple nested if_else
statements. With these new functions if blocks can be written like in
other languages with a clear and simpler structure. In addition not only
for one variable can a new value be assigned, but for multiple.

`if.()` always creates a new variable if the given variable name is not
part of the given data frame. If there already is a variable with the
given name, the existing values will be overwritten if the condition is
TRUE.

If no new variable is provided, `if.()` will select observations by the
given condition instead.

`else_if.()` only acts if there already is a variable with the given
name. Only NA values will get new values if condition is TRUE. The
existing values will not be overwritten.

`else.()` only acts if there already is a variable with the given name.
Sets every remaining NA in given variable to the given value.

## Usage

``` r
if.(data_frame, condition, ...)

else_if.(data_frame, condition, ...)

else.(data_frame, ...)
```

## Arguments

- data_frame:

  A data frame on which to apply an if statement.

- condition:

  The condition on which a value should be passed to a variable.

- ...:

  The Assignment of what should happen when condition becomes TRUE.

## Value

Returns a data frame with conditionally computed variables. If assigned
values are of different types a character variable will be returned.

## See also

The following functions can make use of the
[`do_if()`](https://s3rdia.github.io/qol/reference/do_if.md) filter
variables:

Conditions: `if.()`, `else_if.()`, `else.()`

Filter Data Frame:
[`where.()`](https://s3rdia.github.io/qol/reference/where..md)

## Examples

``` r
# Example data frame
my_data <- dummy_data(1000)

# Call function
new_df <- my_data |>
         if.(age < 18,             age_group = "under 18") |>
    else_if.(age >= 18 & age < 65, age_group = "18 to under 65") |>
    else.   (                      age_group = "65 and older")

# Or with multiple variables
new_df <- my_data |>
         if.(age < 18,             age_group = "under 18"      , age_num = 1L) |>
    else_if.(age >= 18 & age < 65, age_group = "18 to under 65", age_num = 2L) |>
    else.   (                      age_group = "65 and older",   age_num = 3L)

# NOTE: As in other languages the following if blocks won't produce the same result.
#       if.() will overwrite existing values, while else_if.() will not.
state_df <- my_data |>
         if.(state == 1, state_a = "State 1") |>
    else_if.(state < 11, state_a = "West") |>
    else.   (            state_a = "East")

state_df <- state_df |>
      if.(state == 1, state_b = "State 1") |>
      if.(state < 11, state_b = "West") |>
    else.(            state_b = "East")

# Add multiple variables based on single conditions
multi_df <- my_data |>
         if.(age < 18 & education == "low",                  var1 = 1,
                                                             var2 = TRUE,
                                                             var3 = "Category 1") |>
    else_if.(age >= 18 & education %in% c("middle", "high"), var1 = 2,
                                                             var2 = FALSE,
                                                             var3 = "Category 2") |>
    else.   (                                                var1 = 3,
                                                             var2 = FALSE,
                                                             var3 = "Category 3")

# Use if.() as a do-over-loop. In this kind of loop all vectors will be
# advanced one iteration at a time in parallel.
money    <- c("income", "expenses", "balance", "probability")
new_vars <- c("var1", "var2", "var3", "var4")
result   <- c(1, 2, 3, 4)

do_over_df <- my_data |>
      if.(money > 0, new_vars = result) |>
    else.(           new_vars = 0)

# It is also possible to select character expressions based on whether they
# start/end with or contain a certain text.
text_select_df <- my_data |>
    if.(income_class == "01.:",  start    = 1) |>
    if.(income_class == ":more", end      = 1) |>
    if.(education    == ":i:",   contains = 1)

# Select observations by condition instead of generating new variable
subset_df <- my_data |> if.(sex == 1)

# Select all non NA observations by variable
subset_df <- my_data |> if.(sex)

# All these functions can be used in a do_if() situation and are aware of
# overarching conditions.
do_if_df <- my_data |>
    do_if(state < 11) |>
          if.(age < 18, new_var = 1) |>
        else.(          new_var = 2) |>
    else_do() |>
          if.(age < 18, new_var = 3) |>
        else.(          new_var = 4) |>
    end_do()
```
