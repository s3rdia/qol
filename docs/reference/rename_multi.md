# Rename One Or More Variables

Can rename one or more existing variable names into the corresponding
new variable names in one go.

## Usage

``` r
rename_multi(data_frame, ...)
```

## Arguments

- data_frame:

  The data frame which contains the variable names to be renamed.

- ...:

  Pass in variables to be renamed in the form: "old_var" = "new_var".

## Value

Returns a data_frame with renamed variables.

## Examples

``` r
# Example data frame
my_data <- dummy_data(10)

# Rename multiple variables at once
new_names_df <- my_data |> rename_multi("sex"   = "var1",
                                        "age"   = "var2",
                                        "state" = "var3")
```
