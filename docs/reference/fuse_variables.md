# Fuse Multiple Variables

When you have a situation where you have multiple variables with
different NA values that happen to be in different places (where one
variable has a value the other is NA and vice versa) you can fuse these
together to a single variable.

## Usage

``` r
fuse_variables(
  data_frame,
  new_variable_name,
  variables_to_fuse,
  drop_original_vars = TRUE
)
```

## Arguments

- data_frame:

  A data frame with variables to fuse.

- new_variable_name:

  The name of the new fused variable.

- variables_to_fuse:

  A vector with the variables that should be fused together.

- drop_original_vars:

  Whether to drop or keep the original values. TRUE by default.

## Value

Returns a data frame without the variables TYPE, TYPE_NR and DEPTH.

## Examples

``` r
# Example format
sex. <- discrete_format(
    "Total"  = 1:2,
    "Male"   = 1,
    "Female" = 2)

# Example data frame
my_data <- dummy_data(1000)

# Call function
all_possible <- my_data |>
    summarise_plus(class      = c(year, sex),
                   values     = c(income, probability),
                   statistics = c("sum", "mean", "freq"),
                   formats    = list(sex = "sex."),
                   weight     = weight,
                   nesting    = "all",
                   na.rm      = TRUE)

all_possible <- all_possible[DEPTH <= 1] |>
    fuse_variables("fusion", c("year", "sex"))

# NOTE: You can generally use this function to fuse variables. What is done in
#       multiple steps above can be achieved by just using nested = "single" in
#       summarise_plus.
single <- my_data |>
    summarise_plus(class      = c(year, sex),
                   values     = c(income, probability),
                   statistics = c("sum", "mean", "freq"),
                   formats    = list(sex = "sex."),
                   weight     = weight,
                   nesting    = "single",
                   na.rm      = TRUE)
```
