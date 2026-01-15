# Different Facets of Retain

These retain functions all have one thing in common: transferring a
value from one case to the next. What they make out of this
functionality can be quiet different. Therefor there is a function for
each different use case.

`running_number()` computes running numbers in a data frame. Without
specifying a by variable results in the row number. With by variable
computes the running number within each group of expressions.

`mark_case()` sets a flag for the first or last case within the provided
by group.

`retain_value()` retains the first value for all cases of the same group
and saves it into a new variable.

`retain_sum()` retains the summarised values for all cases of the same
group and saves it into a new variable.

`retain_variables()` orders the provided variables to the front or back
of the data frame. If a variable is not part of the data frame it will
be added with all NA values at the desired position.

## Usage

``` r
running_number(data_frame, var_name = "run_nr", by = NULL)

mark_case(data_frame, var_name = "first", by = NULL, first = TRUE)

retain_value(data_frame, values, var_name = "retain_value", by = NULL)

retain_sum(data_frame, values, var_name = "retain_sum", by = NULL)

retain_variables(data_frame, ..., order_last = FALSE)
```

## Arguments

- data_frame:

  The data frame in which to compute retained variables.

- var_name:

  The name of the newly created variable.

  retain_sum: One or multiple variables of which the sum should be
  retained.

- by:

  By group in which to compute the retained variable.

- first:

  `mark_case()`: If TRUE marks the first case within a group, otherwise
  the last case.

- values:

  retain_value: One or multiple variables of which a value should be
  retained.

  retain_sum: One or multiple variables of which their sum should be
  retained.

- ...:

  `retain_variables()`: Put in single variable names or variable ranges
  (var_name1:var_name10) which should be ordered to the front or back of
  the data frame. It is also possible to provide none existent variable
  names which will then be added to the data frame.

- retain_variables:

  `retain_variables()`: FALSE by default. If TRUE puts the variables at
  the end of the data frame instead of the beginning.

## Value

`running_number()`: Returns the data frame with a new variable
containing a running number.

`mark_case()`: Returns the data frame with a new variable marking first
or last cases.

`retain_value()`: Return the data frame with a new variable containing a
retained value.

`retain_sum()`: Return the data frame with a new variable containing a
retained sum.

`retain_sum()`: Return the data frame with a new variable containing a
retained sum.

## Details

The functions listed here are based on the 'SAS' function retain. On a
very basic level retain can do two things, depending on the position in
the 'SAS' code: It can either sort variables column wise or it can -
since it works row wise - remember a value from one row to the next. The
functions here concentrate on the second part.

Remembering a value from a previous observation offers multiple use
cases. E.g. always adding +1 to the previous case creates a running
number. Or if an observation knows the value of the previous one, it can
check whether it is of the same value or another, e.g. to mark first or
last cases within a group.

In it's simplest form it can remember a value from the first observation
and transfer it to all other observations.

All of these functions work on the whole data frame as well as on
groups, e.g. to transfer a value from the first person in a household to
all other persons of the same household.ame retain

## Examples

``` r
# Example data frame
my_data <- dummy_data(1000)

# Get row numbers
my_data <- my_data |> running_number()
my_data <- my_data |> running_number("row_number")

# Running number per variable expression
my_data <- my_data |> running_number(by = year)

# Mark first and last cases
my_data <- my_data |>
    mark_case(by = household_id) |>
    mark_case(var_name = "last", by = household_id, first = FALSE)

# Retain first value inside a group
my_data <- my_data |>
    retain_value(var_name = c("household_weight", "household_icome"),
                 value    = c(weight, income),
                 by       = c(state, household_id))

# Retain sum inside a group
my_data <- my_data |>
    retain_sum(var_name = c("weight_hh_sum", "icome_hh_sum"),
               values    = c(weight, income),
               by       = c(state, household_id))

# Retain columns inside data frame, which orders them to the front
my_data <- my_data |> retain_variables(age, sex, income)

# Retain columns inside data frame, but order them to the end.
# Variable ranges can also be used.
my_data <- my_data |> retain_variables(age:income, order_last = TRUE)

# Retain columns inside data frame and add new variables with all NA values
my_data <- my_data |> retain_variables(age, sex, income, status1:status5)

# You can also use the colon as a placeholder for any text
start1   <- my_data |> retain_variables("s:")   # Variable names start with "s"
end1     <- my_data |> retain_variables(":id")  # Variable names end with "id"
contain1 <- my_data |> retain_variables(":on:") # Variable names which contain "on"
```
