set_no_print(TRUE)

###############################################################################
# Suppressing some functions messages because they only output the information
# on how much time they took.
###############################################################################

dummy_df <- dummy_data(1000)

ifelse_df <- data.table::data.table(age  = c(10, 20, 30, 70, NA),
                                    sex  = c(1, 2, 1, 2, NA),
                                    name = c("Hello World", "Hello Again", "Hello", "World", NA))


# if. can convert values conditionally
test_df <- dummy_df |>
         if.(age < 18,             age_group = "under 18") |>
    else_if.(age >= 18 & age < 65, age_group = "18 to under 65") |>
    else.   (                      age_group = "65 and older")

expect_true("age_group" %in% names(test_df), info = "if. can convert values conditionally")
expect_true(all(c("under 18", "18 to under 65", "65 and older") %in% test_df[["age_group"]]), info = "if. can convert values conditionally")


# if. can handle vectors
test_df <- dummy_df |>
         if.(age < 18,             income_new = income) |>
    else_if.(age >= 18 & age < 65, income_new = income) |>
    else.   (                      income_new = income)

expect_true("income_new" %in% names(test_df), info = "if. can handle vectors")
expect_equal(test_df[["income_new"]], test_df[["income"]], info = "if. can handle vectors")


# if. doesn't overwrite existing values with NA
test_df           <- dummy_df
test_df[["var1"]] <- ifelse(test_df[["first_person"]] == 1, NA, 1)
test_df[["var2"]] <- 1
test_df           <- test_df |> if.(var1 == 1, var2 = 2)

expect_true(all(collapse::funique(test_df[["var2"]]) %in% c(1, 2)), info = "if. doesn't overwrite existing values with NA")
expect_true(!anyNA(test_df[["var2"]]), info = "if. doesn't overwrite existing values with NA")


# else_if. doesn't work without if.
test_df <- dummy_df |>
    else_if.(age >= 18 & age < 65, age_group = "18 to under 65")

expect_true(!"age_group" %in% names(test_df), info = "else_if. doesn't work without if.")
expect_warning(print_stack_as_messages("WARNING"), "No valid target variables found in data frame. For else_if. to work the",
               info = "else_if. doesn't work without if.")


# else. doesn't work without if.
test_df <- dummy_df |>
    else.(age_group = "65 and older")

expect_true(!"age_group" %in% names(test_df), info = "else. doesn't work without if.")
expect_warning(print_stack_as_messages("WARNING"), "No valid target variables found in data frame. For else_if. to work the",
               info = "else. doesn't work without if.")


# else_if. only alters NA values conditionally
test_df1 <- dummy_df |>
         if.(age >= 18 & age < 65, age_group = "under 18") |>
    else_if.(age >= 18 & age < 65, age_group = "18 to under 65")

test_df2 <- dummy_df |>
         if.(age < 0,              age_group = "under 18") |>
    else_if.(age >= 18 & age < 65, age_group = "18 to under 65")

expect_true("age_group" %in% names(test_df1), info = "else_if. only alters NA values conditionally")
expect_true("age_group" %in% names(test_df2), info = "else_if. only alters NA values conditionally")
expect_true(!"18 to under 65" %in% test_df1[["age_group"]], info = "else_if. only alters NA values conditionally")
expect_true(!"under 18" %in% test_df2[["age_group"]], info = "else_if. only alters NA values conditionally")
expect_true(NA %in% test_df2[["age_group"]], info = "else_if. only alters NA values conditionally")


# else. only alters every other NA value
test_df1 <- dummy_df |>
    collapse::fsubset(!is.na(age)) |>
      if.(age < 101, age_group = "under 100") |>
    else.(           age_group = "100 and older")

test_df2 <- dummy_df |>
    collapse::fsubset(!is.na(age)) |>
      if.(age < 0, age_group = "under 18") |>
    else.(         age_group = "18 and older")

expect_true("age_group" %in% names(test_df1), info = "else. only alters every other NA value")
expect_true("age_group" %in% names(test_df2), info = "else. only alters every other NA value")
expect_true(!"100 and older" %in% test_df1[["age_group"]], info = "else. only alters every other NA value")
expect_true(!NA %in% test_df2[["age_group"]], info = "else. only alters every other NA value")


# if. and else_if. are not the same
state_df_a <- dummy_df |>
         if.(state <= 3, state_a = "State 1-3") |>
    else_if.(state < 11, state_a = "West") |>
    else.   (            state_a = "East")

state_df_b <- dummy_df |>
      if.(state <= 3, state_b = "State 1-3") |>
      if.(state < 11, state_b = "West") |>
    else.(            state_b = "East")

expect_false(identical(state_df_a[["state_a"]], state_df_b[["state_b"]]),
             info = "if. and else_if. are not the same")


# if. converts NA values into format of follow up value
result_df <- dummy_df |>
         if.(state <= 3, var1 = NA, var2 = NA) |>
    else_if.(state >  3, var1 = 1,  var2 = "Hello")

expect_equal(collapse::funique(result_df[["var1"]])[-1], 1,       info = "if. converts NA values into format of follow up value")
expect_equal(collapse::funique(result_df[["var2"]])[-1], "Hello", info = "if. converts NA values into format of follow up value")


# if. can check for variable expressions starting with letter
letter_df <- dummy_df |> if.(education == "m:", edu = 1)
test_df   <- letter_df |> if.(edu)

expect_true(all(collapse::funique(letter_df[["edu"]]) %in% c(1, NA)), info = "if. can check for variable expressions starting with letter")
expect_true(collapse::funique(test_df[["education"]]) == "middle", info = "if. can check for variable expressions starting with letter")


# if. can check for variable expressions ending with letter
letter_df <- dummy_df |> if.(education == ":w", edu = 1)
test_df   <- letter_df |> if.(edu)

expect_true(all(collapse::funique(letter_df[["edu"]]) %in% c(1, NA)), info = "if. can check for variable expressions ending with letter")
expect_true(collapse::funique(test_df[["education"]]) == "low", info = "if. can check for variable expressions ending with letter")


# if. can check for variable expressions containing a letter
letter_df <- dummy_df |> if.(education == ":g:", edu = 1)
test_df   <- letter_df |> if.(edu)

expect_true(all(collapse::funique(letter_df[["edu"]]) %in% c(1, NA)), info = "if. can check for variable expressions containing a letter")
expect_true(collapse::funique(test_df[["education"]]) == "high", info = "if. can check for variable expressions containing a letter")


# if. can check for variable expressions starting with letter (unequal)
letter_df <- dummy_df |> if.(education != "m:", edu = 1)
test_df   <- letter_df |> if.(edu)

expect_true(all(collapse::funique(letter_df[["edu"]]) %in% c(1, NA)), info = "if. can check for variable expressions starting with letter (unequal)")
expect_true(!"middle" %in% collapse::funique(test_df[["education"]]), info = "if. can check for variable expressions starting with letter (unequal)")


# if. can check for variable expressions ending with letter (unequal)
letter_df <- dummy_df |> if.(education != ":w", edu = 1)
test_df   <- letter_df |> if.(edu)

expect_true(all(collapse::funique(letter_df[["edu"]]) %in% c(1, NA)), info = "if. can check for variable expressions ending with letter (unequal)")
expect_true(!"low" %in% collapse::funique(test_df[["education"]]), info = "if. can check for variable expressions ending with letter (unequal)")


# if. can check for variable expressions containing a letter (unequal)
letter_df <- dummy_df |> if.(education != ":g:", edu = 1)
test_df   <- letter_df |> if.(edu)

expect_true(all(collapse::funique(letter_df[["edu"]]) %in% c(1, NA)), info = "if. can check for variable expressions containing a letter (unequal)")
expect_true(!"high" %in% collapse::funique(test_df[["education"]]), info = "if. can check for variable expressions containing a letter (unequal)")


# if. as do over loop
vars1  <- c("income", "balance")
vars2  <- c("VAR1", "VAR2")
values <- c(1, 2)

do_over_df <- dummy_df |> if.(vars1 > 0, vars2 = values)

expect_true(all(c("VAR1", "VAR2") %in% names(do_over_df)), info = "if. as do over loop")
expect_true(all(collapse::funique(do_over_df[["VAR1"]]) %in% c(1, NA)), info = "if. as do over loop")
expect_true(all(collapse::funique(do_over_df[["VAR2"]]) %in% c(2, NA)), info = "if. as do over loop")

do_over_df <- do_over_df |> else_if.(vars1 > 500, vars2 = 3)
expect_true(all(collapse::funique(do_over_df[["VAR1"]]) %in% c(1, 3, NA)), info = "if. as do over loop")
expect_true(all(collapse::funique(do_over_df[["VAR2"]]) %in% c(2, 3, NA)), info = "if. as do over loop")

do_over_df <- do_over_df |> else.(vars2 = 4)
expect_true(all(collapse::funique(do_over_df[["VAR1"]]) %in% c(1, 3, 4)), info = "if. as do over loop")
expect_true(all(collapse::funique(do_over_df[["VAR2"]]) %in% c(2, 3, 4)), info = "if. as do over loop")


# if. as do over loop doesn't overwrite existing values with NA
vars   <- c("var2", "var2")
values <- c(2, 3)

test_df           <- dummy_df
test_df[["var1"]] <- ifelse(test_df[["first_person"]] == 1, NA, 1)
test_df[["var2"]] <- 1
test_df           <- test_df |> if.(var1 == 1, vars = values)

expect_true(all(collapse::funique(test_df[["var2"]]) %in% c(1, 3)), info = "if. as do over loop doesn't overwrite existing values with NA")
expect_true(!anyNA(test_df[["var2"]]), info = "if. as do over loop doesn't overwrite existing values with NA")


# if. as do over loop can handle different values in the same variable
dummy_df <- dummy_data(1000)
class_df <- dummy_df |>
         if.(income <     1,                 income1 = 0) |>
    else_if.(income >=    1 & income < 4000, income1 = 1) |>
    else_if.(income >= 4000 & income < 5000, income1 = 2) |>
    else_if.(income >= 5000,                 income1 = 3)

lower_bound <- c(   1, 4000)
upper_bound <- c(4000, 5000)
values      <- c(   1,    2)

do_over_df <- class_df |>
         if.(income <  1,                                  income2 = 0) |>
    else_if.(income >= lower_bound & income < upper_bound, income2 = values) |>
    else_if.(income >= 5000,                               income2 = 3)

expect_equal(do_over_df[["income1"]], do_over_df[["income2"]],
             info = "if. as do over loop can handle different values in the same variable")


# do_if blocks
test_df <- dummy_df |>
    do_if(sex == 1) |>
             if.(age < 18,             age_group = 1) |>
        else_if.(age >= 18 & age < 65, age_group = 2) |>
        else.   (                      age_group = 3) |>
    else_do() |>
             if.(age < 18,             age_group = 4) |>
        else_if.(age >= 18 & age < 65, age_group = 5) |>
        else.   (                      age_group = 6) |>
    end_do()

  male_df <- test_df |> collapse::fsubset(sex == 1)
female_df <- test_df |> collapse::fsubset(sex == 2)

expect_true( all(collapse::funique(male_df[["age_group"]]) %in% c(1, 2, 3)), info = "do_if blocks")
expect_true(!all(collapse::funique(male_df[["age_group"]]) %in% c(4, 5, 6)), info = "do_if blocks")
expect_true( all(collapse::funique(female_df[["age_group"]]) %in% c(4, 5, 6)), info = "do_if blocks")
expect_true(!all(collapse::funique(female_df[["age_group"]]) %in% c(1, 2, 3)), info = "do_if blocks")


# do_if blocks with end_all_do
test_df <- dummy_df |>
    do_if(sex == 1) |>
    end_all_do()

expect_equal(test_df, dummy_df, info = "do_if blocks with end_all_do")


# if. can do all kinds of calculations
variables  <- c("NEW_VAR1", "NEW_VAR2")
values     <- c(1, 2)

result_df <- dummy_df |> if.(state < 11, sum       = state + age,
                                         col_sum   = collapse::fsum(age),
                                         row_sum   = row_calculation("sum", state, age),
                                         var1      = 1,
                                         var2      = "Hello",
                                         variables = values) |>
                   else_if.(state == 11, sum       = state + age,
                                         col_sum   = collapse::fsum(age),
                                         row_sum   = row_calculation("sum", state, age),
                                         var1      = 1,
                                         var2      = "Hello",
                                         variables = values) |>
                                   else.(sum       = state + age,
                                         col_sum   = collapse::fsum(age),
                                         row_sum   = row_calculation("sum", state, age),
                                         var1      = 1,
                                         var2      = "Hello",
                                         variables = values)

expect_true(all(c("sum", "col_sum", "row_sum", "var1", "var2", "NEW_VAR1", "NEW_VAR2") %in% names(result_df)),
            info = "if. can do all kinds of calculations")


# if. and else_if. work with parsed character conditions
vars1  <- c("income", "balance")
vars2  <- c("VAR1", "VAR2")
values <- c(1, 2)

test_df <- dummy_df |>
         if.("15 <= age < 65",        age_group = "15 to under 65") |>
    else_if.("age < 15 or age >= 65", age_group = "other") |>
         if.("vars1 > 0",             vars2     = values)

expect_true("age_group" %in% names(test_df), info = "if. and else_if. work with parsed character conditions")
expect_true(all(c("15 to under 65", "other", NA) %in% test_df[["age_group"]]), info = "if. and else_if. work with parsed character conditions")

expect_true(all(c("VAR1", "VAR2") %in% names(test_df)), info = "if. and else_if. work with parsed character conditions")
expect_true(all(collapse::funique(test_df[["VAR1"]]) %in% c(1, NA)), info = "if. and else_if. work with parsed character conditions")
expect_true(all(collapse::funique(test_df[["VAR2"]]) %in% c(2, NA)), info = "if. and else_if. work with parsed character conditions")


# else_if. throws a warning in do over loop if variables are missing in data frame
vars1  <- c("income", "balance")
vars2  <- c("VAR1", "VAR2")
values <- c(1, 2)

test_df <- dummy_df |>
    if.("15 <= age < 65", age_group = "15 to under 65") |>
    else_if.("vars1 > 0", vars2     = values)

expect_warning(print_stack_as_messages("WARNING"), "No valid target variables found in data frame. For else_if. to work the",
               info = "else_if. throws a warning in do over loop if variables are missing in data frame")

###############################################################################
# if. for subsetting
###############################################################################

# Subset data frame with if.
test_df <- dummy_df |> if.(sex == 1)

expect_true(collapse::fnrow(test_df) < collapse::fnrow(dummy_df), info = "Subset data frame with if.")
expect_true(!2 %in% test_df[["sex"]], info = "Subset data frame with if.")


# Subset data frame with if. (unequal)
test_df <- dummy_df |> if.(sex != 1)

expect_true(collapse::fnrow(test_df) < collapse::fnrow(dummy_df), info = "Subset data frame with if. (unequal)")
expect_true(!1 %in% test_df[["sex"]], info = "Subset data frame with if. (unequal)")


# Subset data frame with if., when only providing single variable
test_df <- dummy_df |> if.(sex)

expect_true(collapse::fnrow(test_df) < collapse::fnrow(dummy_df), info = "Subset data frame with if., when only providing single variable")
expect_true(!NA %in% test_df[["sex"]], info = "Subset data frame with if., when only providing single variable")


# Subset data frame with if., when only providing single variable as character
test_df <- dummy_df |> if.("sex")

expect_true(collapse::fnrow(test_df) < collapse::fnrow(dummy_df), info = "Subset data frame with if., when only providing single variable as character")
expect_true(!NA %in% test_df[["sex"]], info = "Subset data frame with if., when only providing single variable as character")


# Delete observations with 'delete' keyword
test_df <- dummy_df |> if.(sex == 1, delete)

expect_true(collapse::fnrow(test_df) < collapse::fnrow(dummy_df), info = "Delete observations with 'delete' keyword")
expect_true(!1 %in% test_df[["sex"]], info = "Delete observations with 'delete' keyword")


# if. as do over loop for subsetting
vars    <- c("income", "balance")
values1 <- c(1000, 0)
values2 <- c("low", "high")
values3 <- FALSE

test_df <- dummy_df
test_df[["logical"]] <- TRUE

do_over_df1 <- dummy_df |> if.(vars > values1)
do_over_df2 <- dummy_df |> if.(education != values2)
do_over_df3 <- test_df  |> if.(logical == values3)

expect_true(collapse::fmin(do_over_df1[["income"]])  > 1000, info = "if. as do over loop for subsetting")
expect_true(collapse::fmin(do_over_df1[["balance"]]) > 0,    info = "if. as do over loop for subsetting")
expect_true(collapse::funique(do_over_df2[["education"]]) == "middle", info = "if. as do over loop for subsetting")
expect_true(collapse::fnrow(do_over_df3) == 0, info = "if. as do over loop for subsetting")

# Warning on doubled logical operators
test_df <- dummy_df |> if.((sex == 1 && education == "high") || sex == 2)

expect_warning(print_stack_as_messages("WARNING"), "Replaced",  info = "Warning on doubled logical operators")

unique_male   <- test_df |> if.(sex == 1)
unique_male   <- collapse::funique(unique_male[["education"]])
unique_female <- test_df |> if.(sex == 2)
unique_female <- collapse::funique(unique_female[["education"]])

expect_true("high" %in% unique_male, info = "Warning on doubled logical operators")
expect_equal(length(unique_male), 1, info = "Warning on doubled logical operators")
expect_true(all(c("low", "middle", "high") %in% unique_female), info = "Warning on doubled logical operators")


# Abort subset with if., if variable is not part of the data frame
test_df <- dummy_df |> if.("test")

expect_error(print_stack_as_messages("ERROR"), "No variable for subsetting provided. Data frame remains as is.",
               info = "Abort subset with if., if variable is not part of the data frame")

expect_equal(test_df, dummy_df, info = "Abort subset with if., if variable is not part of the data frame")


# Abort subset with if., if multiple variables are provided
test_df <- dummy_df |> if.(c("age", "sex"))

expect_error(print_stack_as_messages("ERROR"), "Only single variables and conditions allowed. Data frame remains as is.",
               info = "Abort subset with if., if multiple variables are provided")

expect_equal(test_df, dummy_df, info = "Abort subset with if., if multiple variables are provided")

###############################################################################
# Where.
###############################################################################

# Filter data frame with where.
test_df <- dummy_df |> where.(sex == 1, c("sex", "age"))

expect_equal(names(test_df), c("sex", "age"), info = "Filter data frame with where.")
expect_true(!any(c(2, NA) %in% test_df[["sex"]]), info = "Filter data frame with where.")


# where. aborts with a warning, if no observations left
test_df <- dummy_df |> where.(sex == 0)

expect_warning(print_stack_as_messages("WARNING"), "No observations left in the data frame.",
               info = "where. aborts with a warning, if no observations left")

###############################################################################
# ifelse_multi
###############################################################################

# ifelse_multi basic recoding works
test_df <- ifelse_df |> ifelse_multi("age < 18" = 1,
                                     "age < 65" = 2,
                                     else. = 3)

expect_equal(test_df, c(1, 2, 2, 3, 3), info = "ifelse_multi basic recoding works")


# ifelse_multi SAS intervals are translated
test_df <- ifelse_df |> ifelse_multi(" 0 <= age < 30" = 1,
                                     "30 <= age < 65" = 2,
                                     else. = 3)

expect_equal(test_df, c(1, 1, 2, 3, 3), info = "ifelse_multi SAS intervals are translated")


# ifelse_multi IN works with commas
test_df <- ifelse_df |> ifelse_multi("age in (10, 20)" = 1, else. = 0)

expect_equal(test_df, c(1, 1, 0, 0, 0), info = "ifelse_multi IN works with commas")


# ifelse_multi IN works with spaces
test_df <- ifelse_df |> ifelse_multi("age in (10 20)" = 1, else. = 0)

expect_equal(test_df, c(1, 1, 0, 0, 0), info = "ifelse_multi IN works with spaces")


# ifelse_multi NOT IN works
test_df <- ifelse_df |> ifelse_multi("age not in (10 20)" = 1, else. = 0)

expect_equal(test_df, c(0, 0, 1, 1, 1), info = "ifelse_multi NOT IN works")


# ifelse_multi character with spaces works with IN
test_df <- ifelse_df |> ifelse_multi("name in ('Hello World' 'Hello Again')" = 1, else. = 0)

expect_equal(test_df, c(1, 1, 0, 0, 0), info = "ifelse_multi character with spaces works with IN")


# ifelse_multi character expressions starting with letter
test_df <- ifelse_df |> ifelse_multi("name == 'Hell:'" = 1, else. = 0)

expect_equal(test_df, c(1, 1, 1, 0, 0), info = "ifelse_multi character expressions starting with letter")


# ifelse_multi character expressions ending with letter
test_df <- ifelse_df |> ifelse_multi("name == ':orld'" = 1, else. = 0)

expect_equal(test_df, c(1, 0, 0, 1, 0), info = "ifelse_multi character expressions ending with letter")


# ifelse_multi character expressions contain letter
test_df <- ifelse_df |> ifelse_multi("name == ':ga:'" = 1, else. = 0)

expect_equal(test_df, c(0, 1, 0, 0, 0), info = "ifelse_multi character expressions contain letter")


# ifelse_multi negated character expressions contain letter
test_df <- ifelse_df |> ifelse_multi("name != ':ga:'" = 1, else. = 0)

expect_equal(test_df, c(1, 0, 1, 1, 1), info = "ifelse_multi negated character expressions contain letter")


# ifelse_multi AND is translated
test_df <- ifelse_df |> ifelse_multi("age < 18 and sex == 1" = 1, else. = 0)

expect_equal(test_df, c(1, 0, 0, 0, 0), info = "ifelse_multi AND is translated")


# ifelse_multi OR is translated
test_df <- ifelse_df |> ifelse_multi("age < 18 or sex == 2" = 1, else. = 0)

expect_equal(test_df, c(1, 1, 0, 1, 0), info = "ifelse_multi AND is translated")


# ifelse_multi na.rm = FALSE preserves NA
test_df <- ifelse_df |> ifelse_multi("age < 18" = 1, else. = 0, na.rm = FALSE)

expect_equal(test_df, c(1, 0, 0, 0, NA), info = "ifelse_multi na.rm = FALSE preserves NA")


# ifelse_multi do_if condition is used on all other conditions
test_df <- ifelse_df |> ifelse_multi(do_if = "sex == 1",
                                     "age < 18" = 1,
                                     "age < 65" = 2,
                                     else. = 3)

expect_equal(test_df, c(1, 3, 2, 3, 3), info = "ifelse_multi do_if condition is used on all other conditions")


# ifelse_multi aborts on missing quotation mark
test_df <- ifelse_df |> ifelse_multi("name in ('Hello World 'Hello Again')" = 1, else. = 0)

expect_error(print_stack_as_messages("ERROR"), "Condition couldn't be parsed.",
             info = "ifelse_multi aborts on missing quotation mark")


# ifelse_multi aborts on non named list condition
test_df <- ifelse_df |> ifelse_multi("name in ('Hello World 'Hello Again')")

expect_error(print_stack_as_messages("ERROR"), "You have to pass conditions and assignments in the form",
             info = "ifelse_multi aborts on non named list condition")

###############################################################################
# Abort checks
###############################################################################

# if. as do over loop aborts on vectors of unequal lengths
vars1  <- c("income", "balance")
vars2  <- c("VAR1", "VAR2", "VAR3")
values <- c(1, 2)

do_over_df <- dummy_df |> if.(vars1 > 0, vars2 = values)

expect_error(print_stack_as_messages("ERROR"), "Passed vectors are of unequal lengths.",
               info = "if. as do over loop aborts on vectors of unequal lengths")

do_over_df <- dummy_df |> else_if.(vars1 > 0, vars2 = values)

expect_error(print_stack_as_messages("ERROR"), "Passed vectors are of unequal lengths.",
               info = "if. as do over loop aborts on vectors of unequal lengths")

do_over_df <- dummy_df |> else.(vars2 = values, vars2 = values)

expect_error(print_stack_as_messages("ERROR"), "Passed vectors are of unequal lengths.",
               info = "if. as do over loop aborts on vectors of unequal lengths")


# if. as do over loop aborts without variable assignment
vars <- c("income", "balance")

do_over_df <- dummy_df |> else_if.(vars > 0)

expect_warning(print_stack_as_messages("WARNING"), "No assignments found. If you want to filter observations",
               info = "if. as do over loop aborts without variable assignment")


# else_do aborts when there is no active filter
test_df <- dummy_df |> else_do()

expect_error(print_stack_as_messages("ERROR"), "No active filter variable found.",
               info = "else_do aborts when there is no active filter")


# end_do aborts when there is no active filter
test_df <- dummy_df |> end_do()

expect_error(print_stack_as_messages("ERROR"), "No active filter variable found.",
               info = "end_do aborts when there is no active filter")


# end_all_do aborts when there is no active filter
test_df <- dummy_df |> end_all_do()

expect_error(print_stack_as_messages("ERROR"), "No active filter variable found.",
               info = "end_all_do aborts when there is no active filter")


set_no_print()
