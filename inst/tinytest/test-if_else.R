set_no_print(TRUE)

###############################################################################
# Suppressing some functions messages because they only output the information
# on how much time they took.
###############################################################################

dummy_df <- dummy_data(1000)


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


# else_if. doesn't work without if.
test_df <- dummy_df |>
    else_if.(age >= 18 & age < 65, age_group = "18 to under 65") |>
    else.   (                      age_group = "65 and older")

expect_true(!"age_group" %in% names(test_df), info = "else_if. doesn't work without if.")


# else. doesn't work without if.
test_df <- dummy_df |>
    else.(age_group = "65 and older")

expect_true(!"age_group" %in% names(test_df), info = "else. doesn't work without if.")


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

expect_false(identical(state_df_a[["state_a"]], state_df_b[["state_b"]]), info = "if. and else_if. are not the same")


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

expect_true(all(c("sum", "col_sum", "row_sum", "var1", "var2", "NEW_VAR1", "NEW_VAR2") %in% names(result_df)), info = "if. can do all kinds of calculations")

###############################################################################
# if. for subsetting
###############################################################################

# Subset data frame with if.
test_df <- dummy_df |> if.(sex == 1)

expect_true(nrow(test_df) < nrow(dummy_df), info = "Subset data frame with if.")
expect_true(!2 %in% test_df[["sex"]], info = "Subset data frame with if.")


# Subset data frame with if. (unequal)
test_df <- dummy_df |> if.(sex != 1)

expect_true(nrow(test_df) < nrow(dummy_df), info = "Subset data frame with if. (unequal)")
expect_true(!1 %in% test_df[["sex"]], info = "Subset data frame with if. (unequal)")


# Subset data frame with if., when only providing single variable
test_df <- dummy_df |> if.(sex)

expect_true(nrow(test_df) < nrow(dummy_df), info = "Subset data frame with if., when only providing single variable")
expect_true(!NA %in% test_df[["sex"]], info = "Subset data frame with if., when only providing single variable")


# Subset data frame with if., when only providing single variable as character
test_df <- dummy_df |> if.("sex")

expect_true(nrow(test_df) < nrow(dummy_df), info = "Subset data frame with if., when only providing single variable as character")
expect_true(!NA %in% test_df[["sex"]], info = "Subset data frame with if., when only providing single variable as character")


# if. as do over loop for subsetting
vars   <- c("income", "balance")
values <- c(1000, 0)

do_over_df <- dummy_df |> if.(vars > values)

expect_true(collapse::fmin(do_over_df[["income"]])  > 1000, info = "if. as do over loop for subsetting")
expect_true(collapse::fmin(do_over_df[["balance"]]) > 0,    info = "if. as do over loop for subsetting")


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
