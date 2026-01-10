###############################################################################
# Suppressing some functions messages because they only output the information
# on how much time they took.
###############################################################################

test_df <- data.frame(
    var_by   = c(1, 1, 2, 3, 3),
    var_num  = c(1, NA, NA, 2, NA),
    var_num2 = c(1, 2, 3, 4, 5),
    var_char = c("a", NA, NA, "b", NA),
    var_sum  = c(1, 1, 1, 1, 1))

dummy_df <- dummy_data(10)


test_that("Generate running number without by", {
    run_df <- suppressMessages(test_df |> running_number())

    expect_equal(run_df[["run_nr"]], c(1, 2, 3, 4, 5))
})


test_that("Generate running number with by", {
    run_df <- suppressMessages(test_df |> running_number(by = var_by))

    expect_equal(run_df[["run_nr"]], c(1, 2, 1, 1, 2))
})


test_that("Mark first and last cases without by", {
    mark_df <- suppressMessages(test_df |>
        mark_case() |>
        mark_case(var_name = "last", first = FALSE))

    expect_equal(mark_df[["first"]], c(TRUE, FALSE, FALSE, FALSE, FALSE))
    expect_equal(mark_df[["last"]],  c(FALSE, FALSE, FALSE, FALSE, TRUE))
})


test_that("Mark first and last cases with by", {
    mark_df <- suppressMessages(test_df |>
       mark_case(by = var_by) |>
       mark_case(var_name = "last", by = var_by, first = FALSE))

    expect_equal(mark_df[["first"]], c(TRUE, FALSE, TRUE, TRUE, FALSE))
    expect_equal(mark_df[["last"]],  c(FALSE, TRUE, TRUE, FALSE, TRUE))
})


test_that("Retain value without by", {
    retain_df <- suppressMessages(test_df |>
        retain_value(value = var_num))

    expect_equal(retain_df[["retain_value"]], c(1, 1, 1, 1, 1))
})


test_that("Retain value with by", {
    retain_df <- suppressMessages(test_df |>
          retain_value(value = var_num, by = var_by))

    expect_equal(retain_df[["retain_value"]], c(1, 1, NA, 2, 2))
})


test_that("Retain value and overwrite existing variable", {
    retain_df <- suppressMessages(test_df |>
          retain_value(value    = var_num,
                       var_name = var_num))

    expect_equal(names(test_df), names(retain_df))
})


test_that("Retain character value and overwrite existing variable", {
    retain_df <- suppressMessages(test_df |>
          retain_value(value    = var_char,
                       var_name = var_char))

    expect_equal(names(test_df), names(retain_df))
})


test_that("Retain character value", {
    retain_df <- suppressMessages(test_df |>
          retain_value(value = var_char))

    expect_equal(retain_df[["retain_value"]], c("a", "a", "a", "a", "a"))
})


test_that("Retain multiple values", {
    retain_df <- suppressMessages(test_df |>
          retain_value(value = c(var_num, var_char),
                       by    = c(var_sum, var_by)))

    expect_equal(retain_df[["var_num_first"]],  c(1, 1, NA, 2, 2))
    expect_equal(retain_df[["var_char_first"]], c("a", "a", NA, "b", "b"))
})


test_that("Retain sum without by", {
    retain_df <- suppressMessages(test_df |>
          retain_sum(value = var_sum))

    expect_equal(retain_df[["retain_sum"]], c(5, 5, 5, 5, 5))
})


test_that("Retain sum with by", {
    retain_df <- suppressMessages(test_df |>
          retain_sum(value = var_sum, by = var_by))

    expect_equal(retain_df[["retain_sum"]], c(2, 2, 1, 2, 2))
})


test_that("Retain multiple sums", {
    retain_df <- suppressMessages(test_df |>
          retain_sum(value = c(var_num, var_num2),
                      by   = c(var_sum, var_by)))

    expect_equal(retain_df[["var_num_sum"]],  c(1, 1, NA, 2, 2))
    expect_equal(retain_df[["var_num2_sum"]], c(3, 3, 3, 9, 9))
})


test_that("Retain columns in a data frame and order them to the front", {
    retain_df <- suppressMessages(dummy_df |>
          retain_variables(age, sex, income))

    expect_equal(names(retain_df)[1:3], c("age", "sex", "income"))
})


test_that("Retain columns in a data frame and order them to the end", {
    retain_df <- suppressMessages(dummy_df |>
          retain_variables(age, sex, income, order_last = TRUE))

    expect_equal(utils::tail(names(retain_df), 3), c("age", "sex", "income"))
})


test_that("Retain column ranges in a data frame", {
    retain_df <- suppressMessages(dummy_df |>
          retain_variables(state:age))

    expect_equal(names(retain_df)[1:3], c("state", "sex", "age"))
})


test_that("Add new single NA columns with retain", {
    retain_df <- suppressMessages(dummy_df |>
          retain_variables(status, new_var, test_var))

    expect_equal(names(retain_df)[1:3], c("status", "new_var", "test_var"))
})


test_that("Add new NA column range with retain", {
    retain_df <- suppressMessages(dummy_df |>
          retain_variables(status1:status3))

    expect_equal(names(retain_df)[1:3], c("status1", "status2", "status3"))
})


test_that("Add new NA column range with wrong pattern returns original data frame", {
    expect_message(retain_df <- (dummy_df |>
          retain_variables(status1:age3)), "X ERROR: Variable range has to be provided in the form 'var_name1:var_name10'.")

    expect_equal(dummy_df, retain_df)
})

###############################################################################
# Note checks
###############################################################################

test_that("Generate running number with multiple by variables", {
    expect_message(run_df <- dummy_df |> running_number(by = c(year, sex)),
                   " ~ NOTE: Running number is generated in current data frame order.")
})


test_that("Mark first and last cases with multiple by variables", {
    expect_message(mark_df <- dummy_df |>
                       mark_case(by = c(sex, household_id)) |>
                       mark_case(var_name = "last", by = c(sex, household_id), first = FALSE),
                   " ~ NOTE: Cases are marked in current data frame order.")

    expect_true(all(c("first", "last") %in% names(mark_df)))
})

###############################################################################
# Abort checks
###############################################################################

test_that("Retain value without providing a value", {
    expect_message(retain_df <- dummy_df |>
                       retain_value(),
                   " X ERROR: Must provide <values> to retain. Retain will be aborted.")
})


test_that("Retain sum without providing sum_of", {
    expect_message(retain_df <- dummy_df |>
                       retain_sum(),
                   " X ERROR: Must provide a <values> to retain. Retain will be aborted.")
})
