###############################################################################
# Suppressing some functions messages because they only output the information
# on how much time they took.
###############################################################################

dummy_df <- suppressMessages(dummy_data(1000))


test_that("Generate running number without by", {
    run_df <- suppressMessages(dummy_df |> running_number())

    expect_true("run_nr" %in% names(run_df))
})


test_that("Generate running number with by", {
    run_df <- suppressMessages(dummy_df |> running_number(by = year))

    expect_true("run_nr" %in% names(run_df))
})


test_that("Mark first and last cases without by", {
    mark_df <- suppressMessages(dummy_df |>
                                    mark_case() |>
                                    mark_case(var_name = "last", first = FALSE))

    expect_true(all(c("first", "last") %in% names(mark_df)))
})


test_that("Mark first and last cases with by", {
    mark_df <- suppressMessages(dummy_df |>
       mark_case(by = household_id) |>
       mark_case(var_name = "last", by = household_id, first = FALSE))

    expect_true(all(c("first", "last") %in% names(mark_df)))
})


test_that("Retain value without by", {
    retain_df <- suppressMessages(dummy_df |>
        retain_value(value = income))

    expect_true("retain_value" %in% names(retain_df))
})


test_that("Retain value with by", {
    retain_df <- suppressMessages(dummy_df |>
          retain_value(value = income, by = household_id))

    expect_true("retain_value" %in% names(retain_df))
})


test_that("Retain sum without by", {
    retain_df <- suppressMessages(dummy_df |>
          retain_sum(value = income))

    expect_true("retain_sum" %in% names(retain_df))
})


test_that("Retain sum with by", {
    retain_df <- suppressMessages(dummy_df |>
          retain_sum(value = income, by = household_id))

    expect_true("retain_sum" %in% names(retain_df))
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
                   " X ERROR: Must provide a <value> to retain. Retain will be aborted.")
})


test_that("Retain sum without providing sum_of", {
    expect_message(retain_df <- dummy_df |>
                       retain_sum(),
                   " X ERROR: Must provide a <value> to retain. Retain will be aborted.")
})
