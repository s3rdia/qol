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
    test_df[["run_nr"]] <- suppressMessages(test_df |> running_number())

    expect_equal(test_df[["run_nr"]], c(1, 2, 3, 4, 5))
})


test_that("Generate running number with by", {
    test_df[["run_nr"]] <- suppressMessages(test_df |> running_number(by = var_by))

    expect_equal(test_df[["run_nr"]], c(1, 2, 1, 1, 2))
})


test_that("Mark first and last cases without by", {
    test_df[["first"]] <- suppressMessages(test_df |> mark_case())
    test_df[["last"]]  <- suppressMessages(test_df |> mark_case(first = FALSE))

    expect_equal(test_df[["first"]], c(1, 0, 0, 0, 0))
    expect_equal(test_df[["last"]],  c(0, 0, 0, 0, 1))
})


test_that("Mark first and last cases with by", {
    test_df[["first"]] <- suppressMessages(test_df |> mark_case(by = var_by))
    test_df[["last"]]  <- suppressMessages(test_df |> mark_case(by = var_by, first = FALSE))

    expect_equal(test_df[["first"]], c(1, 0, 1, 1, 0))
    expect_equal(test_df[["last"]],  c(0, 1, 1, 0, 1))
})


test_that("Retain value without by", {
    test_df[["retain_value"]] <- suppressMessages(test_df |>
        retain_value(values = var_num))

    expect_equal(test_df[["retain_value"]], c(1, 1, 1, 1, 1))
})


test_that("Retain value with by", {
    test_df[["retain_value"]] <- suppressMessages(test_df |>
          retain_value(values = var_num, by = var_by))

    expect_equal(test_df[["retain_value"]], c(1, 1, NA, 2, 2))
})


test_that("Retain character value", {
    test_df[["retain_value"]] <- suppressMessages(test_df |>
          retain_value(values = var_char))

    expect_equal(test_df[["retain_value"]], c("a", "a", "a", "a", "a"))
})


test_that("Retain multiple values", {
    test_df[, c("var_num_first", "var_char_first")] <-
        suppressMessages(test_df |>
          retain_value(values = c(var_num, var_char),
                       by    = c(var_sum, var_by)))

    expect_equal(test_df[["var_num_first"]],  c(1, 1, NA, 2, 2))
    expect_equal(test_df[["var_char_first"]], c("a", "a", NA, "b", "b"))
})


test_that("Retain sum without by", {
    test_df[["retain_sum"]] <- suppressMessages(test_df |>
          retain_sum(values = var_sum))

    expect_equal(test_df[["retain_sum"]], c(5, 5, 5, 5, 5))
})


test_that("Retain sum with by", {
    test_df[["retain_sum"]] <- suppressMessages(test_df |>
          retain_sum(values = var_sum, by = var_by))

    expect_equal(test_df[["retain_sum"]], c(2, 2, 1, 2, 2))
})


test_that("Retain multiple sums", {
    test_df[, c("var_num_sum", "var_num2_sum")] <-
        suppressMessages(test_df |>
          retain_sum(values = c(var_num, var_num2),
                      by    = c(var_sum, var_by)))

    expect_equal(test_df[["var_num_sum"]],  c(1, 1, NA, 2, 2))
    expect_equal(test_df[["var_num2_sum"]], c(3, 3, 3, 9, 9))
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


test_that("Retain variables starting with letter", {
    retain_df1 <- suppressMessages(dummy_df |>
          retain_variables("s:"))

    retain_df2 <- suppressMessages(dummy_df |>
          retain_variables("s:", order_last = TRUE))

    expect_equal(names(retain_df1)[1:2], c("state", "sex"))
    expect_equal(utils::tail(names(retain_df2), 2), c("state", "sex"))
})


test_that("Retain variables ending with letter", {
    retain_df1 <- suppressMessages(dummy_df |>
          retain_variables(":id"))

    retain_df2 <- suppressMessages(dummy_df |>
          retain_variables(":id", order_last = TRUE))

    expect_equal(names(retain_df1)[1:2], c("household_id", "person_id"))
    expect_equal(utils::tail(names(retain_df2), 2), c("household_id", "person_id"))
})


test_that("Retain variables containing letter", {
    retain_df1 <- suppressMessages(dummy_df |>
          retain_variables(":on:"))

    retain_df2 <- suppressMessages(dummy_df |>
          retain_variables(":on:", order_last = TRUE))

    expect_equal(names(retain_df1)[1:3], c("person_id", "first_person", "education"))
    expect_equal(utils::tail(names(retain_df2), 3), c("person_id", "first_person", "education"))
})


test_that("Retain variables with all actions together doesn't break", {
    retain_df <- suppressMessages(dummy_df |>
          retain_variables(state:age, "s:", ":id", ":on:", status1:status3, income))

    expect_equal(names(retain_df), c("state", "sex", "age", "household_id", "person_id", "first_person", "education",
                                     "status1", "status2", "status3", "income", "year", "probability", "weight"))
})

###############################################################################
# Note checks
###############################################################################

test_that("Generate running number with multiple by variables", {
    expect_message(dummy_df[["run_nr"]] <- dummy_df |> running_number(by = c(year, sex)),
                   " ~ NOTE: Running number is generated in current data frame order.")
})


test_that("Mark first and last cases with multiple by variables", {
    expect_message(dummy_df[["first"]] <- dummy_df |>
                       mark_case(by = c(sex, household_id)),
                   " ~ NOTE: Cases are marked in current data frame order.")

    expect_message(dummy_df[["last"]] <- dummy_df |>
                       mark_case(by = c(sex, household_id), first = FALSE),
                   " ~ NOTE: Cases are marked in current data frame order.")

    expect_true(all(c("first", "last") %in% names(dummy_df)))
})

###############################################################################
# Abort checks
###############################################################################

test_that("Retain value without providing a value", {
    expect_message(dummy_df[["retain_value"]] <- dummy_df |>
                       retain_value(),
                   " X ERROR: Must provide <values> to retain. Retain will be aborted.")
})


test_that("Retain sum without providing sum_of", {
    expect_message(dummy_df[["retain_sum"]] <- dummy_df |>
                       retain_sum(),
                   " X ERROR: Must provide a <values> to retain. Retain will be aborted.")
})


test_that("Add new NA column range with wrong pattern returns original data frame", {
    expect_message(retain_df <- (dummy_df |>
         retain_variables(status1:age3)), "X ERROR: Variable range has to be provided in the form 'var_name1:var_name10'.")

    expect_equal(dummy_df, retain_df)
})
