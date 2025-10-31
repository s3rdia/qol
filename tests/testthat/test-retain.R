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


test_that("Generate running number with multiple by variables", {
    expect_message(run_df <- dummy_df |> running_number(by = c(year, sex)),
                              " ~ NOTE: Running number is generated in current data frame order.")
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


test_that("Mark first and last cases with multiple by variables", {
    expect_message(mark_df <- dummy_df |>
                        mark_case(by = c(sex, household_id)) |>
                        mark_case(var_name = "last", by = c(sex, household_id), first = FALSE),
                    " ~ NOTE: Cases are marked in current data frame order.")

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


test_that("Retain value without providing a value", {
    expect_message(retain_df <- dummy_df |>
                        retain_value(),
                    " X ERROR: Must provide a value to retain. Retain will be aborted.")
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


test_that("Retain sum without providing sum_of", {
    expect_message(retain_df <- dummy_df |>
                        retain_sum(),
                    " X ERROR: Must provide a value to retain. Retain will be aborted.")
})
