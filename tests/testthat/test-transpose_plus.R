###############################################################################
# Suppressing some functions messages because they only output the information
# on how much time they took.
# Printing the output is always set to FALSE, so the overall code can be tested
# but without drawing the whole outputs on screen.
###############################################################################

dummy_df      <- suppressMessages(dummy_data(1000))
dummy_wide_df <- suppressMessages(dummy_df |>
    transpose_plus(preserve = year,
                   pivot    = c("sex", "education"),
                   values   = income,
                   na.rm    = TRUE,
                   formats  = list(sex = suppressMessages(discrete_format(
                       "Total"  = 1:2,
                       "Male"   = 1,
                       "Female" = 2)))))


test_that("Simple long to wide transposition", {
    dummy_df <- suppressMessages(dummy_df |> sort_plus(by = sex))

    wide_df <- suppressMessages(dummy_df |>
        transpose_plus(preserve = year,
                       pivot    = "sex",
                       values   = income,
                       weight   = weight))

    expect_equal(names(wide_df), c("year", "1", "2", NA))
})


test_that("Side by side long to wide transposition", {
    dummy_df <- suppressMessages(dummy_df |> sort_plus(by = c(sex, education)))

    wide_df <- suppressMessages(dummy_df |>
        transpose_plus(preserve = year,
                       pivot    = c("sex", "education"),
                       values   = income,
                       na.rm    = TRUE))

    expect_equal(names(wide_df), c("year", "1", "2", "high", "low", "middle"))
})


test_that("Nested long to wide transposition", {
    dummy_df <- suppressMessages(dummy_df |> sort_plus(by = c(sex, education)))

    wide_df <- suppressMessages(dummy_df |>
        transpose_plus(preserve = year,
                       pivot    = "sex + education",
                       values   = income,
                       na.rm    = TRUE))

    expect_equal(names(wide_df), c("year", "1_high", "1_low", "1_middle",
                                           "2_high", "2_low", "2_middle"))
})


test_that("Transpose multiple value variables", {
    dummy_df <- suppressMessages(dummy_df |> sort_plus(by = sex))
    wide_df <- suppressMessages(dummy_df |>
        transpose_plus(preserve = year,
                       pivot    = "sex",
                       values   = c(income, weight),
                       na.rm    = TRUE))

    expect_equal(names(wide_df), c("year", "income_1", "income_2", "weight_1", "weight_2"))
})


test_that("Using formats in long to wide transposition", {
    wide_df <- suppressMessages(dummy_df |>
        transpose_plus(preserve = year,
                       pivot    = "sex",
                       values   = income,
                       na.rm    = TRUE,
                       formats  = list(sex = suppressMessages(
                           discrete_format("Total"  = 1:2,
                                           "Male"   = 1,
                                           "Female" = 2)))))

    expect_equal(names(wide_df), c("year", "Total", "Male", "Female"))
})


test_that("Simple wide to long transposition", {
    wide_to_long <- suppressMessages(dummy_wide_df |>
            transpose_plus(preserve = year,
                           pivot    = list(sex = c("Total", "Male", "Female"))))

    expect_equal(names(wide_to_long), c("year", "sex", "VALUE"))
})


test_that("Transpose multiple variables from wide to long", {
    wide_to_long <- suppressMessages(dummy_wide_df |>
             transpose_plus(preserve = year,
                            pivot    = list(sex       = c("Male", "Female"),
                                            education = c("low", "middle", "high")),
                            formats  = list(sex = suppressMessages(
                                discrete_format("Total"  = c("Male", "Female"),
                                                "Male"   = "Male",
                                                "Female" = "Female")))))

    expect_equal(names(wide_to_long), c("year", "BY", "VARIABLE", "VALUE"))
})

###############################################################################
# Warning checks
###############################################################################


test_that("Wide to long transposition doesn't support value parameter transposition", {
    expect_message(wide_df <- dummy_wide_df |>
        transpose_plus(preserve = year,
                       pivot    = list(sex = c("Male", "Female")),
                       values   = "Total"), " ~ NOTE: <Values> parameter has no effect in wide to long transposition.")
})


test_that("Wide to long transposition doesn't support weight parameter transposition", {
    expect_message(wide_df <- dummy_wide_df |>
       transpose_plus(preserve = year,
                      pivot    = list(sex = c("Male", "Female")),
                      weight   = "Total"), " ~ NOTE: <Weight> parameter has no effect in wide to long transposition.")
})


test_that("Preserve variable in transposition is not part of the data frame", {
    expect_message(wide_df <- dummy_df |>
       transpose_plus(preserve = "test",
                      pivot    = "sex",
                      value    = income), " ! WARNING: The provided <preserve> variable")
})


test_that("Value variable in transposition is also part of preserve", {
    expect_message(wide_df <- dummy_df |>
       transpose_plus(preserve = sex,
                      pivot    = "age",
                      value    = sex), " ! WARNING: The provided <values> variable")
})

###############################################################################
# Abort checks
###############################################################################

test_that("Wide to long transposition doesn't support nesting variables", {
    expect_message(wide_df <- dummy_wide_df |>
                       transpose_plus(preserve = year,
                                      pivot    = list(sex = "Male + Female")),
                   " X ERROR: Nesting <pivot> variables in a wide to long transposition is not possible.")
})


test_that("Abort transposition if pivot variable is part of preserve", {
    expect_message(wide_df <- dummy_wide_df |>
                       transpose_plus(preserve = year,
                                      pivot    = "year",
                                      value    = income), " X ERROR: The provided <pivot> variable")
})


test_that("Abort transposition if no valid value variable is provided", {
    expect_message(wide_df <- dummy_df |>
                       transpose_plus(preserve = year,
                                      pivot    = "sex"), " X ERROR: No <values> provided. Transposition will be aborted.")
})


test_that("Abort transposition if value variable is not part of the data frame", {
    expect_message(wide_df <- dummy_df |>
                       transpose_plus(pivot    = "sex",
                                      value    = test), " X ERROR: No valid <values> to transpose provided. Transposition will be aborted.")
})


test_that("Abort if value variable in transposition is also part of pivot", {
    expect_message(wide_df <- dummy_df |>
                       transpose_plus(pivot    = "sex",
                                      value    = sex), " X ERROR: The provided <values> variable")
})


test_that("Abort on duplicate variable names after transposition", {
    expect_message(wide_df <- dummy_df |>
                       transpose_plus(preserve = year,
                                      pivot    = c("sex", "education"),
                                      values   = income), " X ERROR: Duplicate column names found:")
})


test_that("Abort if no valid pivot variable is provided in transposition", {
    expect_message(wide_df <- dummy_df |>
                       transpose_plus(pivot = "test",
                                      value = income), " X ERROR: The provided <pivot> variable")
})
