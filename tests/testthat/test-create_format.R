###############################################################################
# Suppressing some functions messages because they only output the information
# on how much time they took.
###############################################################################

test_that("Create single discrete label", {
    sex. <- suppressMessages(discrete_format(
            "Male"   = 1,
            "Female" = 2))

    expect_true(all(c("value", "label") %in% names(sex.)))
    expect_equal(nrow(sex.), 2)
    expect_equal(ncol(sex.), 2)
})


test_that("Create discrete multilabel", {
    sex. <- suppressMessages(discrete_format(
        "Total"  = 1:2,
        "Male"   = 1,
        "Female" = 2))

    expect_true(all(c("value", "label") %in% names(sex.)))
    expect_equal(nrow(sex.), 4)
    expect_equal(ncol(sex.), 2)
})


test_that("Create single interval label", {
    income. <- suppressMessages(interval_format(
        "below 500"          = 0:499,
        "500 to under 1000"  = 500:999,
        "1000 to under 2000" = 1000:1999,
        "2000 and more"      = 2000:99999))

    expect_true(all(c("from", "to", "label") %in% names(income.)))
    expect_equal(nrow(income.), 4)
    expect_equal(ncol(income.), 3)
})


test_that("Create interval multilabel", {
    income. <- suppressMessages(interval_format(
        "Total"              = 0:99999,
        "below 500"          = 0:499,
        "500 to under 1000"  = 500:999,
        "1000 to under 2000" = 1000:1999,
        "2000 and more"      = 2000:99999))

    expect_true(all(c("from", "to", "label") %in% names(income.)))
    expect_equal(nrow(income.), 5)
    expect_equal(ncol(income.), 3)
})


test_that("Create interval format with low and high keywords", {
    income. <- suppressMessages(interval_format(
        "Total"              = c("low", "high"),
        "below 500"          = c("low", 499),
        "500 to under 1000"  = 500:999,
        "1000 to under 2000" = 1000:1999,
        "2000 and more"      = c(2000, "high")))

    expect_true(all(c("from", "to", "label") %in% names(income.)))
    expect_equal(income.[1, "from"], -Inf)
    expect_equal(income.[1, "to"],    Inf)
})


test_that("Expand formats", {
    sex. <- suppressMessages(discrete_format(
        "Total"  = 1:2,
        "Male"   = 1,
        "Female" = 2))

    income. <- suppressMessages(interval_format(
        "below 500"          = 0:499,
        "500 to under 1000"  = 500:999,
        "1000 to under 2000" = 1000:1999,
        "2000 and more"      = 2000:99999))

    expand_df <- expand_formats(sex., income.,
                                names = c("sex", "income"))

    expect_true(all(c("sex", "income") %in% names(expand_df)))
    expect_equal(collapse::fncol(expand_df), 2)
    expect_equal(collapse::fnrow(expand_df), 12)
})


test_that("Expand formats provided as list", {
    sex. <- suppressMessages(discrete_format(
        "Total"  = 1:2,
        "Male"   = 1,
        "Female" = 2))

    income. <- suppressMessages(interval_format(
        "below 500"          = 0:499,
        "500 to under 1000"  = 500:999,
        "1000 to under 2000" = 1000:1999,
        "2000 and more"      = 2000:99999))

    expand_df <- expand_formats(list(sex., income.), names = c("sex", "income"))

    expect_true(all(c("sex", "income") %in% names(expand_df)))
    expect_equal(collapse::fncol(expand_df), 2)
    expect_equal(collapse::fnrow(expand_df), 12)
})


test_that("Expand formats returns unique label values if only one format is provided", {
    sex. <- suppressMessages(discrete_format(
        "Total"  = 1:2,
        "Male"   = 1,
        "Female" = 2))

    expand_df <- expand_formats(sex.)

    expect_equal(expand_df[[1]], c("Total", "Male", "Female"))
})

###############################################################################
# Abort checks
###############################################################################

test_that("Abort format expansion, if a data frame has no label column", {
    sex. <- suppressMessages(discrete_format(
        "Total"  = 1:2,
        "Male"   = 1,
        "Female" = 2))

    sex. <- sex. |> dropp("label")

    income. <- suppressMessages(interval_format(
        "below 500"          = 0:499,
        "500 to under 1000"  = 500:999,
        "1000 to under 2000" = 1000:1999,
        "2000 and more"      = 2000:99999))

    expect_message(expand_df <- expand_formats(sex., income.,
                                               names = c("sex", "income")),
                   " X ERROR: A data frame is missing the 'label' column. This function is especially for expanding formats created")
})


test_that("Create single discrete label aborts, if elements not provided in the correct way", {
    expect_message(discrete_format(test == 1),
                   " X ERROR: Formats must be provided in the form.")
})


test_that("Create single discrete label aborts, if list element is missing a name", {
    expect_message(discrete_format(1),
                   " X ERROR: Formats must be provided in the form.")
})


test_that("Create single interval label aborts, if elements not provided in the correct way", {
    expect_message(interval_format(test == 1),
                   " X ERROR: Formats must be provided in the form.")
})


test_that("Create single interval label aborts, if list element is missing a name", {
    expect_message(discrete_format(1),
                   " X ERROR: Formats must be provided in the form.")
})
