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

