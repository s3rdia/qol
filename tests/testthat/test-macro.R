test_that("Resolving single macro variable", {
    year <- 2026
    text <- macro("The current year is &year")

    expect_equal(text, "The current year is 2026")
})


test_that("Resolving combination of macro variables", {
    year          <- 2026
    some_variable <- "current"
    current2026   <- "The current year is"

    text <- macro("&&some_variable&year &year")

    expect_equal(text, "The current year is 2026")
})


test_that("Resolving character vector containing macro variables", {
    year          <- 2026
    some_variable <- "current"
    current2026   <- "The current year is"

    char_vector <- c("The current year is &year", "The &some_variable year is &year", "&&some_variable&year &year")

    text_vector <- apply_macro(char_vector)

    expect_equal(text_vector, c("The current year is 2026",
                                "The current year is 2026",
                                "The current year is 2026"))
})

###############################################################################
# Warning checks
###############################################################################

test_that("Resolving macro variable throws a warning if text is a vector", {
    year <- 2026
    expect_message(text <- macro(c("The current year is &year", "The current year is &year")),
                   " ! WARNING: <Text> may only be of length one. The first Element will be used.")

    expect_equal(text, "The current year is 2026")
})


test_that("Resolving macro variable throws a warning if macro variable is a vector", {
    year <- c(2026, 2025)
    expect_message(text <- macro("The current year is &year"),
                   " ! WARNING: Macro variable '&year' may only be of length one. The first Element will be used.")

    expect_equal(text, "The current year is 2026")
})


test_that("Resolving macro variable throws a warning if macro variable is neither character nor numeric", {
    year <- TRUE
    expect_message(text <- macro("The current year is &year"),
                   " ! WARNING: Macro variable '&year' is a complex object. Macro variables may only be character or numeric.")

    expect_equal(text, "The current year is year")
})

###############################################################################
# Abort checks
###############################################################################

test_that("Resolving macro aborts if something other than a text is provided", {
    expect_message(text <- macro(1), " X ERROR: <Text> must be a character. Macro will be aborted.")
})
