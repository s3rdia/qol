default_options <- get_style_options()


test_that("Get global style options", {
  expect_equal(default_options, get_style_options())
})


test_that("Set global style options", {
    set_style_options(save_path = "C:/MyPath/")

    new_options <- get_style_options()

    expect_equal(default_options[["save_path"]], NULL)
    expect_equal(new_options[["save_path"]], "C:/MyPath/")
})


test_that("Reset global style options", {
    set_style_options(save_path = "C:/MyPath/")

    new_options <- get_style_options()

    expect_equal(new_options[["save_path"]], "C:/MyPath/")

    reset_style_options()

    reset_options <- get_style_options()
    expect_equal(reset_options[["save_path"]], NULL)
})

###############################################################################
# Warning checks
###############################################################################


test_that("Warning on setting wrong logical style in global options", {
    expect_message(set_style_options(filters = 1), " ! WARNING: 'filters' must be <logical>. Option will be omitted.")
})


test_that("Warning on setting wrong numeric style in global options", {
    expect_message(set_style_options(start_row = "1"), " ! WARNING: 'start_row' must be <numeric>. Option will be omitted.")
})


test_that("Warning on setting wrong character style in global options", {
    expect_message(set_style_options(sheet_name = 1), " ! WARNING: 'sheet_name' must be <character>. Option will be omitted.")
})


test_that("Warning on setting wrong color style in global options", {
    expect_message(set_style_options(header_back_color = 1), " ! WARNING: 'header_back_color' must be a 6 character <hex code>. Option will be omitted")
})


test_that("Warning on setting non existent style in global options", {
    expect_message(set_style_options(test = 1), " ! WARNING: 'test' is not a valid style option. See")
})

###############################################################################
# Abort checks
###############################################################################


test_that("Abort setting global style options on empty list", {
    expect_message(set_style_options(), " X ERROR: Empty list found. See")
})
