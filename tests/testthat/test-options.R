default_style_options <- get_style_options()
default_var_labels    <- get_variable_labels()
default_stat_labels   <- get_stat_labels()
default_print         <- get_print()
default_monitor       <- get_monitor()
default_na            <- get_na.rm()
default_output        <- get_output()
default_titles        <- get_titles()
default_footnotes     <- get_footnotes()


test_that("Get global style options", {
  expect_equal(default_style_options, get_style_options())
})


test_that("Set global style options", {
    set_style_options(save_path = "C:/MyPath/")

    new_options <- get_style_options()

    expect_equal(default_style_options[["save_path"]], NULL)
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


test_that("Close file in global options", {
    set_style_options(file = "MyFile.xlsx")

    new_options <- get_style_options()

    expect_equal(new_options[["file"]], "MyFile.xlsx")

    close_file()

    closed_file <- get_style_options()
    expect_equal(closed_file[["file"]], NULL)
})


test_that("Get global variable labels", {
    expect_equal(default_var_labels, list())
})


test_that("Get global statistic labels", {
    expect_equal(default_stat_labels, list())
})


test_that("Get global print option", {
    expect_equal(default_print, TRUE)
})


test_that("Get global monitor option", {
    expect_equal(default_monitor, FALSE)
})


test_that("Get global na.rm option", {
    expect_equal(default_na, FALSE)
})


test_that("Set global variable labels", {
    set_variable_labels(var1 = "Variable 1")

    new_options <- get_variable_labels()

    expect_equal(new_options[["var1"]], "Variable 1")
})


test_that("Set global statistic labels", {
    set_stat_labels(pct = "Percent")

    new_options <- get_stat_labels()

    expect_equal(new_options[["pct"]], "Percent")
})


test_that("Set global print option", {
    set_print(FALSE)

    new_options <- get_print()

    expect_equal(new_options, FALSE)
})


test_that("Set global monitor option", {
    set_monitor(TRUE)

    new_options <- get_monitor()

    expect_equal(new_options, TRUE)
})


test_that("Set global na.rm option", {
    set_na.rm(TRUE)

    new_options <- get_na.rm()

    expect_equal(new_options, TRUE)
})


test_that("Set global output option", {
    set_output("excel")

    new_options <- get_output()

    expect_equal(new_options, "excel")
})


test_that("Set global titles", {
    set_titles("Title1", "Title2")

    new_options <- get_titles()

    expect_equal(new_options, c("Title1", "Title2"))
})


test_that("Set global footnotes", {
    set_titles("Footnote1", "Footnote2")

    new_options <- get_titles()

    expect_equal(new_options, c("Footnote1", "Footnote2"))
})


test_that("Reset global options", {
    reset_qol_options()

    new_print     <- get_print()
    new_monitor   <- get_monitor()
    new_na        <- get_na.rm()
    new_output    <- get_output()
    new_titles    <- get_titles()
    new_footnotes <- get_footnotes()

    expect_true(default_print     == new_print)
    expect_true(default_monitor   == new_monitor)
    expect_true(default_na        == new_na)
    expect_true(default_output    == new_output)
    expect_equal(new_titles, NULL)
    expect_equal(new_footnotes, NULL)
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
    expect_message(set_style_options(),   " X ERROR: Empty list found.")
    expect_message(set_variable_labels(), " X ERROR: Empty list found.")
    expect_message(set_stat_labels(),     " X ERROR: Empty list found.")
})


test_that("Abort setting global options on empty list", {
    expect_message(set_print(1),     " X ERROR: Print option can only be TRUE or FALSE. Global option remains unchanged.")
    expect_message(set_monitor(1),   " X ERROR: Monitor option can only be TRUE or FALSE. Global option remains unchanged.")
    expect_message(set_na.rm(1),     " X ERROR: NA removal option can only be TRUE or FALSE. Global option remains unchanged.")
    expect_message(set_output(1),    " X ERROR: Output can only be 'console', 'text', 'excel' or 'excel_nostyle'. Global option remains unchanged.")
    expect_message(set_titles(1),    " X ERROR: Titles must be provided as character. Global titles remain unchanged.")
    expect_message(set_footnotes(1), " X ERROR: Footnotes must be provided as character. Global footnotes remain unchanged.")
})
