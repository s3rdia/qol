reset_style_options()
default_style_options <- get_style_options()
default_var_labels    <- get_variable_labels()
default_stat_labels   <- get_stat_labels()
default_print         <- get_print()
default_monitor       <- get_monitor()
default_na            <- get_na.rm()
default_print_miss    <- get_print_miss()
default_output        <- get_output()
default_titles        <- get_titles()
default_footnotes     <- get_footnotes()
default_threads       <- get_threads()


# Get global style options
expect_equal(default_style_options, get_style_options(), info = "Get global style options")


# Set global style options
set_style_options(save_path = "C:/MyPath/")

new_options <- get_style_options()

expect_equal(default_style_options[["save_path"]], NULL, info = "Set global style options")
expect_equal(new_options[["save_path"]], "C:/MyPath/", info = "Set global style options")


# Reset global style options
set_style_options(save_path = "C:/MyPath/")

new_options <- get_style_options()

expect_equal(new_options[["save_path"]], "C:/MyPath/", info = "Reset global style options")

reset_style_options()

reset_options <- get_style_options()
expect_equal(reset_options[["save_path"]], NULL, info = "Reset global style options")


# Close file in global options
set_style_options(file = "MyFile.xlsx")

new_options <- get_style_options()

expect_equal(new_options[["file"]], "MyFile.xlsx", info = "Close file in global options")

close_file()

closed_file <- get_style_options()
expect_equal(closed_file[["file"]], NULL, info = "Close file in global options")


# Get global variable labels
expect_equal(length(default_var_labels), 0, info = "Get global variable labels")


# Get global statistic labels
expect_equal(length(default_stat_labels), 0, info = "Get global statistic labels")


# Get global print option
expect_equal(default_print, TRUE, info = "Get global print option")


# Get global monitor option
expect_equal(default_monitor, FALSE, info = "Get global monitor option")


# Get global na.rm option
expect_equal(default_na, FALSE, info = "Get global na.rm option")


# Set global variable labels
set_variable_labels(var1 = "Variable 1")

new_options <- get_variable_labels()

expect_equal(new_options[["var1"]], "Variable 1", info = "Set global variable labels")


# Set global statistic labels
set_stat_labels(pct = "Percent")

new_options <- get_stat_labels()

expect_equal(new_options[["pct"]], "Percent", info = "Set global statistic labels")


# Set global print option
set_print(FALSE)

new_options <- get_print()

expect_equal(new_options, FALSE, info = "Set global print option")


# Set global monitor option
set_monitor(TRUE)

new_options <- get_monitor()

expect_equal(new_options, TRUE, info = "Set global monitor option")


# Set global na.rm option
set_na.rm(TRUE)

new_options <- get_na.rm()

expect_equal(new_options, TRUE, info = "Set global na.rm option")


# Set global print_miss option
set_print_miss(TRUE)

new_options <- get_print_miss()

expect_equal(new_options, TRUE, info = "Set global print_miss option")


# Set global output option
set_output("excel")

new_options <- get_output()

expect_equal(new_options, "excel", info = "Set global output option")


# Set global titles
set_titles("Title1", "Title2")

new_options <- get_titles()

expect_equal(new_options, c("Title1", "Title2"), info = "Set global titles")


# Set global footnotes
set_footnotes("Footnote1", "Footnote2")

new_options <- get_footnotes()

expect_equal(new_options, c("Footnote1", "Footnote2"), info = "Set global footnotes")


# Set global threads
set_threads(1)

new_options <- get_threads()

expect_equal(new_options, 1, info = "Set global threads")

set_threads(NULL)

new_options <- get_threads()

expect_equal(new_options, fst::threads_fst(), info = "Set global threads")


# Reset global options
reset_qol_options()

new_print      <- get_print()
new_monitor    <- get_monitor()
new_na         <- get_na.rm()
new_print_miss <- get_print_miss()
new_output     <- get_output()
new_titles     <- get_titles()
new_footnotes  <- get_footnotes()
new_threads    <- get_threads()

expect_true(default_print      == new_print, info = "Reset global options")
expect_true(default_monitor    == new_monitor, info = "Reset global options")
expect_true(default_na         == new_na, info = "Reset global options")
expect_true(default_print_miss == new_print_miss, info = "Reset global options")
expect_true(default_output     == new_output, info = "Reset global options")
expect_equal(new_titles, NULL, info = "Reset global options")
expect_equal(new_footnotes, NULL, info = "Reset global options")
expect_equal(new_threads, fst::threads_fst(), info = "Reset global options")

###############################################################################
# Warning checks
###############################################################################


# Warning on setting wrong logical style in global options
expect_message(set_style_options(filters = 1), " ! WARNING: 'filters' must be <logical>. Option will be omitted.", info = "Warning on setting wrong logical style in global options")


# Warning on setting wrong numeric style in global options
expect_message(set_style_options(start_row = "1"), " ! WARNING: 'start_row' must be <numeric>. Option will be omitted.", info = "Warning on setting wrong numeric style in global options")


# Warning on setting wrong character style in global options
expect_message(set_style_options(sheet_name = 1), " ! WARNING: 'sheet_name' must be <character>. Option will be omitted.", info = "Warning on setting wrong character style in global options")


# Warning on setting wrong color style in global options
expect_message(set_style_options(header_back_color = 1), " ! WARNING: 'header_back_color' must be a 6 character <hex code>. Option will be omitted", info = "Warning on setting wrong color style in global options")


# Warning on setting non existent style in global options
expect_message(set_style_options(test = 1), " ! WARNING: 'test' is not a valid style option. See", info = "Warning on setting non existent style in global options")

###############################################################################
# Abort checks
###############################################################################


# Abort setting global style options on empty list
expect_message(set_style_options(),   " X ERROR: Empty list found.", info = "Abort setting global style options on empty list")
expect_message(set_variable_labels(), " X ERROR: Empty list found.", info = "Abort setting global style options on empty list")
expect_message(set_stat_labels(),     " X ERROR: Empty list found.", info = "Abort setting global style options on empty list")


# Abort setting global options on empty list
expect_message(set_print(1),      " X ERROR: Print option can only be TRUE or FALSE. Global option remains unchanged.", info = "Abort setting global options on empty list")
expect_message(set_monitor(1),    " X ERROR: Monitor option can only be TRUE or FALSE. Global option remains unchanged.", info = "Abort setting global options on empty list")
expect_message(set_na.rm(1),      " X ERROR: NA removal option can only be TRUE or FALSE. Global option remains unchanged.", info = "Abort setting global options on empty list")
expect_message(set_print_miss(1), " X ERROR: Print missing categories option can only be TRUE or FALSE. Global option remains unchanged.", info = "Abort setting global options on empty list")
expect_message(set_output(1),     " X ERROR: Output can only be 'console', 'text', 'excel' or 'excel_nostyle'. Global option remains unchanged.", info = "Abort setting global options on empty list")
expect_message(set_titles(1),     " X ERROR: Titles must be provided as character. Global titles remain unchanged.", info = "Abort setting global options on empty list")
expect_message(set_footnotes(1),  " X ERROR: Footnotes must be provided as character. Global footnotes remain unchanged.", info = "Abort setting global options on empty list")
expect_message(set_threads(""),   " X ERROR: Number of used threads must be provided as integer value. Global number of used threads remains unchanged.", info = "Abort setting global options on empty list")
