set_no_print(TRUE)

# Resolving single macro variable
year <- 2026
text <- macro("The current year is &year")

expect_equal(text, "The current year is 2026", info = "Resolving single macro variable")


# Resolving combination of macro variables
year          <- 2026
some_variable <- "current"
current2026   <- "The current year is"

text <- macro("&&some_variable&year &year")

expect_equal(text, "The current year is 2026", info = "Resolving combination of macro variables")


# Resolving character vector containing macro variables
year          <- 2026
some_variable <- "current"
current2026   <- "The current year is"

char_vector <- c("The current year is &year", "The &some_variable year is &year", "&&some_variable&year &year")

text_vector <- apply_macro(char_vector)

expect_equal(text_vector, c("The current year is 2026",
                            "The current year is 2026",
                            "The current year is 2026"), info = "Resolving character vector containing macro variables")

# Resolving macro coerces non-character text to character
text <- macro(1)

expect_equal(text, "1", info = "Resolving macro coerces non-character text to character")

###############################################################################
# Warning checks
###############################################################################

# Resolving macro variable throws a warning if text is a vector
year <- 2026
text <- macro(c("The current year is &year", "The current year is &year"))

expect_warning(print_stack_as_messages("WARNING"), "<Text> may only be of length one. The first Element will be used.",
               info = "Resolving macro variable throws a warning if text is a vector")

expect_equal(text, "The current year is 2026", info = "Resolving macro variable throws a warning if text is a vector")


# Resolving macro variable throws a warning if macro variable is a vector
year <- c(2026, 2025)
text <- macro("The current year is &year")

expect_warning(print_stack_as_messages("WARNING"), "Macro variable \'&year\' may only be of length one. The first Element will be used.",
               info = "Resolving macro variable throws a warning if macro variable is a vector")

expect_equal(text, "The current year is 2026", info = "Resolving macro variable throws a warning if macro variable is a vector")


# Resolving macro variable throws a warning if macro variable is neither character nor numeric
year <- TRUE
text <- macro("The current year is &year")

expect_warning(print_stack_as_messages("WARNING"), "Macro variable \'&year\' is a complex object. Macro variables may only be character or numeric.",
               info = "Resolving macro variable throws a warning if macro variable is neither character nor numeric")

expect_equal(text, "The current year is year", info = "Resolving macro variable throws a warning if macro variable is neither character nor numeric")

###############################################################################
# Centralised checks for macro variables used for formats
###############################################################################

dummy_df <- dummy_data(1000)

sex. <- discrete_format(
    "Total"  = 1:2,
    "Male"   = 1,
    "Female" = 2)

state. <- discrete_format(
    "Total" = 1:16,
    "West"  = 1:8,
    "East"  = 9:16)

sex_variable   <- "sex"
sex_format     <- "sex."
state_variable <- "state"
state_format   <- "state."

# Helper: compares the result of a call using macros against the same call
# using the literal variable and format names. All constellations must lead to
# an identical result.
check_macro <- function(info, literal_fn, macro_fn){
    literal <- literal_fn()
    macro   <- macro_fn()

    expect_identical(macro, literal, info = info)
}

###############################################################################
# any_table
###############################################################################

check_macro(
    "any_table formats with macro variable name and macro format name",
    function() dummy_df |> any_table(rows = "sex", columns = "state",
                                     formats = list(sex = sex., state = state.), output = "no"),
    function() dummy_df |> any_table(rows = "sex", columns = "state",
                                     formats = list("&sex_variable" = "&sex_format", "&state_variable" = "&state_format"), output = "no"))

check_macro(
    "any_table formats with macro variable name and object format",
    function() dummy_df |> any_table(rows = "sex", columns = "state",
                                     formats = list(sex = sex., state = state.), output = "no"),
    function() dummy_df |> any_table(rows = "sex", columns = "state",
                                     formats = list("&sex_variable" = sex., state = "&state_format"), output = "no"))

check_macro(
    "any_table formats with literal variable name and macro format name",
    function() dummy_df |> any_table(rows = "sex", columns = "state",
                                     formats = list(sex = sex., state = state.), output = "no"),
    function() dummy_df |> any_table(rows = "sex", columns = "state",
                                     formats = list(sex = "&sex_format", "&state_variable" = state.), output = "no"))

###############################################################################
# frequencies
###############################################################################

check_macro(
    "frequencies formats with macro variable name and macro format name",
    function() frequencies(dummy_df, variables = "sex", formats = list(sex = sex.), print = FALSE),
    function() frequencies(dummy_df, variables = "sex", formats = list("&sex_variable" = "&sex_format"), print = FALSE))

check_macro(
    "frequencies formats with macro variable name and object format",
    function() frequencies(dummy_df, variables = "sex", formats = list(sex = sex.), print = FALSE),
    function() frequencies(dummy_df, variables = "sex", formats = list("&sex_variable" = sex.), print = FALSE))

check_macro(
    "frequencies formats with literal variable name and macro format name",
    function() frequencies(dummy_df, variables = "sex", formats = list(sex = sex.), print = FALSE),
    function() frequencies(dummy_df, variables = "sex", formats = list(sex = "&sex_format"), print = FALSE))

###############################################################################
# crosstabs
###############################################################################

check_macro(
    "crosstabs formats with macro variable name and macro format name",
    function() crosstabs(dummy_df, rows = "sex", columns = "state", formats = list(sex = sex., state = state.), print = FALSE),
    function() crosstabs(dummy_df, rows = "sex", columns = "state", formats = list("&sex_variable" = "&sex_format", "&state_variable" = "&state_format"), print = FALSE))

check_macro(
    "crosstabs formats with macro variable name and object format",
    function() crosstabs(dummy_df, rows = "sex", columns = "state", formats = list(sex = sex., state = state.), print = FALSE),
    function() crosstabs(dummy_df, rows = "sex", columns = "state", formats = list("&sex_variable" = sex., state = "&state_format"), print = FALSE))

check_macro(
    "crosstabs formats with literal variable name and macro format name",
    function() crosstabs(dummy_df, rows = "sex", columns = "state", formats = list(sex = sex., state = state.), print = FALSE),
    function() crosstabs(dummy_df, rows = "sex", columns = "state", formats = list(sex = "&sex_format", "&state_variable" = state.), print = FALSE))

###############################################################################
# sort_plus
###############################################################################

check_macro(
    "sort_plus formats with macro variable name and macro format name",
    function() sort_plus(dummy_df, by = "sex", formats = list(sex = sex.)),
    function() sort_plus(dummy_df, by = "sex", formats = list("&sex_variable" = "&sex_format")))

check_macro(
    "sort_plus formats with macro variable name and object format",
    function() sort_plus(dummy_df, by = "sex", formats = list(sex = sex.)),
    function() sort_plus(dummy_df, by = "sex", formats = list("&sex_variable" = sex.)))

check_macro(
    "sort_plus formats with literal variable name and macro format name",
    function() sort_plus(dummy_df, by = "sex", formats = list(sex = sex.)),
    function() sort_plus(dummy_df, by = "sex", formats = list(sex = "&sex_format")))

###############################################################################
# summarise_plus
###############################################################################

check_macro(
    "summarise_plus formats with macro variable name and macro format name",
    function() suppressMessages(summarise_plus(dummy_df, values = "sex", formats = list(sex = sex.), print = FALSE)),
    function() suppressMessages(summarise_plus(dummy_df, values = "sex", formats = list("&sex_variable" = "&sex_format"), print = FALSE)))

check_macro(
    "summarise_plus formats with macro variable name and object format",
    function() suppressMessages(summarise_plus(dummy_df, values = "sex", formats = list(sex = sex.), print = FALSE)),
    function() suppressMessages(summarise_plus(dummy_df, values = "sex", formats = list("&sex_variable" = sex.), print = FALSE)))

check_macro(
    "summarise_plus formats with literal variable name and macro format name",
    function() suppressMessages(summarise_plus(dummy_df, values = "sex", formats = list(sex = sex.), print = FALSE)),
    function() suppressMessages(summarise_plus(dummy_df, values = "sex", formats = list(sex = "&sex_format"), print = FALSE)))

###############################################################################
# transpose_plus
###############################################################################

check_macro(
    "transpose_plus formats with macro variable name and macro format name",
    function() suppressMessages(transpose_plus(dummy_df, pivot = "sex", values = "state", formats = list(sex = sex., state = state.))),
    function() suppressMessages(transpose_plus(dummy_df, pivot = "sex", values = "state", formats = list("&sex_variable" = "&sex_format", "&state_variable" = "&state_format"))))

check_macro(
    "transpose_plus formats with macro variable name and object format",
    function() suppressMessages(transpose_plus(dummy_df, pivot = "sex", values = "state", formats = list(sex = sex., state = state.))),
    function() suppressMessages(transpose_plus(dummy_df, pivot = "sex", values = "state", formats = list("&sex_variable" = sex., state = "&state_format"))))

check_macro(
    "transpose_plus formats with literal variable name and macro format name",
    function() suppressMessages(transpose_plus(dummy_df, pivot = "sex", values = "state", formats = list(sex = sex., state = state.))),
    function() suppressMessages(transpose_plus(dummy_df, pivot = "sex", values = "state", formats = list(sex = "&sex_format", "&state_variable" = state.))))

###############################################################################
# recode.
###############################################################################

check_macro(
    "recode. formats with macro variable name and macro format name",
    function() recode.(dummy_df, sex = sex., state = state.),
    function() recode.(dummy_df, "&sex_variable" = "&sex_format", "&state_variable" = "&state_format"))

check_macro(
    "recode. formats with macro variable name and object format",
    function() recode.(dummy_df, sex = sex., state = state.),
    function() recode.(dummy_df, "&sex_variable" = sex., state = "&state_format"))

check_macro(
    "recode. formats with literal variable name and macro format name",
    function() recode.(dummy_df, sex = sex., state = state.),
    function() recode.(dummy_df, sex = "&sex_format", "&state_variable" = state.))

###############################################################################
# split_by
###############################################################################

check_macro(
    "split_by formats with macro variable name and macro format name",
    function() suppressMessages(split_by(dummy_df, sex, formats = list(sex = sex.))),
    function() suppressMessages(split_by(dummy_df, sex, formats = list("&sex_variable" = "&sex_format"))))

check_macro(
    "split_by formats with macro variable name and object format",
    function() suppressMessages(split_by(dummy_df, sex, formats = list(sex = sex.))),
    function() suppressMessages(split_by(dummy_df, sex, formats = list("&sex_variable" = sex.))))

check_macro(
    "split_by formats with literal variable name and macro format name",
    function() suppressMessages(split_by(dummy_df, sex, formats = list(sex = sex.))),
    function() suppressMessages(split_by(dummy_df, sex, formats = list(sex = "&sex_format"))))


set_no_print()
