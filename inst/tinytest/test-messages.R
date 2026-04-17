# Print normal messages utf8
test_func <- function(){
    print_start_message()
    print_message("NOTE",    "This is a note.",    utf8 = TRUE)
    print_message("WARNING", "This is a warning.", utf8 = TRUE)
    print_message("ERROR",   "This is an error.",  utf8 = TRUE)
    print_message("NEUTRAL", "This is neutral.",   utf8 = TRUE)
    print_closing()
}

test_func()

expect_equal(length(get_message_stack()), 6, info = "Print step messages")
expect_message(print_stack_as_messages("NOTE"),    "This is a note.",    info = "Print normal messages utf8")
expect_warning(print_stack_as_messages("WARNING"), "This is a warning.", info = "Print normal messages utf8")
expect_error(print_stack_as_messages("ERROR"),     "This is an error.",  info = "Print normal messages utf8")
expect_message(print_stack_as_messages("NEUTRAL"),  "This is neutral.",  info = "Print normal messages utf8")


# Print normal messages non-utf8
test_func <- function(){
    print_start_message()
    print_message("NOTE",    "This is a note.",    utf8 = FALSE)
    print_message("WARNING", "This is a warning.", utf8 = FALSE)
    print_message("ERROR",   "This is an error.",  utf8 = FALSE)
    print_message("NEUTRAL", "This is neutral.",   utf8 = FALSE)
    print_closing()
}

test_func()

expect_equal(length(get_message_stack()), 6, info = "Print step messages")
expect_message(print_stack_as_messages("NOTE"),    "This is a note.",    info = "Print normal messages non-utf8")
expect_warning(print_stack_as_messages("WARNING"), "This is a warning.", info = "Print normal messages non-utf8")
expect_error(print_stack_as_messages("ERROR"),     "This is an error.",  info = "Print normal messages non-utf8")
expect_message(print_stack_as_messages("NEUTRAL"), "This is neutral.",   info = "Print normal messages non-utf8")


# Print step messages utf8
test_func <- function(){
    print_start_message()
    print_headline("This is a headline.", utf8 = TRUE)
    print_step("GREY",  "Grey step.",     utf8 = TRUE)
    print_step("MAJOR", "Major step.",    utf8 = TRUE)
    print_step("MINOR", "Minor step.",    utf8 = TRUE)
    print_closing()
}

test_func()

expect_equal(length(get_message_stack()), 6, info = "Print step messages")
expect_message(print_stack_as_messages("GREY"),  "Grey step.",  info = "Print step messages utf8")
expect_message(print_stack_as_messages("MAJOR"), "Major step.", info = "Print step messages utf8")
expect_message(print_stack_as_messages("MINOR"), "Minor step.", info = "Print step messages utf8")


# Print step messages non-utf8
test_func <- function(){
    print_start_message()
    print_headline("This is a headline.", utf8 = FALSE)
    print_step("GREY",  "Grey step.",     utf8 = FALSE)
    print_step("MAJOR", "Major step.",    utf8 = FALSE)
    print_step("MINOR", "Minor step.",    utf8 = FALSE)
    print_closing()
}

test_func()

expect_equal(length(get_message_stack()), 6, info = "Print step messages")
expect_message(print_stack_as_messages("GREY"),  "Grey step.",  info = "Print step messages non-utf8")
expect_message(print_stack_as_messages("MAJOR"), "Major step.", info = "Print step messages non-utf8")
expect_message(print_stack_as_messages("MINOR"), "Minor step.", info = "Print step messages non-utf8")


# Print custom messages utf8
unicorn <- set_up_custom_message(text_bold      = TRUE,
                                 text_italic    = TRUE,
                                 text_underline = TRUE,
                                 text_color     = "#FF00FF")

test_func <- function(){
    print_start_message()
    print_message(unicorn, "This is a unicorn message.", utf8 = TRUE)
    print_step(unicorn,    "This is a unicorn step.",    utf8 = TRUE)
    print_closing()
}

test_func()

expect_equal(length(get_message_stack()), 4, info = "Print step messages")
expect_message(print_stack_as_messages("UNICORN"), "This is a unicorn message.", info = "Print custom messages utf8")
expect_message(print_stack_as_messages("UNICORN"), "This is a unicorn step.",    info = "Print custom messages utf8")


# Print custom messages non-utf8
unicorn <- set_up_custom_message()

test_func <- function(){
    print_start_message()
    print_message(unicorn, "This is a unicorn message.", utf8 = FALSE)
    print_step(unicorn,    "This is a unicorn step.",    utf8 = FALSE)
    print_closing()
}

test_func()

expect_equal(length(get_message_stack()), 4, info = "Print step messages")
expect_message(print_stack_as_messages("UNICORN"), "This is a unicorn message.", info = "Print custom messages non-utf8")
expect_message(print_stack_as_messages("UNICORN"), "This is a unicorn step.",    info = "Print custom messages non-utf8")


# Print messages without start and end only return a message stack of length 1
test_func <- function(){
    print_message("NOTE",    "This is a note.",    utf8 = TRUE)
    print_message("WARNING", "This is a warning.", utf8 = TRUE)
    print_message("ERROR",   "This is an error.",  utf8 = TRUE)
}

test_func()

expect_equal(length(get_message_stack()), 1, info = "Print messages without start and end only reutnr a message stack of length 1")


# Print closing without start creates a fake start
test_func <- function(){
    print_message("NEUTRAL", "This is neutral.")
    print_closing()
}

test_func()

expect_equal(length(get_message_stack()), 3, info = "Print closing without start creates a fake start")


# Message stack resets after new function call
test_func <- function(){
    print_start_message()
    print_message("NEUTRAL", "This is neutral.")
    print_closing()
}

test_func()
test_func()

expect_equal(length(get_message_stack()), 3, info = "Message stack resets after new function call")


# Styling messages
print_message("NEUTRAL", "[b][i][u][#FF00FF This is styled.][/u][/i][/b].")

expect_message(print_stack_as_messages("NEUTRAL"), "This is styled.", info = "Styling messages")


# Printing message in singular
vars <- "var1"

print_message("NEUTRAL", "[?This/These] [?is a/are] variable[?s]: [vars].", vars = vars)

expect_message(print_stack_as_messages("NEUTRAL"), "This is a variable: var1.", info = "Printing message in singular")


# Printing message in plural
vars <- c("var1", "var2")

print_message("NEUTRAL", "[?This/These] [?is a/are] variable[?s]: [vars].", vars = vars)

expect_message(print_stack_as_messages("NEUTRAL"), "These are variables: var1, var2.", info = "Printing message in singular")


# Get message stack as data frame
expect_inherits(get_message_stack(as_data_frame = TRUE), "data.frame", info = "Get message stack as data frame")
