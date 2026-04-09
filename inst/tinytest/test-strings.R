set_no_print(TRUE)

###############################################################################
# Suppressing some functions messages because they only output the information
# on how much time they took.
###############################################################################

test_df <- data.frame(
    var1 = c(1, 10, 100),
    var2 = c(1, NA, 3),
    var3 = c("abc", "ab", "a"))

text_df <- data.frame(
    var1 = c("This is a text", "Hello World", "this is a Text"))

text_df2 <- data.frame(
    var1 = c(" This  is  a  text ", " Hello  World ", " this  is  a  Text "),
    var2 = c(1, 2, 3))


# Concatenating multiple variables as provided
test_df[["concat_vec"]] <- test_df |> concat(var1, var2, var3)

expect_equal(test_df[["concat_vec"]], c("11abc", "10NAab", "1003a"), info = "Concatenating multiple variables as provided")


# Concatenating multiple variables with automatic padding
test_df[["concat_vec"]] <- test_df |> concat(var1, var2, var3,
                                padding_char = "0")

expect_equal(test_df[["concat_vec"]], c("0011abc", "01000ab", "100300a"), info = "Concatenating multiple variables with automatic padding")


# Concatenating multiple variables with individual padding length
test_df[["concat_vec"]] <- test_df |> concat(var1, var2, var3,
                                padding_char   = "0",
                                padding_length = c(5, 6, 7))

expect_equal(test_df[["concat_vec"]], c("000010000010000abc", "0001000000000000ab", "00100000003000000a"), info = "Concatenating multiple variables with individual padding length")


# Concatenating multiple variables with individual padding length and no padding character provided
test_df[["concat_vec"]] <- test_df |> concat(var1, var2, var3,
                                padding_length = c(5, 6, 7))

expect_equal(test_df[["concat_vec"]], c("    1     1    abc", "   10           ab", "  100     3      a"), info = "Concatenating multiple variables with individual padding length and no padding character provided")


# Concatenating multiple variables with individual padding length and too small values removes NA values
test_df[["concat_vec"]] <- test_df |> concat(var1, var2, var3,
                                padding_length = c(1, 1, 1))

expect_equal(test_df[["concat_vec"]], c("11abc", "10 ab", "1003a"), info = "Concatenating multiple variables with individual padding length and too small values removes NA values")


# Pad single variable
test_df[["concat_vec"]] <- test_df |> concat(var1, padding_char = "0")

expect_equal(test_df[["concat_vec"]], c("001", "010", "100"), info = "Pad single variable")


# Pad single variable with individual length
test_df[["concat_vec"]] <- test_df |> concat(var1, padding_char   = "_",
                                      padding_length = 5)

expect_equal(test_df[["concat_vec"]], c("____1", "___10", "__100"), info = "Pad single variable with individual length")


# Pad single variable on the right side
test_df[["concat_vec"]] <- test_df |> concat(var1, padding_char = "0", padding_right = TRUE)

expect_equal(test_df[["concat_vec"]], c("100", "100", "100"), info = "Pad single variable on the right side")


# Concatenating multiple variables with automatic padding on the right side
test_df[["concat_vec"]] <- test_df |> concat(var1, var2, var3,
                                padding_char = "0", padding_right = TRUE)

expect_equal(test_df[["concat_vec"]], c("1001abc", "1000ab0", "1003a00"), info = "Concatenating multiple variables with automatic padding on the right side")


# Substring from the left with numeric value
test_df[["sub_vec"]] <- text_df |> sub_string(var1, to = 3)

expect_equal(test_df[["sub_vec"]], c("Thi", "Hel", "thi"), info = "Substring from the left with numeric value")


# Substring from the right with numeric value
test_df[["sub_vec"]] <- text_df |> sub_string(var1, from = 5)

expect_equal(test_df[["sub_vec"]], c(" is a text", "o World", " is a Text"), info = "Substring from the right with numeric value")


# Substring in the middle with numeric values
test_df[["sub_vec"]] <- text_df |> sub_string(var1, from = 5, to = 10)

expect_equal(test_df[["sub_vec"]], c(" is a ", "o Worl", " is a "), info = "Substring in the middle with numeric values")


# Substring from the left with character value
test_df[["sub_vec"]] <- text_df |> sub_string(var1, to = "s")

expect_equal(test_df[["sub_vec"]], c("This", "Hello World", "this"), info = "Substring from the left with character value")


# Substring from the right with character value
test_df[["sub_vec"]] <- text_df |> sub_string(var1, from = "s")

expect_equal(test_df[["sub_vec"]], c("s is a text", "Hello World", "s is a Text"), info = "Substring from the right with character value")


# Substring in the middle with character values
test_df[["sub_vec"]] <- text_df |> sub_string(var1, from = "t", to = "s")

expect_equal(test_df[["sub_vec"]], c("", "Hello World", "this"), info = "Substring in the middle with character values")


# Substring in the middle with character values and no case sensitivity
test_df[["sub_vec"]] <- text_df |> sub_string(var1, from = "t", to = "s", case_sensitive = FALSE)

expect_equal(test_df[["sub_vec"]], c("This", "Hello World", "this"), info = "Substring in the middle with character values and no case sensitivity")


# Substring in the middle with character values and no case sensitivity
test_df[["sub_vec"]] <- text_df |> sub_string(var1, from = "t", to = "s", case_sensitive = FALSE)

expect_equal(test_df[["sub_vec"]], c("This", "Hello World", "this"), info = "Substring in the middle with character values and no case sensitivity")


# Remove leading blanks
text_df2[["gone_blanks"]] <- text_df2 |> remove_blanks(var1, which = "leading")

expect_equal(text_df2[["gone_blanks"]], c("This  is  a  text ", "Hello  World ", "this  is  a  Text "), info = "Remove leading blanks")


# Remove trailing blanks
text_df2[["gone_blanks"]] <- text_df2 |> remove_blanks(var1, which = "trailing")

expect_equal(text_df2[["gone_blanks"]], c(" This  is  a  text", " Hello  World", " this  is  a  Text"), info = "Remove trailing blanks")


# Remove leading and trailing blanks
text_df2[["gone_blanks"]] <- text_df2 |> remove_blanks(var1, which = "trim")

expect_equal(text_df2[["gone_blanks"]], c("This  is  a  text", "Hello  World", "this  is  a  Text"), info = "Remove leading and trailing blanks")


# Remove all blanks
text_df2[["gone_blanks"]] <- text_df2 |> remove_blanks(var1, which = "all")

expect_equal(text_df2[["gone_blanks"]], c("Thisisatext", "HelloWorld", "thisisaText"), info = "Remove all blanks")


# Normalize blanks
text_df2[["gone_blanks"]] <- text_df2 |> remove_blanks(var1, which = "normalize")

expect_equal(text_df2[["gone_blanks"]], c("This is a text", "Hello World", "this is a Text"), info = "Normalize blanks")

###############################################################################
# Warning checks
###############################################################################

# Concatenating only possible with single length padding_char
test_df[["concat_vec"]] <- test_df |> concat(var1, var2, var3,
                                padding_char = "00")

expect_warning(print_stack_as_messages("WARNING"), "<Padding chararacter> must be a single character. Concat will be done without <padding character>.",
               info = "Concatenating only possible with single length padding_char")

expect_equal(test_df[["concat_vec"]], c("11abc", "10NAab", "1003a"), info = "Concatenating only possible with single length padding_char")

test_df[["concat_vec"]] <- test_df |> concat(var1, var2, var3,
                                               padding_char = "")

expect_warning(print_stack_as_messages("WARNING"), "<Padding chararacter> must be a single character. Concat will be done without <padding character>.",
               info = "Concatenating only possible with single length padding_char")

expect_equal(test_df[["concat_vec"]], c("11abc", "10NAab", "1003a"), info = "Concatenating only possible with single length padding_char")


# Concatenating with too short individual padding length
test_df[["concat_vec"]] <- test_df |> concat(var1, var2, var3,
                                               padding_char   = "0",
                                               padding_length = c(5, 6))

expect_warning(print_stack_as_messages("WARNING"), "<Padding length> is shorter than the number of variables.",
               info = "Concatenating with too short individual padding length")

expect_equal(test_df[["concat_vec"]], c("00001000001abc", "000100000000ab", "0010000000300a"), info = "Concatenating with too short individual padding length")


# Concatenating with too long individual padding length
test_df[["concat_vec"]] <- test_df |> concat(var1, var2, var3,
                                padding_char   = "0",
                                padding_length = c(5, 6, 7, 8))

expect_warning(print_stack_as_messages("WARNING"), "<Padding length> is longer than the number of variables.",
               info = "Concatenating with too long individual padding length")

expect_equal(test_df[["concat_vec"]], c("000010000010000abc", "0001000000000000ab", "00100000003000000a"), info = "Concatenating with too long individual padding length")


# Substring throws a warning when variable is provided as a vector
test_df[["sub_vec"]] <- text_df |> sub_string(c(var1, var1), to = 3)

expect_warning(print_stack_as_messages("WARNING"), "<Variable> may only be of length one. The first Element will be used.",
               info = "Substring throws a warning when variable is provided as a vector")


# Substring throws a warning when to is provided as a vector
test_df[["sub_vec"]] <- text_df |> sub_string(var1, to = c(3, 5))

expect_warning(print_stack_as_messages("WARNING"), "<To> may only be of length one. The first Element will be used.",
               info = "Substring throws a warning when to is provided as a vector")


# Substring throws a warning when from is provided as a vector
test_df[["sub_vec"]] <- text_df |> sub_string(var1, from = c(3, 5))

expect_warning(print_stack_as_messages("WARNING"), "<From> may only be of length one. The first Element will be used.",
               info = "Substring throws a warning when from is provided as a vector")


# Remove all blanks with invalid <which> option
text_df2[["gone_blanks"]] <- text_df2 |> remove_blanks(var1, which = "lala")

expect_warning(print_stack_as_messages("WARNING"), "Invalid option for <which> provided. Allowed are 'trim', 'leading', 'trailing', 'all' and",
               info = "Remove all blanks with invalid <which> option")

expect_equal(text_df2[["gone_blanks"]], c("Thisisatext", "HelloWorld", "thisisaText"), info = "Remove all blanks with invalid <which> option")


# Remove blanks only of the first provided variable
text_df2[["gone_blanks"]] <- text_df2 |> remove_blanks(c(var1, var2), which = "all")

expect_warning(print_stack_as_messages("WARNING"), "<Variable> may only be of length one. The first Element will be used.",
               info = "Remove blanks only of the first provided variable")

expect_equal(text_df2[["gone_blanks"]], c("Thisisatext", "HelloWorld", "thisisaText"), info = "Remove blanks only of the first provided variable")

###############################################################################
# Abort checks
###############################################################################

# Substring aborts if variable is not part of the data frame
test_df[["sub_vec"]] <- text_df |> sub_string(var2, to = 3)

expect_error(print_stack_as_messages("ERROR"), "The provided <variable> '", info = "Substring aborts if variable is not part of the data frame")


# Substring aborts if variable is not character
test_df[["sub_vec"]] <- test_df |> sub_string(var1, to = 3)

expect_error(print_stack_as_messages("ERROR"), "<Variable> type must be character. Substring will be aborted.",
               info = "Substring aborts if variable is not character")


# Substring aborts if neither from nor to are provided
test_df[["sub_vec"]] <- text_df |> sub_string(var1)

expect_error(print_stack_as_messages("ERROR"), "Neither <from> nor <to> is provided. Substring will be aborted.",
               info = "Substring aborts if neither from nor to are provided")


# Abort blank removal with no variable provided
text_df2[["gone_blanks"]] <- text_df2 |> remove_blanks()

expect_error(print_stack_as_messages("ERROR"), "No <variable> provided. Blank removal will be aborted.",
               info = "Abort blank removal with no variable provided")


# Abort blank removal if variable is not part of the data frame
text_df2[["gone_blanks"]] <- text_df2 |> remove_blanks(var3)

expect_error(print_stack_as_messages("ERROR"), "No valid <variable> provided. Blank removal will be aborted.",
               info = "Abort blank removal if variable is not part of the data frame")


# Abort blank removal if variable is not character
text_df2[["gone_blanks"]] <- text_df2 |> remove_blanks(var2)

expect_error(print_stack_as_messages("ERROR"), "Blank removal only works with a character <variable>. Blank removal will be aborted.",
               info = "Abort blank removal if variable is not character")


set_no_print()
