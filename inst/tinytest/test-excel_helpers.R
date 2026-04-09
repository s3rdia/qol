set_no_print(TRUE)

# Modfiy Excel style
my_style <- excel_output_style()
my_style <- my_style |> modify_output_style(font = "Calibri")

expect_equal(my_style[["font"]], "Calibri", info = "Modfiy Excel style")


# Add none existent element to style
my_style <- excel_output_style()
my_style <- my_style |> modify_output_style(test = "Calibri")

expect_warning(print_stack_as_messages("WARNING"), "Style element 'test' is invalid and will", info = "Add none existent element to style")


# Modfiy number format style
my_num_fmt <- number_format_style()
my_num_fmt <- my_num_fmt |> modify_number_formats(pct_decimals = 8)

expect_equal(my_num_fmt[["pct_decimals"]], 8, info = "Modfiy number format style")


# Modfiy number format style
my_num_fmt <- number_format_style()
my_num_fmt <- my_num_fmt |> modify_number_formats(test = 8)

expect_warning(print_stack_as_messages("WARNING"), "Number format 'test' is invalid and will", info = "Modfiy number format style")


set_no_print()
