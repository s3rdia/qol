test_that("Modfiy Excel style", {
  my_style <- excel_output_style()
  my_style <- my_style |> modify_output_style(font = "Calibri")

  expect_equal(my_style[["font"]], "Calibri")
})


test_that("Add none existent element to style", {
    my_style <- excel_output_style()
    expect_message(my_style <- my_style |> modify_output_style(test = "Calibri"),
                   " ! WARNING: Style element 'test' is invalid and will")
})


test_that("Modfiy number format style", {
    my_num_fmt <- number_format_style()
    my_num_fmt <- my_num_fmt |> modify_number_formats(pct_decimals = 8)

    expect_equal(my_num_fmt[["pct_decimals"]], 8)
})


test_that("Modfiy number format style", {
    my_num_fmt <- number_format_style()
    expect_message(my_num_fmt <- my_num_fmt |> modify_number_formats(test = 8),
                   " ! WARNING: Number format 'test' is invalid and will")
})
