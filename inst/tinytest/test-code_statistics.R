# Print code statistics
temp_file <- tempfile(fileext = ".R")

writeLines("print('Hello World')", con = temp_file)

result_df <- code_statistics(tempdir())
expect_true(result_df[["total"]] == 1, info = "Print code statistics")

result_df <- code_statistics(tempdir(), output_per = "folder")
expect_true(result_df[["total"]] == 1, info = "Print code statistics")

result_df <- code_statistics(tempdir(), output_per = "file")
expect_true(result_df[["total"]] == 1, info = "Print code statistics")

unlink(temp_file)
