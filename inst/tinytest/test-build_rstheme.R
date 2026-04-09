set_no_print(TRUE)

# Test theme building
temp_file <- tempfile(fileext = ".rstheme")
on.exit(unlink(temp_file), add = TRUE)

build_rstheme(file_path  = dirname(temp_file),
              theme_name = basename(tools::file_path_sans_ext(temp_file)))

expect_true(file.exists(temp_file), info = "Test theme building")


# Abort theme building on invalid directory
build_rstheme(file_path = "Test")

expect_error(print_stack_as_messages("ERROR"), "Directory '", info = "Abort theme building on invalid directory")


set_no_print()
