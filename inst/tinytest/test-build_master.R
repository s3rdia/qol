set_no_print(TRUE)

# Test master building
temp_file <- tempfile(fileext = ".Rmd")

build_master(dir = dirname(temp_file),
             master_name = basename(tools::file_path_sans_ext(temp_file)))

expect_true(file.exists(temp_file), info = "Test master building")


# Abort master building on invalid directory
build_master(dir = "Test")
expect_error(print_stack_as_messages("ERROR"), "Directory '", info = "Abort master building on invalid directory")


set_no_print()
