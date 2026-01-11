test_that("Test master building", {
    temp_file <- tempfile(fileext = ".Rmd")
    on.exit(unlink(temp_file), add = TRUE)

    build_master(dir = dirname(temp_file),
                 master_name = basename(tools::file_path_sans_ext(temp_file)))

    expect_true(file.exists(temp_file))
})


test_that("Abort master building on invalid directory", {
    expect_message(build_master(dir = "Test"),
                    " X ERROR: Directory '")
})
