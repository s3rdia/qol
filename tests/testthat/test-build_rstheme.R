test_that("Test theme building", {
    temp_file <- tempfile(fileext = ".rstheme")
    on.exit(unlink(temp_file), add = TRUE)

    build_rstheme(file_path  = dirname(temp_file),
                  theme_name = basename(tools::file_path_sans_ext(temp_file)))

    expect_true(file.exists(temp_file))
})


test_that("Abort theme building on invalid directory", {
    expect_message(build_rstheme(file_path = "Test"),
                   " X ERROR: Directory '")
})
