###############################################################################
# Suppressing some functions messages because they only output the information
# on how much time they took.
###############################################################################

dummy_df1 <- suppressMessages(dummy_data(100))
dummy_df2 <- suppressMessages(dummy_data(100))
dummy_df3 <- suppressMessages(dummy_data(100))

external_path <- system.file("extdata",  package = "qol")


test_that("Stack data fames", {
    new_df1 <- set(dummy_df1, dummy_df2, dummy_df3)
    new_df2 <- set(dummy_df1, dummy_df2, dummy_df3, compress = "factor")

    expect_equal(nrow(new_df1), 300)
    expect_equal(nrow(new_df2), 300)
    expect_equal(class(new_df1[["education"]]), "character")
    expect_equal(class(new_df2[["education"]]), "factor")
})


test_that("Stack data fames with id column", {
    new_df <- set(dummy_df1, dummy_df2, dummy_df3, id = TRUE)

    expect_equal(max(new_df[["ID"]]), 3)
})


test_that("Retrieve path with libname", {
    expect_message(my_path <- libname(external_path), " > Path successfully assigned: ")
    expect_equal(my_path, external_path)
})


test_that("Retrieve files from path with libname", {
    expect_message(my_path <- libname(external_path, get_files = TRUE), " > Filepaths successfully retrieved: ")
    expect_equal(names(my_path), c("qol_example_data_csv.csv",  "qol_example_data_fst.fst",
                                   "qol_example_data_rds.rds", "qol_example_data_txt.txt",
                                   "qol_example_data_xlsx.xlsx", "qol_nuts.csv"))
})

###############################################################################
# Loading
###############################################################################

test_that("Example fst and rds files exist", {
    fst_file <- system.file("extdata", "qol_example_data_fst.fst", package = "qol")
    rds_file <- system.file("extdata", "qol_example_data_rds.rds", package = "qol")

    expect_true(file.exists(fst_file))
    expect_true(file.exists(rds_file))
})


test_that("Loading files with keep renames variables and reorders them", {
    fst_file <- system.file("extdata", "qol_example_data_fst.fst", package = "qol")
    rds_file <- system.file("extdata", "qol_example_data_rds.rds", package = "qol")

    fst_keep <- load_file(dirname(fst_file), basename(fst_file), keep = c(Sex, aGe, STATE))
    rds_keep <- load_file(dirname(rds_file), basename(rds_file), keep = c(Sex, aGe, STATE))

    expect_equal(names(fst_keep), c("Sex", "aGe", "STATE"))
    expect_equal(names(rds_keep), c("Sex", "aGe", "STATE"))
})


test_that("Loading files with subset", {
    fst_file <- system.file("extdata", "qol_example_data_fst.fst", package = "qol")
    rds_file <- system.file("extdata", "qol_example_data_rds.rds", package = "qol")

    fst_keep <- load_file(dirname(fst_file), basename(fst_file), where = first_person == 1)
    rds_keep <- load_file(dirname(rds_file), basename(rds_file), where = first_person == 1)

    expect_equal(collapse::funique(fst_keep[["first_person"]]), 1)
    expect_equal(collapse::funique(rds_keep[["first_person"]]), 1)
})


test_that("Loading multiple files and stack them", {
    fst_file <- system.file("extdata", "qol_example_data_fst.fst", package = "qol")
    rds_file <- system.file("extdata", "qol_example_data_rds.rds", package = "qol")

    fst_df <- load_file(dirname(fst_file), basename(fst_file))
    rds_df <- load_file(dirname(rds_file), basename(rds_file))
    all_df <- load_file_multi(c(fst_file, rds_file))

    expect_equal(names(fst_df), names(all_df))
    expect_equal(collapse::fnrow(fst_df) + collapse::fnrow(rds_df),
                 collapse::fnrow(all_df))
})


test_that("Loading multiple files and return as list", {
    fst_file <- system.file("extdata", "qol_example_data_fst.fst", package = "qol")
    rds_file <- system.file("extdata", "qol_example_data_rds.rds", package = "qol")

    fst_df <- load_file(dirname(fst_file), basename(fst_file))
    rds_df <- load_file(dirname(rds_file), basename(rds_file))
    all_df <- load_file_multi(c(fst_file, rds_file), stack_files = FALSE)

    expect_equal(names(fst_df), names(all_df[[1]]))
    expect_equal(fst_df, all_df[[1]])
    expect_equal(rds_df, all_df[[2]])
})


test_that("Loading multiple files with keep and stack them", {
    fst_file <- system.file("extdata", "qol_example_data_fst.fst", package = "qol")
    rds_file <- system.file("extdata", "qol_example_data_rds.rds", package = "qol")

    fst_df <- load_file(dirname(fst_file), basename(fst_file), keep = c("sex", "age"))
    rds_df <- load_file(dirname(rds_file), basename(rds_file), keep = c("income", "state"))
    all_df <- load_file_multi(c(fst_file, rds_file), keep = list(c("sex", "age"), c("income", "state")))

    expect_equal(names(all_df), c("sex", "age", "income", "state"))
    expect_equal(collapse::fnrow(fst_df) + collapse::fnrow(rds_df),
                 collapse::fnrow(all_df))
})


test_that("Loading multiple files with keep and return as list", {
    fst_file <- system.file("extdata", "qol_example_data_fst.fst", package = "qol")
    rds_file <- system.file("extdata", "qol_example_data_rds.rds", package = "qol")

    fst_df <- load_file(dirname(fst_file), basename(fst_file), keep = c("sex", "age"))
    rds_df <- load_file(dirname(rds_file), basename(rds_file), keep = c("income", "state"))
    all_df <- load_file_multi(c(fst_file, rds_file),
                              keep        = list(c("sex", "age"), c("income", "state")),
                              stack_files = FALSE)

    expect_equal(names(fst_df), names(all_df[[1]]))
    expect_equal(fst_df, all_df[[1]])
    expect_equal(rds_df, all_df[[2]])
})

###############################################################################
# Saving
###############################################################################

test_that("Saving file works correctly", {
    fst_file <- tempfile(fileext = ".fst")
    rds_file <- tempfile(fileext = ".rds")
    on.exit(unlink(c(fst_file, rds_file)), add = TRUE)

    dummy_df1 |> save_file(dirname(fst_file), basename(fst_file))
    dummy_df1 |> save_file(dirname(rds_file), basename(rds_file))

    expect_true(file.exists(fst_file))
    expect_true(file.exists(rds_file))

    fst_df <- load_file(dirname(fst_file), basename(fst_file))
    rds_df <- load_file(dirname(rds_file), basename(rds_file))

    expect_equal(dummy_df1, fst_df)
    expect_equal(dummy_df1, rds_df)
})


test_that("Saving file with keep works correctly", {
    fst_file <- tempfile(fileext = ".fst")
    rds_file <- tempfile(fileext = ".rds")
    on.exit(unlink(c(fst_file, rds_file)), add = TRUE)

    dummy_df1 |> save_file(dirname(fst_file), basename(fst_file), keep = c(sex, age, state))
    dummy_df1 |> save_file(dirname(rds_file), basename(rds_file), keep = c(sex, age, state))

    expect_true(file.exists(fst_file))
    expect_true(file.exists(rds_file))

    fst_df <- load_file(dirname(fst_file), basename(fst_file))
    rds_df <- load_file(dirname(rds_file), basename(rds_file))

    expect_equal(names(fst_df), c("sex", "age", "state"))
    expect_equal(names(rds_df), c("sex", "age", "state"))
})


test_that("Saving file with subset works correctly", {
    fst_file <- tempfile(fileext = ".fst")
    rds_file <- tempfile(fileext = ".rds")
    on.exit(unlink(c(fst_file, rds_file)), add = TRUE)

    dummy_df1 |> save_file(dirname(fst_file), basename(fst_file), where = first_person == 1)
    dummy_df1 |> save_file(dirname(rds_file), basename(rds_file), where = first_person == 1)

    expect_true(file.exists(fst_file))
    expect_true(file.exists(rds_file))

    fst_df <- load_file(dirname(fst_file), basename(fst_file))
    rds_df <- load_file(dirname(rds_file), basename(rds_file))

    expect_equal(collapse::funique(fst_df[["first_person"]]), 1)
    expect_equal(collapse::funique(rds_df[["first_person"]]), 1)
})


test_that("Saving multiple files", {
    fst_file <- tempfile(fileext = ".fst")
    rds_file <- tempfile(fileext = ".rds")
    on.exit(unlink(c(fst_file, rds_file)), add = TRUE)

    save_file_multi(data_frame_list = list(dummy_df1, dummy_df2),
                    file_list       = c(fst_file, rds_file))

    expect_true(file.exists(fst_file))
    expect_true(file.exists(rds_file))
})


test_that("Saving multiple files with keep", {
    fst_file <- tempfile(fileext = ".fst")
    rds_file <- tempfile(fileext = ".rds")
    on.exit(unlink(c(fst_file, rds_file)), add = TRUE)

    save_file_multi(data_frame_list = list(dummy_df1, dummy_df2),
                    file_list       = c(fst_file, rds_file),
                    keep            = list(c("sex", "age"), c("income", "state")))

    expect_true(file.exists(fst_file))
    expect_true(file.exists(rds_file))

    all_df <- load_file_multi(c(fst_file, rds_file), stack_files = FALSE)

    expect_equal(names(all_df[[1]]), c("sex", "age"))
    expect_equal(names(all_df[[2]]), c("income", "state"))
})

###############################################################################
# Warning checks
###############################################################################

test_that("Loading throws warning when variables are not in loaded file", {
    fst_file <- system.file("extdata", "qol_example_data_fst.fst", package = "qol")
    rds_file <- system.file("extdata", "qol_example_data_rds.rds", package = "qol")

    expect_message(load_file(dirname(fst_file), basename(fst_file), keep = "test"),
                   " ! WARNING: Variables not found:")
    expect_message(load_file(dirname(rds_file), basename(rds_file), keep = "test"),
                   " ! WARNING: Variables not found:")
})


test_that("Saving sets fst as file extension if it is otherwise invalid", {
    test_dir <- tempdir()

    file <- "test_file"

    expected_path1 <- file.path(test_dir, paste0(file, "1.fst"))
    expected_path2 <- file.path(test_dir, paste0(file, "2.fst"))

    on.exit(unlink(c(expected_path1, expected_path2)), add = TRUE)

    expect_message(dummy_df1 |> save_file(test_dir, paste0(file, "1")),
                   " ! WARNING: No file extension provided in <file>. 'fst' will be used.")
    expect_message(dummy_df1 |> save_file(test_dir, paste0(file, "2.test")),
                   " ! WARNING: Only 'fst' or 'rds' are allowed as file extensions in <file>.")

    expect_true(file.exists(expected_path1))
    expect_true(file.exists(expected_path2))
})

###############################################################################
# Abort checks
###############################################################################

test_that("Abort libname on invalid path", {
    expect_message(my_path <- libname("Test"), " X ERROR: Path does not exist: ")
})


test_that("Loading aborts with invalid file path", {
    file <- system.file("extdata", "qol_example_data_fst.fst", package = "qol")

    expect_message(load_file(dirname(file), "test.fst",
                             " X ERROR: File does not exist:"))
})


test_that("Loading aborts with invalid file extension", {
    file <- system.file("extdata", "qol_example_data_fst.fst", package = "qol")

    expect_message(load_file(dirname(file), "test",
                             " X ERROR: No file extension provided in <file>."))
    expect_message(load_file(dirname(file), "test.test",
                             " X ERROR: Only 'fst' or 'rds' are allowed as file extensions in <file>."))
})


test_that("Saving file has write protection", {
    fst_file <- tempfile(fileext = ".fst")
    rds_file <- tempfile(fileext = ".rds")
    on.exit(unlink(c(fst_file, rds_file)), add = TRUE)

    dummy_df1 |> save_file(dirname(fst_file), basename(fst_file))
    dummy_df1 |> save_file(dirname(rds_file), basename(rds_file))

    expect_true(file.exists(fst_file))
    expect_true(file.exists(rds_file))

    expect_message(dummy_df1 |> save_file(dirname(fst_file), basename(fst_file)),
                   " X ERROR: File already exists:")
    expect_message(dummy_df1 |> save_file(dirname(rds_file), basename(rds_file)),
                   " X ERROR: File already exists:")
})


test_that("Saving file aborts with invalid keep variables", {
    fst_file <- tempfile(fileext = ".fst")
    on.exit(unlink(fst_file), add = TRUE)

    expect_message(dummy_df1 |> save_file(dirname(fst_file), basename(fst_file), keep = "test"),
                   " X ERROR: The provided variables to <keep>")

    expect_false(file.exists(fst_file))
})


test_that("Saving multiple files aborts if data frame and file list are of unequal lengths", {
    fst_file <- tempfile(fileext = ".fst")
    rds_file <- tempfile(fileext = ".rds")
    on.exit(unlink(c(fst_file, rds_file)), add = TRUE)

    expect_message(save_file_multi(data_frame_list = list(dummy_df1),
                                   file_list       = c(fst_file, rds_file)),
                   " X ERROR: Data frame and file list are of unequal lengths. Saving will be aborted.")

    expect_false(file.exists(fst_file))
    expect_false(file.exists(rds_file))
})
