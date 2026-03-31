###############################################################################
# Suppressing some functions messages because they only output the information
# on how much time they took.
###############################################################################

dummy_df1 <- suppressMessages(dummy_data(100))
dummy_df2 <- suppressMessages(dummy_data(100))
dummy_df3 <- suppressMessages(dummy_data(100))

external_path <- system.file("extdata",  package = "qol")


# Stack data fames
new_df1 <- set(dummy_df1, dummy_df2, dummy_df3)
new_df2 <- set(dummy_df1, dummy_df2, dummy_df3, compress = "factor")

expect_equal(nrow(new_df1), 300, info = "Stack data fames")
expect_equal(nrow(new_df2), 300, info = "Stack data fames")
expect_equal(class(new_df1[["education"]]), "character", info = "Stack data fames")
expect_equal(class(new_df2[["education"]]), "factor", info = "Stack data fames")


# Stack data fames with id column
new_df <- set(dummy_df1, dummy_df2, dummy_df3, id = TRUE)

expect_equal(max(new_df[["ID"]]), 3, info = "Stack data fames with id column")


# Retrieve path with libname
expect_message(my_path <- libname(external_path), " > Path successfully assigned: ", info = "Retrieve path with libname")
expect_equal(my_path, external_path, info = "Retrieve path with libname")


# Retrieve files from path with libname
expect_message(my_path <- libname(external_path, get_files = TRUE), " > Filepaths successfully retrieved: ", info = "Retrieve files from path with libname")
expect_equal(names(my_path), c("qol_example_data_csv.csv",  "qol_example_data_fst.fst", "qol_example_data_rds.rds",
                               "qol_example_data_txt.txt", "qol_example_data_xlsx.xlsx", "qol_nuts.csv",
                               "qol_tinytest_results.fst"), info = "Retrieve files from path with libname")

###############################################################################
# Loading
###############################################################################

# Example fst and rds files exist
fst_file <- system.file("extdata", "qol_example_data_fst.fst", package = "qol")
rds_file <- system.file("extdata", "qol_example_data_rds.rds", package = "qol")

expect_true(file.exists(fst_file), info = "Example fst and rds files exist")
expect_true(file.exists(rds_file), info = "Example fst and rds files exist")


# Loading files with keep renames variables and reorders them
fst_file <- system.file("extdata", "qol_example_data_fst.fst", package = "qol")
rds_file <- system.file("extdata", "qol_example_data_rds.rds", package = "qol")

fst_keep <- load_file(dirname(fst_file), basename(fst_file), keep = c(Sex, aGe, STATE))
rds_keep <- load_file(dirname(rds_file), basename(rds_file), keep = c(Sex, aGe, STATE))

expect_equal(names(fst_keep), c("Sex", "aGe", "STATE"), info = "Loading files with keep renames variables and reorders them")
expect_equal(names(rds_keep), c("Sex", "aGe", "STATE"), info = "Loading files with keep renames variables and reorders them")


# Loading files with subset
fst_file <- system.file("extdata", "qol_example_data_fst.fst", package = "qol")
rds_file <- system.file("extdata", "qol_example_data_rds.rds", package = "qol")

fst_keep <- load_file(dirname(fst_file), basename(fst_file), where = first_person == 1)
rds_keep <- load_file(dirname(rds_file), basename(rds_file), where = first_person == 1)

expect_equal(collapse::funique(fst_keep[["first_person"]]), 1, info = "Loading files with subset")
expect_equal(collapse::funique(rds_keep[["first_person"]]), 1, info = "Loading files with subset")


# Loading multiple files and stack them
fst_file <- system.file("extdata", "qol_example_data_fst.fst", package = "qol")
rds_file <- system.file("extdata", "qol_example_data_rds.rds", package = "qol")

fst_df <- load_file(dirname(fst_file), basename(fst_file))
rds_df <- load_file(dirname(rds_file), basename(rds_file))
all_df <- load_file_multi(c(fst_file, rds_file))

expect_equal(names(fst_df), names(all_df), info = "Loading multiple files and stack them")
expect_equal(collapse::fnrow(fst_df) + collapse::fnrow(rds_df),
             collapse::fnrow(all_df), info = "Loading multiple files and stack them")


# Loading multiple files and return as list
fst_file <- system.file("extdata", "qol_example_data_fst.fst", package = "qol")
rds_file <- system.file("extdata", "qol_example_data_rds.rds", package = "qol")

fst_df <- load_file(dirname(fst_file), basename(fst_file))
rds_df <- load_file(dirname(rds_file), basename(rds_file))
all_df <- load_file_multi(c(fst_file, rds_file), stack_files = FALSE)

expect_equal(names(fst_df), names(all_df[[1]]), info = "Loading multiple files and return as list")
expect_equal(fst_df, all_df[[1]], info = "Loading multiple files and return as list")
expect_equal(rds_df, all_df[[2]], info = "Loading multiple files and return as list")


# Loading multiple files with keep and stack them
fst_file <- system.file("extdata", "qol_example_data_fst.fst", package = "qol")
rds_file <- system.file("extdata", "qol_example_data_rds.rds", package = "qol")

fst_df <- load_file(dirname(fst_file), basename(fst_file), keep = c("sex", "age"))
rds_df <- load_file(dirname(rds_file), basename(rds_file), keep = c("income", "state"))
all_df <- load_file_multi(c(fst_file, rds_file), keep = list(c("sex", "age"), c("income", "state")))

expect_equal(names(all_df), c("sex", "age", "income", "state"), info = "Loading multiple files with keep and stack them")
expect_equal(collapse::fnrow(fst_df) + collapse::fnrow(rds_df),
             collapse::fnrow(all_df), info = "Loading multiple files with keep and stack them")


# Loading multiple files with keep and return as list
fst_file <- system.file("extdata", "qol_example_data_fst.fst", package = "qol")
rds_file <- system.file("extdata", "qol_example_data_rds.rds", package = "qol")

fst_df <- load_file(dirname(fst_file), basename(fst_file), keep = c("sex", "age"))
rds_df <- load_file(dirname(rds_file), basename(rds_file), keep = c("income", "state"))
all_df <- load_file_multi(c(fst_file, rds_file),
                          keep        = list(c("sex", "age"), c("income", "state")),
                          stack_files = FALSE)

expect_equal(names(fst_df), names(all_df[[1]]), info = "Loading multiple files with keep and return as list")
expect_equal(fst_df, all_df[[1]], info = "Loading multiple files with keep and return as list")
expect_equal(rds_df, all_df[[2]], info = "Loading multiple files with keep and return as list")

###############################################################################
# Saving
###############################################################################

# Saving file works correctly
fst_file <- tempfile(fileext = ".fst")
rds_file <- tempfile(fileext = ".rds")
on.exit(unlink(c(fst_file, rds_file)), add = TRUE)

dummy_df1 |> save_file(dirname(fst_file), basename(fst_file))
dummy_df1 |> save_file(dirname(rds_file), basename(rds_file))

expect_true(file.exists(fst_file), info = "Saving file works correctly")
expect_true(file.exists(rds_file), info = "Saving file works correctly")

fst_df <- load_file(dirname(fst_file), basename(fst_file))
rds_df <- load_file(dirname(rds_file), basename(rds_file))

expect_equal(dummy_df1, fst_df, info = "Saving file works correctly")
expect_equal(dummy_df1, rds_df, info = "Saving file works correctly")


# Saving file with keep works correctly
fst_file <- tempfile(fileext = ".fst")
rds_file <- tempfile(fileext = ".rds")
on.exit(unlink(c(fst_file, rds_file)), add = TRUE)

dummy_df1 |> save_file(dirname(fst_file), basename(fst_file), keep = c(sex, age, state))
dummy_df1 |> save_file(dirname(rds_file), basename(rds_file), keep = c(sex, age, state))

expect_true(file.exists(fst_file), info = "Saving file with keep works correctly")
expect_true(file.exists(rds_file), info = "Saving file with keep works correctly")

fst_df <- load_file(dirname(fst_file), basename(fst_file))
rds_df <- load_file(dirname(rds_file), basename(rds_file))

expect_equal(names(fst_df), c("sex", "age", "state"), info = "Saving file with keep works correctly")
expect_equal(names(rds_df), c("sex", "age", "state"), info = "Saving file with keep works correctly")


# Saving file with subset works correctly
fst_file <- tempfile(fileext = ".fst")
rds_file <- tempfile(fileext = ".rds")
on.exit(unlink(c(fst_file, rds_file)), add = TRUE)

dummy_df1 |> save_file(dirname(fst_file), basename(fst_file), where = first_person == 1)
dummy_df1 |> save_file(dirname(rds_file), basename(rds_file), where = first_person == 1)

expect_true(file.exists(fst_file), info = "Saving file with subset works correctly")
expect_true(file.exists(rds_file), info = "Saving file with subset works correctly")

fst_df <- load_file(dirname(fst_file), basename(fst_file))
rds_df <- load_file(dirname(rds_file), basename(rds_file))

expect_equal(collapse::funique(fst_df[["first_person"]]), 1, info = "Saving file with subset works correctly")
expect_equal(collapse::funique(rds_df[["first_person"]]), 1, info = "Saving file with subset works correctly")


# Saving multiple files
fst_file <- tempfile(fileext = ".fst")
rds_file <- tempfile(fileext = ".rds")
on.exit(unlink(c(fst_file, rds_file)), add = TRUE)

save_file_multi(data_frame_list = list(dummy_df1, dummy_df2),
                file_list       = c(fst_file, rds_file))

expect_true(file.exists(fst_file), info = "Saving multiple files")
expect_true(file.exists(rds_file), info = "Saving multiple files")


# Saving multiple files with keep
fst_file <- tempfile(fileext = ".fst")
rds_file <- tempfile(fileext = ".rds")
on.exit(unlink(c(fst_file, rds_file)), add = TRUE)

save_file_multi(data_frame_list = list(dummy_df1, dummy_df2),
                file_list       = c(fst_file, rds_file),
                keep            = list(c("sex", "age"), c("income", "state")))

expect_true(file.exists(fst_file), info = "Saving multiple files with keep")
expect_true(file.exists(rds_file), info = "Saving multiple files with keep")

all_df <- load_file_multi(c(fst_file, rds_file), stack_files = FALSE)

expect_equal(names(all_df[[1]]), c("sex", "age"), info = "Saving multiple files with keep")
expect_equal(names(all_df[[2]]), c("income", "state"), info = "Saving multiple files with keep")

###############################################################################
# Warning checks
###############################################################################

# Loading throws warning when variables are not in loaded file
fst_file <- system.file("extdata", "qol_example_data_fst.fst", package = "qol")
rds_file <- system.file("extdata", "qol_example_data_rds.rds", package = "qol")

expect_message(load_file(dirname(fst_file), basename(fst_file), keep = "test"),
               " ! WARNING: Variables not found:", info = "Loading throws warning when variables are not in loaded file")
expect_message(load_file(dirname(rds_file), basename(rds_file), keep = "test"),
               " ! WARNING: Variables not found:", info = "Loading throws warning when variables are not in loaded file")


# Saving sets fst as file extension if it is otherwise invalid
test_dir <- tempdir()

file <- "test_file"

expected_path1 <- file.path(test_dir, paste0(file, "1.fst"))
expected_path2 <- file.path(test_dir, paste0(file, "2.fst"))

on.exit(unlink(c(expected_path1, expected_path2)), add = TRUE)

expect_message(dummy_df1 |> save_file(test_dir, paste0(file, "1")),
               " ! WARNING: No file extension provided in <file>. 'fst' will be used.", info = "Saving sets fst as file extension if it is otherwise invalid")
expect_message(dummy_df1 |> save_file(test_dir, paste0(file, "2.test")),
               " ! WARNING: Only 'fst' or 'rds' are allowed as file extensions in <file>.", info = "Saving sets fst as file extension if it is otherwise invalid")

expect_true(file.exists(expected_path1), info = "Saving sets fst as file extension if it is otherwise invalid")
expect_true(file.exists(expected_path2), info = "Saving sets fst as file extension if it is otherwise invalid")

###############################################################################
# Abort checks
###############################################################################

# Abort libname on invalid path
expect_message(my_path <- libname("Test"), " X ERROR: Path does not exist: ", info = "Abort libname on invalid path")


# Loading aborts with invalid file path
file <- system.file("extdata", "qol_example_data_fst.fst", package = "qol")

expect_message(load_file(dirname(file), "test.fst",
                         " X ERROR: File does not exist:"), info = "Loading aborts with invalid file path")


# Loading aborts with invalid file extension
file <- system.file("extdata", "qol_example_data_fst.fst", package = "qol")

expect_message(load_file(dirname(file), "test",
                         " X ERROR: No file extension provided in <file>."), info = "Loading aborts with invalid file extension")
expect_message(load_file(dirname(file), "test.test",
                         " X ERROR: Only 'fst' or 'rds' are allowed as file extensions in <file>."), info = "Loading aborts with invalid file extension")


# Saving file has write protection
fst_file <- tempfile(fileext = ".fst")
rds_file <- tempfile(fileext = ".rds")
on.exit(unlink(c(fst_file, rds_file)), add = TRUE)

dummy_df1 |> save_file(dirname(fst_file), basename(fst_file))
dummy_df1 |> save_file(dirname(rds_file), basename(rds_file))

expect_true(file.exists(fst_file), info = "Saving file has write protection")
expect_true(file.exists(rds_file), info = "Saving file has write protection")

expect_message(dummy_df1 |> save_file(dirname(fst_file), basename(fst_file)),
               " X ERROR: File already exists:", info = "Saving file has write protection")
expect_message(dummy_df1 |> save_file(dirname(rds_file), basename(rds_file)),
               " X ERROR: File already exists:", info = "Saving file has write protection")


# Saving file aborts with invalid keep variables
fst_file <- tempfile(fileext = ".fst")
on.exit(unlink(fst_file), add = TRUE)

expect_message(dummy_df1 |> save_file(dirname(fst_file), basename(fst_file), keep = "test"),
               " X ERROR: The provided variables to <keep>", info = "Saving file aborts with invalid keep variables")

expect_false(file.exists(fst_file), info = "Saving file aborts with invalid keep variables")


# Saving multiple files aborts if data frame and file list are of unequal lengths
fst_file <- tempfile(fileext = ".fst")
rds_file <- tempfile(fileext = ".rds")
on.exit(unlink(c(fst_file, rds_file)), add = TRUE)

expect_message(save_file_multi(data_frame_list = list(dummy_df1),
                               file_list       = c(fst_file, rds_file)),
               " X ERROR: Data frame and file list are of unequal lengths. Saving will be aborted.", info = "Saving multiple files aborts if data frame and file list are of unequal lengths")

expect_false(file.exists(fst_file), info = "Saving multiple files aborts if data frame and file list are of unequal lengths")
expect_false(file.exists(rds_file), info = "Saving multiple files aborts if data frame and file list are of unequal lengths")
