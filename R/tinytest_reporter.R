#' Print Styled Tinytest Results
#'
#' @name reporter
#'
#' @description
#' Styles the results of tinytest to get a better visual overview.
#'
#' @param tiny_results The results produced by tinytest.
#' @param utf8 Whether to display complex characters or just plain text.
#'
#' @return
#' Returns the results.
#'
#' @examples
#' # Example results
#' result_file <- system.file("extdata", "qol_tinytest_results.fst", package = "qol")
#' results     <- load_file(dirname(result_file), basename(result_file))
#'
#' # Display results
#' results |> report_test_results()
#'
#' # Normally you would do this:
#' # tinytest::test_all("PATH TO PACKAGE") |> report_test_results()
#' # or
#' # tinytest::test_package("PACKAGE NAME", testdir = "inst/tinytest") |> report_test_results()
#'
#' # To test the whole package with the custom reporter use:
#' # test_package("qol")
#'
#' @rdname reporter
#'
#' @export
report_test_results <- function(tiny_results, utf8 = .qol_messages[["format"]][["utf8"]]){
    result_df <- as.data.frame(tiny_results)

    if (!all(c("result", "file", "call", "first", "short", "diff") %in% names(result_df))){
        print_message("ERROR", "Input file is not a tinytest result list. Reporting will be aborted.")

        return(invisible(tiny_results))
    }

    # Get base summary results
    total  <- collapse::fnrow(result_df)
    passed <- collapse::fsum(result_df[["result"]])
    failed <- total - passed

    # Set up a theme which is used to color the output
    if (utf8){
        symbol_total  <- "\ud83d\udcdd"
        symbol_pass   <- "\u2705"
        symbol_fail  <- "\u274c"
        symbol_script <- "\U{1f4c4} FILE:"
    }
    else{
        symbol_total  <- ""
        symbol_pass   <- ""
        symbol_fail   <- ""
        symbol_script <- "FILE:"
    }

    # Set up custom messages
    set_up_custom_message(ansi_icon = "\u274c",       text_icon = "X", type = ".FAIL",       color = "#C93F3F")
    set_up_custom_message(ansi_icon = "\ud83d\udd0d", text_icon = ">", type = ".PATH",       color = "#BEDD1A")
    set_up_custom_message(ansi_icon = "\u2699\ufe0f", text_icon = "^", type = ".CALL",       color = "#32CD32")
    set_up_custom_message(ansi_icon = "\U0001f4e6",   text_icon = "?", type = ".TYPE",       color = "#8F8DC7")
    set_up_custom_message(ansi_icon = "\u26a0\ufe0f", text_icon = "!", type = ".DIFFERENCE", color = "#FFC90E")

    # Set up main headline
    print_headline("[b][#63C2C9 Results][/b]: [tests] [b]TESTS:[/b] [total] | [pass] [b][#32CD32 PASSED:][/b] [passed] | [fail] [b][#C93F3F FAILED:][/b] [failed]",
                   tests = symbol_total, pass = symbol_pass, fail = symbol_fail,
                   total = total, passed = passed, failed = failed)

    # If there are no failed tests, then return
    if (failed == 0){
        print_headline("[pass] [b]All tests [#32CD32 PASSED][/b]", pass = symbol_pass)

        return(invisible(tiny_results))
    }

    # Loop through all files and print out failed tests per file
    test_files <- collapse::funique(result_df[["file"]])

    for (test_file in test_files){
        # Get number of failed tests in the current file. If there are none, skip the file.
        failed_df      <- result_df |> collapse::fsubset(file == test_file & result == FALSE)
        failed_in_file <- collapse::fnrow(failed_df)

        if (failed_in_file == 0){
            next
        }

        # Get full absolute path to test file
        full_path <- normalizePath(file.path("inst/tinytest", test_file), winslash = "/", mustWork = FALSE)

        # Set the headline for the file
        print_headline("[b][#63C2C9 {script}][/b] [b][test_file][/b]",
                       script = symbol_script, test_file = test_file,
                       line_char = "-")

        # Now loop through all failed tests and print the results per file
        for (i in seq_len(failed_in_file)){
            # Extract the info parameter from the expect call to use this as headline
            # and additionally print a clean call.
            expect_call <- failed_df[["call"]][i]
            info_text   <- regmatches(expect_call, regexec('info\\s*=\\s*["\']([^"\']+)["\']', expect_call))[[1]][2]
            expect_call <- gsub(',\\s*info\\s*=\\s*["\'].*?["\']', '', expect_call)

            # Get the calls line number
            line_number <- failed_df[["first"]][i]

            # Get fail type and the actual difference
            fail_type  <- failed_df[["short"]][i]
            difference <- unlist(strsplit(failed_df[["diff"]][i], "\n"))

            # Print out fail block
            print_message(".FAIL", "[info_text]", info_text = info_text)
            print_message(".PATH", 'Line: [line_number], file.edit("[full_path]")', line_number = line_number, full_path = full_path)
            print_message(".CALL", "[expect_call]", expect_call = expect_call)
            print_message(".TYPE", "[fail_type]", fail_type = fail_type)

            # Print out formatted difference text or as is depending on failure type
            if (startsWith(difference[1], "Found")){
                difference <- substr(difference, 1, getOption("width") * 3)
            }

            print_message(".DIFFERENCE", difference)
            cat("\n")
        }
    }

    print_headline("[b]END OF REPORT[/b]")

    invisible(tiny_results)
}


#' @description
#' [test_package()]: Runs all set up unit tests with tinytest and outputs the results with the
#' custom reporter.
#'
#' @param package_name Name of the package to test.
#' @param multithread FALSE by default. Whether to run tests multithreaded.
#' NOTE: To make this work you have to manually run [devtools::install()] first.
#'
#' @rdname reporter
#'
#' @export
test_package <- function(package_name,
                         multithread = FALSE){
    if (!requireNamespace("tinytest", quietly = TRUE)){
        print_message("NOTE", 'To use this function you have to install "tinytest": install.packages("tinytest")')

        return(invisible(NULL))
    }

    if (multithread){
        results <- tinytest::test_package("qol", testdir = "inst/tinytest", ncpu = .qol_options[["threads"]])
    }
    else{
        results <- tinytest::test_package("qol", testdir = "inst/tinytest", color = TRUE)
    }

    # Clean up the temporary folder after testing
    test_dir <- tempdir()
    unlink(list.files(test_dir, full.names = TRUE), recursive = TRUE)

    results |> report_test_results()
}


#' @description
#' [test_single_file()]: Runs a single unit test file with tinytest.
#'
#' @param file_name Name of the file to test.
#'
#' @rdname reporter
#'
#' @export
test_single_file <- function(file_name){
    if (!requireNamespace("tinytest", quietly = TRUE)){
        print_message("NOTE", 'To use this function you have to install "tinytest": install.packages("tinytest")')

        return(invisible(NULL))
    }

    file_name <- tools::file_path_sans_ext(basename(file_name))

    results <- tinytest::run_test_file(paste0("inst/tinytest/test-", file_name, ".R"))

    # Clean up the temporary folder after testing
    test_dir <- tempdir()
    unlink(list.files(test_dir, full.names = TRUE), recursive = TRUE)

    results |> report_test_results()
}
