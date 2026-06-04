#' Analyze R Scripts And Print Out Statistics
#'
#' @description
#' [code_statistics()] reads in a folder or entire folder structure, grabs all
#' 'R' script files and scans the contents for different patterns. It then outputs
#' a small report which shows of which parts the code consists.
#'
#' @param paths A full folder path in which there are 'R' scripts to be analyzed. Can
#' be a single path or a vector of paths.
#' @param recursive FALSE by default. If TRUE scans the provided paths recursive
#' for sub folders and the 'R' scripts within them.
#' @param output_per "overall" by default. Determines how detailed the results are
#' printed. Valid options are "overall", "folder" or "file".
#'
#' @return
#' Returns the results as a data frame.
#'
#' @examples
#' # Run this function on a directory, actually containing R script files
#' # to see some results.
#' code_statistics(tempdir())
#'
#' @export
code_statistics <- function(paths,
                            recursive  = FALSE,
                            output_per = "overall"){
    output_per <- tolower(output_per)

    if (!output_per %in% c("overall", "folder", "file")){
        print_message("WARNING", c("'[output_per]' is not a valid option. Available are 'overall', 'folder'",
                                   "or 'file'. 'overall' will be used."), output_per = output_per)

        output_per <- "overall"
    }

    # Loop through all the paths and analyze the R scripts. Gather the results
    # in a data frame and row bind everything together to a master data frame.
    result_list <- list()

    for (path in paths){
        if (!dir.exists(path)){
            print_message("WARNING", c("Path does not exist: [path]",
                                       "and will be skipped."), path = path)
            next
        }

        # Get all the R scripts from the path, scan them for patterns and add
        # the results to the main result data frame
        r_scripts   <- libname(path,
                               get_files  = TRUE,
                               recursive  = recursive,
                               extensions = "R")
        result_list <- c(result_list, scan_files(r_scripts))
    }

    # Row bind together all results to one data frame
    result_df <- data.table::rbindlist(result_list, use.names = TRUE)

    if (collapse::fnrow(result_df) == 1 && result_df[["total"]] == 0){
        return(invisible(NULL))
    }

    # Transpose data frame to make tabulation easier
    transpose_df <- result_df |>
        collapse::fselect(-total) |>
        collapse::pivot(c("path", "file")) |>
        collapse::fsubset(value > 0)

    # Create format for table output
    categories. <- suppressMessages(discrete_format(
        "Actual code lines"            = "actual_code",
        "Comments"                     = "comments",
        "Roxygen documentation"        = "documentation",
        "Blank lines"                  = "blank_line",
        "Only bracket lines"           = "bracket_line",
        "String only lines"            = "strings",
        "<- function() lines"          = "declared_functions",
        "Exported functions (@export)" = "exported_functions",
        "Internal functions (@noRd)"   = "internal_functions",
        "Unit tests"                   = "unit_tests"))

    # Print results
    if (output_per == "overall"){
        print_headline("[b]Overall code statistics[/b]")
        print_message("NEUTRAL", c("\nPaths that were scanned:", paths))

        suppressMessages(transpose_df |>
                        frequencies(variables = "variable",
                                    weight    = "value",
                                    formats   = list("variable" = categories.),
                                    style     = excel_output_style(number_formats = number_format_style(sum_decimals = 0))))
    }
    else if (output_per == "folder"){
        subfolders <- collapse::funique(transpose_df[["path"]])

        print_headline("[b]Code statistics per folder[/b]")
        print_message("NEUTRAL", c("\nPaths that were scanned:", subfolders))

        suppressMessages(transpose_df |>
                        frequencies(variables = "variable",
                                    by        = "path",
                                    weight    = "value",
                                    formats   = list("variable" = categories.),
                                    style     = excel_output_style(number_formats = number_format_style(sum_decimals = 0))))
    }
    else if (output_per == "file"){
        subfolders <- collapse::funique(transpose_df[["path"]])
        files      <- collapse::funique(transpose_df[["file"]])

        print_headline("[b]Code statistics per file[/b]")
        print_message("NEUTRAL", c("\nPaths that were scanned:", subfolders))
        print_message("NEUTRAL", c("\nFiles that were scanned:", files))

        suppressMessages(transpose_df |>
                        frequencies(variables = "variable",
                                    by        = "file",
                                    weight    = "value",
                                    formats   = list("variable" = categories.),
                                    style     = excel_output_style(number_formats = number_format_style(sum_decimals = 0))))
    }

    result_df
}


#' @description
#' Analyzes the provided 'R' script files and scans the contents for different patterns.
#'
#' @param files Vector of full paths to 'R' script files.
#'
#' @noRd
scan_files <- function(files){
    # If no R files are present in the currently scanned folder, just return an
    # empty data frame.
    if (length(files) == 0){
        return(list(data.frame(total              = 0,
                               actual_code        = 0,
                               comments           = 0,
                               documentation      = 0,
                               blank_lines        = 0,
                               bracket_line       = 0,
                               strings            = 0,
                               declared_functions = 0,
                               exported_functions = 0,
                               internal_functions = 0,
                               unit_tests         = 0)))
    }

    # Capture the different patterns from the script file
    file_statistics <- lapply(files, function(file){
        # Read in complete file and trim all blanks
        trimmed_lines <- trimws(readLines(file, warn = FALSE))

        # Total number of code lines
        total_lines <- length(trimmed_lines)

        # Empty lines and empty roxygen lines
        blank_line_ids <- trimmed_lines == "" | grepl("^#'\\s*$", trimmed_lines)

        # Exported and internal functions based on the roxygen tags
        exported_ids <- grepl("^#'\\s*@export\\s*$", trimmed_lines)
        internal_ids <- grepl("^#'\\s*@noRd\\s*$",   trimmed_lines)

        # Actual roxygen documentation. Roxygen lines carrying text which is not
        # already included in one of the above patterns.
        documentation_ids <- grepl("^#'\\s*.+", trimmed_lines) &
                                   !exported_ids & !internal_ids & !blank_line_ids

        # Regular comments without roxygen comments
        comment_ids <- grepl("^#", trimmed_lines) & !grepl("^#'", trimmed_lines)

        # Bracket only lines and constructs such as "} else {". Keep this as a
        # mutually exclusive line class, so trailing comments are included here.
        bracket_ids <- grepl("^[][{}(),[:space:]]+([#].*)?$|^}[[:space:]]*else[[:space:]]*\\{?([#].*)?$",
                             trimmed_lines)

        # Pure character lines
        string_ids <- grepl('^".*"$', trimmed_lines) | grepl("^'.*'$", trimmed_lines)

        # The actual function declaration lines without the ones happening in the examples
        # on roxygen lines.
        function_ids <- grepl("<-[[:space:]]*function[[:space:]]*\\(", trimmed_lines) & !documentation_ids

        # Unit test expectations
        unit_test_ids <- grepl("\\b(expect|expect_)[A-Za-z0-9_]*\\s*\\(", trimmed_lines)

        # The actual code is everything not already covered by the above patterns
        code_ids <- !(blank_line_ids |
                      documentation_ids |
                      exported_ids |
                      internal_ids |
                      comment_ids |
                      bracket_ids |
                      string_ids |
                      function_ids |
                      unit_test_ids)

        data.table::data.table(path               = dirname(file),
                               file               = basename(file),
                               total              = total_lines,
                               actual_code        = collapse::fsum(code_ids),
                               comments           = collapse::fsum(comment_ids),
                               documentation      = collapse::fsum(documentation_ids),
                               blank_line         = collapse::fsum(blank_line_ids),
                               bracket_line       = collapse::fsum(bracket_ids),
                               strings            = collapse::fsum(string_ids),
                               declared_functions = collapse::fsum(function_ids),
                               exported_functions = collapse::fsum(exported_ids),
                               internal_functions = collapse::fsum(internal_ids),
                               unit_tests         = collapse::fsum(unit_test_ids))
    })

    file_statistics
}
