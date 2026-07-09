#' Set Global Styling Options For Excel Workbooks
#'
#' @name style_options
#'
#' @description
#' Modify Styling options for Excel workbooks. Available parameters can be seen in
#' [excel_output_style()] or [number_format_style()].
#'
#' [set_style_options()] sets the styling options for Excel workbooks globally.These
#' options are used by all tabulation and output functions, which are capable of
#' exporting styled outputs.
#'
#' @param ... Put in any styling option from [excel_output_style()] or [number_format_style()]
#' with the new value.
#' @param save_file A full file path to an RDS file in which global style options
#' should be stored.
#'
#' @return
#' [set_style_options()]: Returns modified global styling options.
#'
#' @seealso
#' Functions that use global styling options: [any_table()], [frequencies()],
#' [crosstabs()], [export_with_style()].
#'
#' @examples
#' # This function can process any parameter from excel_output_style() or
#' # number_format_style() and sets the option globally.
#' set_style_options(save_path    = "C:/My Projects/",
#'                   sum_decimals = 8)
#'
#' @rdname style_options
#'
#' @export
set_style_options <- function(..., save_file = NULL){
    # Translate ... into a list if possible
    style_list <- tryCatch({
        # Force evaluation to see if it exists
        list(...)
    }, error = function(e){
        # Evaluation failed
        NULL
    })

    if (is.null(style_list)){
        print_message("ERROR", c("Unknown object found. See 'excel_output_style()' and 'number_format_style()'",
								 "for valid function parameters. Global style remains unchanged."))
        return(invisible(.qol_options[["excel_style"]]))
    }

    if (length(style_list) == 0){
        print_message("ERROR", c("Empty list found. See 'excel_output_style()' and 'number_format_style()'",
								 "for valid function parameters. Global style remains unchanged."))
        return(invisible(.qol_options[["excel_style"]]))
    }

    # Define expected types for validation
    number_numerics <- c("pct_decimals", "freq_decimals", "freq.g0_decimals", "sum_decimals", "sum.wgt_decimals",
                         "mean_decimals", "median_decimals", "mode_decimals", "min_decimals", "max_decimals",
                         "sd_decimals", "variance_decimals", "first_decimals", "last_decimals", "p_decimals",
                         "missing_decimals")

    number_characters <- c("pct_excel", "freq_excel", "freq.g0_excel", "sum_excel", "sum.wgt_excel", "mean_excel",
                           "median_excel", "mode_excel", "min_excel", "max_excel", "sd_excel", "variance_excel",
                           "first_excel", "last_excel", "p_excel", "missing_excel")

    logicals <- c("freeze_col_header", "freeze_row_header", "filters", "grid_lines", "by_as_subheaders", "header_font_bold",
                  "header_borders", "cat_col_font_bold", "cat_col_borders", "table_font_bold", "table_borders",
                  "box_font_bold", "box_borders", "title_font_bold", "footnote_font_bold", "as_heatmap")

    numerics <- c("start_row", "start_column", "header_font_size", "header_indent", "subheader_font_size", "subheader_indent",
                  "cat_col_font_size", "cat_col_indent", "table_font_size", "table_indent",
                  "box_font_size", "box_indent", "title_font_size", "footnote_font_size",
                  "title_heights", "header_heights", "subheader_heights", "table_heights", "footnote_heights",
                  "column_widths", "row_heights")

    characters  <- c("sheet_name", "font", "header_alignment", "header_wrap", "subheader_alignment", "subheader_wrap",
                     "cat_col_alignment", "cat_col_wrap", "table_alignment", "box_alignment", "box_wrap", "title_alignment",
                     "footnote_alignment", "na_symbol", "save_path", "file", "header_stat_merging")

    colors <- c("header_back_color", "header_font_color", "header_border_color", "subheader_back_color", "subheader_font_color",
                "subheader_border_color", "cat_col_back_color", "cat_col_font_color", "cat_col_border_color", "table_back_color",
                "table_font_color", "table_border_color", "box_back_color", "box_font_color", "box_border_color", "title_font_color",
                "footnote_font_color", "heatmap_low_color", "heatmap_middle_color", "heatmap_high_color", "background_color")

    # Loop through passed arguments and check if they are of valid type
    for (style_option in names(style_list)){
        value <- style_list[[style_option]]

        if (style_option %in% logicals && !all(is.logical(value))){
            print_message("WARNING", "'[style_option]' must be <logical>. Option will be omitted.", style_option = style_option)
            style_list[[style_option]] <- NULL
        }
        else if (style_option %in% c(numerics, number_numerics) && !all(is.numeric(value))){
            print_message("WARNING", "'[style_option]' must be <numeric>. Option will be omitted.", style_option = style_option)
            style_list[[style_option]] <- NULL
        }
        else if (style_option %in% c(characters, number_characters) && !all(is.character(value))){
            print_message("WARNING", "'[style_option]' must be <character>. Option will be omitted.", style_option = style_option)
            style_list[[style_option]] <- NULL
        }
        else if (style_option %in% colors && !all(grepl("^[A-Fa-f0-9]{6}$", value)) && value != ""){
            print_message("WARNING", "'[style_option]' must be a 6 character <hex code>. Option will be omitted.", style_option = style_option)
            style_list[[style_option]] <- NULL
        }
        else if (!style_option %in% c(number_numerics, number_characters, logicals, numerics, characters, colors)){
            print_message("WARNING", c("'[style_option]' is not a valid style option. See 'excel_output_style()' and 'number_format_style()'",
                                       "for valid function parameters. Option will be omitted."), style_option = style_option)
            style_list[[style_option]] <- NULL
        }
    }

    # Split up the list into the number formats and the rest. The number formats are a
    # list inside the list of style elements. So the number format list is added as full
    # list back again
    number_fmt_list <- style_list[names(style_list) %in% c(number_characters, number_numerics)]
    style_list      <- style_list[setdiff(names(style_list), names(number_fmt_list))]

    if (!length(number_fmt_list) == 0){
        style_list[["number_formats"]] <- number_fmt_list
    }

    # Update the internal state
    if (length(style_list) > 0){
        .qol_options[["excel_style"]] <- utils::modifyList(.qol_options[["excel_style"]], style_list)

        print_message("NOTE", "Global style options successfully changed.")
    }

    # Save global style options as physical file
    if (!is.null(save_file)){
        if (!file.exists(dirname(save_file))){
            print_message("ERROR", "Path does not exist: [path]", path = save_file)
        }
        else{
            extension <- tolower(tools::file_ext(save_file))

            if (extension == ""){
                save_file <- paste0(save_file, ".rds")
                extension <- "rds"
            }

            if (!extension == "rds"){
                save_file <- gsub(extension, "rds", save_file)
            }

            saveRDS(.qol_options[["excel_style"]], file = save_file)

            print_message("NOTE", "Global style has been saved to: [save_file]",
                          save_file = save_file)
        }
    }

    invisible(.qol_options[["excel_style"]])
}


#' Get Global Styling Options For Excel Workbooks
#'
#' @description
#' [get_style_options()] Prints out the currently set global styling options.
#'
#' @param from_file A full file path to an RDS file in which global style options
#' are stored.
#'
#' @return
#' [get_style_options()]: List of global styling options.
#'
#' @examples
#' get_style_options()
#'
#' @rdname style_options
#'
#' @export
get_style_options <- function(from_file = NULL){
    # If a saved file is specified, check if it exists and read it in
    if (!is.null(from_file)){
        if (!file.exists(from_file)){
            print_message("ERROR", "File does not exist: [file]", file = from_file)
        }
        else{
            style_list <- readRDS(file = from_file)
            set_style_options(style_list)
        }
    }

    # To make the console output more readable the list will be formatted before
    # outputting it. First expand the number format list inside the excel style
    # list, to be able to handle all formatting in one go.
    style_list <- c(.qol_options[["excel_style"]], unlist(.qol_options[["excel_style"]]["number_formats"]))
    style_list[["number_formats"]] <- NULL
    style_names <- gsub("\\.", " - ", names(style_list))

    # Add padding to names to make the values display in one column
    padded_names <- sprintf(paste0("%-", max(nchar(style_names)), "s"), style_names)

    # Join complete lines and print
    output_vector <- paste0(padded_names, " : ", style_list)

    print_message("NEUTRAL", output_vector)

    # Return style list
    invisible(.qol_options[["excel_style"]])
}


#' Reset Global Styling Options For Excel Workbooks
#'
#' @description
#' [reset_style_options()] Resets global style options to the default parameters.
#' This includes all options set with [set_style_options()], [set_labels()],
#' [set_titles()] and [set_footnotes()].
#'
#' @return
#' [reset_style_options()]: Returns default global styling options.
#'
#' @examples
#' # Reset all the style options including variable and statistic labels as well
#' # as titles and footnotes.
#' reset_style_options()
#'
#' @rdname style_options
#'
#' @export
reset_style_options <- function(){
    .qol_options[["excel_style"]] <- excel_output_style()
    .qol_options[["var_labels"]]  <- list()
    .qol_options[["stat_labels"]] <- list()
    .qol_options[["titles"]]      <- c()
    .qol_options[["footnotes"]]   <- c()

    print_message("NOTE", c("Style options have been reset, this includes:",
                            "Everything from excel_output_style(), variable and statistic labels,",
                            "as well as titles and footnotes"))

    invisible(.qol_options)
}


#' Set File To NULL In Global Styling Options For Excel Workbooks
#'
#' @description
#' [close_file()] is a simple, more readable wrapper for setting file parameter to NULL.
#'
#' @return
#' [close_file()]: List of global styling options with file = NULL.
#'
#' @examples
#' # Reset the file parameter
#' close_file()
#'
#' @rdname style_options
#'
#' @export
close_file <- function(){
    print_message("NOTE", "File '[file]' has been closed.", file = .qol_options[["excel_style"]][["file"]])

    .qol_options[["excel_style"]][["file"]] <- NULL

    invisible(.qol_options[["excel_style"]][["file"]])
}


#' Set Global Variable Labels
#'
#' @description
#' [set_labels()]: Can set variable and statistic labels globally so that they don't
#' have to be provided in every output function separately.
#'
#' @param ... [set_labels()]: Put in the variable names and their respective labels.
#'
#' @return
#' [set_labels()]: List of variable and statistic labels.
#'
#' @seealso
#' Functions that use global variable and statistic labels: [any_table()], [frequencies()],
#' [crosstabs()].
#'
#' Functions that also use global variable labels: [export_with_style()].
#'
#' @examples
#' # Set variable and statistic labels globally, retrieve and reset them
#' set_labels(age_gr = "Group of ages",
#'            status = "Current status",
#'            pct    = "%")
#'
#' # To reset labels do this
#' set_labels(NULL)
#'
#' @rdname style_options
#'
#' @export
set_labels <- function(...){
    # Translate ... into a list if possible
    label_list <- tryCatch({
        # Force evaluation to see if it exists
        list(...)
    }, error = function(e){
        # Evaluation failed
        NULL
    })

    if (is.null(label_list)){
        .qol_options[["var_labels"]]  <- list()
        .qol_options[["stat_labels"]] <- list()

        print_message("NOTE", "Global labels have been removed.")

        return(invisible(list("var_labels"  = .qol_options[["var_labels"]],
                              "stat_labels" = .qol_options[["stat_labels"]])))
    }

    if (is.null(names(label_list))){
        print_message("ERROR", "List must have names to set labels. Global labels remain unchanged.")

        return(invisible(list("var_labels"  = .qol_options[["var_labels"]],
                              "stat_labels" = .qol_options[["stat_labels"]])))
    }

    if (length(label_list) == 0){
        print_message("ERROR", "Empty list found. Global labels remain unchanged.")
        return(invisible(list("var_labels"  = .qol_options[["var_labels"]],
                              "stat_labels" = .qol_options[["stat_labels"]])))
    }

    # Define explicit list of statistic names. Percentile names will be checked
    # inside the loop on the fly.
    statistics <- c("sum", "sum_wgt", "freq", "freq_g0", "pct", "mean", "median",
                    "mode", "min", "max", "sd", "variance", "first", "last", "pn",
                    "missing")

    # Check which label type is passed in the list and sort it to the according
    # label list.
    if (length(.qol_options[["var_labels"]]) == 0){
        .qol_options[["var_labels"]] <- list()
    }
    if (length(.qol_options[["stat_labels"]]) == 0){
        .qol_options[["stat_labels"]] <- list()
    }

    for (i in seq_len(length(label_list))){
        label <- names(label_list)[i]

        # Check if the name is in the explicit list OR matches the pattern p1 to p99
        is_statistic_label <- (label %in% statistics) || grepl("^p[1-9][0-9]?$", label)

        if (is_statistic_label){
            .qol_options[["stat_labels"]][[label]] <- label_list[[i]]
        }
        else{
            .qol_options[["var_labels"]][[label]] <- label_list[[i]]
        }
    }

    print_message("NOTE", "Labels where succesfully added to global label lists.")

    invisible(list("var_labels"  = .qol_options[["var_labels"]],
                   "stat_labels" = .qol_options[["stat_labels"]]))
}


#' Get Global Variable Labels
#'
#' @description
#' [get_labels()]: Get the globally stored variable and statistic labels as a list
#' and print the contents to the console.
#'
#' @return
#' [get_labels()]: List of variable and statistic labels.
#'
#' @examples
#' get_labels()
#'
#' @rdname style_options
#'
#' @export
get_labels <- function(){
    # To make the console output more readable the list will be formatted before
    # outputting it. First expand the number format list inside the excel style
    # list, to be able to handle all formatting in one go.
    label_list <- c(.qol_options[["var_labels"]], unlist(.qol_options[["stat_labels"]]))

    if (length(label_list) > 0){
        label_names <- names(label_list)

        # Add padding to names to make the values display in one column
        padded_names <- sprintf(paste0("%-", max(nchar(label_names)), "s"), label_names)

        # Join complete lines and print
        output_vector <- paste0(padded_names, " : ", label_list)

        print_message("NEUTRAL", output_vector)
    }

    # Return style list
    invisible(list("var_labels"  = .qol_options[["var_labels"]],
                   "stat_labels" = .qol_options[["stat_labels"]]))
}


#' Set Global Print Option
#'
#' @name qol_options
#'
#' @description
#' [set_print()]: Set the print option globally for the tabulation and export to
#' Excel functions.
#'
#' @param ... [set_print()]: Put in TRUE or FALSE to activate or deactivate the option.
#'
#' @return
#' [set_print()]: Changed global print option.
#'
#' @examples
#' # Set and get the global print option
#' set_print(FALSE)
#' set_print(TRUE)
#'
#' @rdname qol_options
#'
#' @export
set_print <- function(...){
    # Translate ... into a list if possible
    print_option <- tryCatch({
        # Force evaluation to see if it exists
        unlist(list(...))
    }, error = function(e){
        # Evaluation failed
        NULL
    })

    if (is.null(print_option)){
        print_message("ERROR", "Unknown object found. Global option remains unchanged.")
        return(invisible(.qol_options[["print"]]))
    }

    if (!is.logical(print_option)){
        print_message("ERROR", "Print option can only be TRUE or FALSE. Global option remains unchanged.")
        return(invisible(.qol_options[["print"]]))
    }

    .qol_options[["print"]] <- print_option

    print_message("NOTE", "Global print option is now [option].", option = print_option)

    invisible(.qol_options[["print"]])
}


#' Get Global Print Option
#'
#' @description
#' [get_print()]: Get the globally stored print option.
#'
#' @return
#' [get_print()]: TRUE or FALSE.
#'
#' @examples
#' get_print()
#'
#' @rdname qol_options
#'
#' @export
get_print <- function(){
    .qol_options[["print"]]
}


#' Set Global Monitor Option
#'
#' @description
#' [set_monitor()]: Set the monitor option globally for the heavier functions which are
#' able to show how they work internally.
#'
#' @param ... [set_monitor()]: Put in TRUE or FALSE to activate or deactivate the option.
#'
#' @return
#' [set_monitor()]: Changed global monitor option.
#'
#' @examples
#' # Set and get the global monitor option
#' set_monitor(TRUE)
#' set_monitor(FALSE)
#'
#' @rdname qol_options
#'
#' @export
set_monitor <- function(...){
    # Translate ... into a list if possible
    monitor_option <- tryCatch({
        # Force evaluation to see if it exists
        unlist(list(...))
    }, error = function(e){
        # Evaluation failed
        NULL
    })

    if (is.null(monitor_option)){
        print_message("ERROR", "Unknown object found. Global option remains unchanged.")
        return(invisible(.qol_options[["monitor"]]))
    }

    if (!is.logical(monitor_option)){
        print_message("ERROR", "Monitor option can only be TRUE or FALSE. Global option remains unchanged.")
        return(invisible(.qol_options[["monitor"]]))
    }

    .qol_options[["monitor"]] <- monitor_option

    print_message("NOTE", "Global monitor option is now [option].", option = monitor_option)

    invisible(.qol_options[["monitor"]])
}


#' Get Global Monitor Option
#'
#' @description
#' [get_monitor()]: Get the globally stored monitor option.
#'
#' @return
#' [get_monitor()]: TRUE or FALSE.
#'
#' @examples
#' get_monitor()
#'
#' @rdname qol_options
#'
#' @export
get_monitor <- function(){
    .qol_options[["monitor"]]
}


#' Set Global NA Removal Option
#'
#' @description
#' [set_na.rm()]: Set the na.rm option globally for each function which can remove
#' NA values.
#'
#' @param ... [set_na.rm()]: Put in TRUE or FALSE to activate or deactivate the option.
#'
#' @return
#' [set_na.rm()]: Changed global na.rm option.
#'
#' @examples
#' # Set and get the global NA removal option
#' set_na.rm(TRUE)
#' set_na.rm(FALSE)
#'
#' @rdname qol_options
#'
#' @export
set_na.rm <- function(...){
    # Translate ... into a list if possible
    na_option <- tryCatch({
        # Force evaluation to see if it exists
        unlist(list(...))
    }, error = function(e){
        # Evaluation failed
        NULL
    })

    if (is.null(na_option)){
        print_message("ERROR", "Unknown object found. Global option remains unchanged.")
        return(invisible(.qol_options[["na.rm"]]))
    }

    if (!is.logical(na_option)){
        print_message("ERROR", "NA removal option can only be TRUE or FALSE. Global option remains unchanged.")
        return(invisible(.qol_options[["na.rm"]]))
    }

    .qol_options[["na.rm"]] <- na_option

    print_message("NOTE", "Global NA removal option is now [option].", option = na_option)

    invisible(.qol_options[["na.rm"]])
}


#' Get Global NA Removal Option
#'
#' @description
#' [get_na.rm()]: Get the globally stored na.rm option.
#'
#' @return
#' [get_na.rm()]: TRUE or FALSE.
#'
#' @examples
#' get_na.rm()
#'
#' @rdname qol_options
#'
#' @export
get_na.rm <- function(){
    .qol_options[["na.rm"]]
}


#' Set Global Print Missing Categories Option
#'
#' @description
#' [set_print_miss()]: Set the print_miss option globally for each function which can display
#' missing categories.
#'
#' @param ... [set_print_miss()]: Put in TRUE or FALSE to activate or deactivate the option.
#'
#' @return
#' [set_print_miss()]: Changed global print_miss option.
#'
#' @examples
#' # Set and get the global print missing option
#' set_print_miss(TRUE)
#' set_print_miss(FALSE)
#'
#' @rdname qol_options
#'
#' @export
set_print_miss <- function(...){
    # Translate ... into a list if possible
    print_miss_option <- tryCatch({
        # Force evaluation to see if it exists
        unlist(list(...))
    }, error = function(e){
        # Evaluation failed
        NULL
    })

    if (is.null(print_miss_option)){
        print_message("ERROR", "Unknown object found. Global option remains unchanged.")
        return(invisible(.qol_options[["print_miss"]]))
    }

    if (!is.logical(print_miss_option)){
        print_message("ERROR", "Print missing categories option can only be TRUE or FALSE. Global option remains unchanged.")
        return(invisible(.qol_options[["print_miss"]]))
    }

    .qol_options[["print_miss"]] <- print_miss_option

    print_message("NOTE", "Global print missing option is now [option].", option = print_miss_option)

    invisible(.qol_options[["print_miss"]])
}


#' Get Global Print Missing Categories Option
#'
#' @description
#' [get_print_miss()]: Get the globally stored print_miss option.
#'
#' @return
#' [get_print_miss()]: TRUE or FALSE.
#'
#' @examples
#' get_print_miss()
#'
#' @rdname qol_options
#'
#' @export
get_print_miss <- function(){
    .qol_options[["print_miss"]]
}


#' Set Global Output Option
#'
#' @description
#' [set_output()]: Set the output option globally for each function that can output
#' results to "console", "text", "excel" or "excel_nostyle".
#'
#' @param ... [set_print_miss()]: Input option to set global output style.
#'
#' @return
#' [set_output()]: Changed global output option.
#'
#' @examples
#' # Set and get the global output option
#' set_output("excel")
#'
#' @rdname qol_options
#'
#' @export
set_output <- function(...){
    # Translate ... into a list if possible
    output_option <- tryCatch({
        # Force evaluation to see if it exists
        unlist(list(...))
    }, error = function(e){
        # Evaluation failed
        NULL
    })

    if (is.null(output_option)){
        print_message("ERROR", "Unknown object found. Global option remains unchanged.")
        return(invisible(.qol_options[["output"]]))
    }

    if (!is.character(output_option)){
        print_message("ERROR", "Output can only be 'console', 'text', 'excel' or 'excel_nostyle'. Global option remains unchanged.")
        return(invisible(.qol_options[["output"]]))
    }

    if (!tolower(output_option) %in% c("console", "text", "excel", "excel_nostyle")){
        print_message("ERROR", "Output can only be 'console', 'text', 'excel' or 'excel_nostyle'. Global option remains unchanged.")
        return(invisible(.qol_options[["output"]]))
    }

    .qol_options[["output"]] <- tolower(output_option)

    print_message("NOTE", "Global output option is now [option].", option = .qol_options[["output"]])

    invisible(.qol_options[["output"]])
}


#' Get Global Output Option
#'
#' @description
#' [get_output()]: Get the globally stored output option.
#'
#' @return
#' [get_output()]: Current output option as character.
#'
#' @examples
#' get_output()
#'
#' @rdname qol_options
#'
#' @export
get_output <- function(){
    .qol_options[["output"]]
}


#' Set Global Titles
#'
#' @description
#' [set_titles()]: Set the titles globally for each function that can print titles
#' above the output table.
#'
#' @param ... [set_titles()]: Put in the titles that should appear above tables.
#'
#' @return
#' [set_titles()]: Changed global titles.
#'
#' @examples
#' # Set and get table titles globally
#' set_titles("This is title number 1 link: https://cran.r-project.org/",
#'            "This is title number 2 cell: W22",
#'            "This is title number 3 file: C:/MyFolder/MyFile.docx",
#'            "This is title number 4")
#'
#' # To reset titles do
#' set_titles(NULL)
#'
#' @rdname qol_options
#'
#' @export
set_titles <- function(...){
    # Translate ... into a list if possible
    titles_option <- tryCatch({
        # Force evaluation to see if it exists
        unlist(list(...))
    }, error = function(e){
        # Evaluation failed
        "ERROR"
    })

    if (is.null(titles_option)){
        .qol_options[["titles"]] <- c()

        print_message("NOTE", "Global titles have been removed.")

        return(invisible(.qol_options[["titles"]]))
    }

    if (length(titles_option) == 1 && titles_option == "ERROR"){
        print_message("ERROR", "Unknown object found. Global titles remain unchanged.")
        return(invisible(.qol_options[["titles"]]))
    }

    if (!is.character(titles_option)){
        print_message("ERROR", "Titles must be provided as character. Global titles remain unchanged.")
        return(invisible(.qol_options[["titles"]]))
    }

    .qol_options[["titles"]] <- titles_option

    print_message("NOTE", "Global titles have been successfully set.")

    invisible(.qol_options[["titles"]])
}


#' Get Global Titles
#'
#' @description
#' [get_titles()]: Get the globally stored titles.
#'
#' @return
#' [get_titles()]: Current titles as character.
#'
#' @examples
#' get_titles()
#'
#' @rdname qol_options
#'
#' @export
get_titles <- function(){
    print_message("NEUTRAL", c("", .qol_options[["titles"]], ""))

    invisible(.qol_options[["titles"]])
}


#' Set Global Footnotes
#'
#' @description
#' [set_footnotes()]: Set the footnotes globally for each function that can print footnotes
#' above the output table.
#'
#' @param ... [set_footnotes()]: Put in the footnotes that should appear below tables.
#'
#' @return
#' [set_footnotes()]: Changed global footnotes.
#'
#' @examples
#' # Set and get table footnotes globally
#' set_footnotes("This is footnote number 1 link: https://cran.r-project.org/",
#'               "This is footnote number 2 cell: W22",
#'               "This is footnote number 3 file: C:/MyFolder/MyFile.docx",
#'               "This is footnote number 4")
#'
#' # To reset footnotes do
#' set_footnotes(NULL)
#'
#' @rdname qol_options
#'
#' @export
set_footnotes <- function(...){
    # Translate ... into a list if possible
    footnotes_option <- tryCatch({
        # Force evaluation to see if it exists
        unlist(list(...))
    }, error = function(e){
        # Evaluation failed
        "ERROR"
    })

    if (is.null(footnotes_option)){
        .qol_options[["footnotes"]] <- c()

        print_message("NOTE", "Global footnotes have been removed.")

        return(invisible(.qol_options[["footnotes"]]))
    }

    if (length(footnotes_option) == 1 && footnotes_option == "ERROR"){
        print_message("ERROR", "Unknown object found. Global footnotes remain unchanged.")
        return(invisible(.qol_options[["footnotes"]]))
    }

    if (!is.character(footnotes_option)){
        print_message("ERROR", "Footnotes must be provided as character. Global footnotes remain unchanged.")
        return(invisible(.qol_options[["footnotes"]]))
    }

    .qol_options[["footnotes"]] <- footnotes_option

    print_message("NOTE", "Global footnotes have been successfully set.")

    invisible(.qol_options[["footnotes"]])
}


#' Get Global Footnotes
#'
#' @description
#' [get_footnotes()]: Get the globally stored footnotes.
#'
#' @return
#' [get_footnotes()]: Current footnotes as character.
#'
#' @examples
#' get_footnotes()
#'
#' @rdname qol_options
#'
#' @export
get_footnotes <- function(){
    print_message("NEUTRAL", c("", .qol_options[["footnotes"]], ""))

    invisible(.qol_options[["footnotes"]])
}


#' Set Global Number Of Used Threads
#'
#' @description
#' [set_threads()]: Globally sets the number of used threads for the save and load
#' file functions.
#'
#' @param ... [set_threads()]: Put in the number of threads to use or NULL to reset.
#'
#' @return
#' [set_threads()]: Changed global number of used threads.
#'
#' @examples
#' # Set and get used threads globally
#' set_threads(8)
#'
#' @rdname qol_options
#'
#' @export
set_threads <- function(...){
    # Translate ... into a list if possible
    threads_option <- tryCatch({
        # Force evaluation to see if it exists
        unlist(list(...))
    }, error = function(e){
        # Evaluation failed
        "ERROR"
    })

    if (is.null(threads_option)){
        .qol_options[["threads"]] <- fst::threads_fst(NULL)
        return(invisible(.qol_options[["threads"]]))
    }

    if (length(threads_option) == 1 && threads_option == "ERROR"){
        print_message("ERROR", "Unknown object found. Global number of used threads remains unchanged.")
        return(invisible(.qol_options[["threads"]]))
    }

    if (!is.numeric(threads_option)){
        print_message("ERROR", "Number of used threads must be provided as integer value. Global number of used threads remains unchanged.")
        return(invisible(.qol_options[["threads"]]))
    }

    .qol_options[["threads"]] <- as.integer(threads_option)

    print_message("NOTE", "Global number of used threads is now [option].", option = .qol_options[["threads"]])

    invisible(.qol_options[["threads"]])
}


#' Get Global Number Of Used Threads
#'
#' @description
#' [get_threads()]: Get the globally stored number of used threads.
#'
#' @return
#' [get_threads()]: Current number of used threads.
#'
#' @examples
#' get_threads()
#'
#' @rdname qol_options
#'
#' @export
get_threads <- function(){
    .qol_options[["threads"]]
}


#' Reset Global Options
#'
#' @description
#' [reset_qol_options()] Resets global options to the default parameters.
#'
#' @return
#' [reset_qol_options()]: Returns default global options.
#'
#' @examples
#' # Reset all globally set options
#' reset_qol_options()
#'
#' @rdname qol_options
#'
#' @export
reset_qol_options <- function(){
    .qol_options[["print"]]       <- TRUE
    .qol_options[["monitor"]]     <- FALSE
    .qol_options[["na.rm"]]       <- FALSE
    .qol_options[["print_miss"]]  <- FALSE
    .qol_options[["output"]]      <- "console"
    .qol_options[["excel_style"]] <- excel_output_style()
    .qol_options[["var_labels"]]  <- list()
    .qol_options[["stat_labels"]] <- list()
    .qol_options[["titles"]]      <- c()
    .qol_options[["footnotes"]]   <- c()
    .qol_options[["threads"]]     <- suppressMessages(fst::threads_fst(NULL))
    .qol_options[["graphic_visuals"]]     <- graphic_visuals()
    .qol_options[["graphic_axes"]]        <- graphic_axes()
    .qol_options[["graphic_dimensions"]]  <- graphic_dimensions()
    .qol_options[["graphic_output"]]      <- graphic_output()
    .qol_options[["graphic_fine_tuning"]] <- graphic_fine_tuning()

    print_message("NOTE", c("All qol package options have been reset."))

    invisible(.qol_options)
}


#' Go To GitHub NEWS Page
#'
#' @description
#' Opens browser and goes to the Github NEWS page
#'
#' @return
#' URL.
#'
#' @export
qol_news <- function(){
    utils::browseURL("https://s3rdia.github.io/qol/news/index.html")
}


#' Go To GitHub NEWS Page
#'
#' @description
#' Opens browser and goes to the DeepWiki page
#'
#' @return
#' URL.
#'
#' @export
qol_chat <- function(){
    utils::browseURL("https://deepwiki.com/s3rdia/qol")
}


###############################################################################
# Graphics
###############################################################################

#' Set Global Graphic Options
#'
#' @name graphic_options
#'
#' @description
#' Modify styling options used by [design_graphic()]. Available parameters can be seen in
#' [graphic_visuals()], [graphic_axes()], [graphic_dimensions()],
#' [graphic_output()] and [graphic_fine_tuning()].
#'
#' [set_graphic_options()] sets the graphic options globally. These
#' options are used by all functions involved in graphics production.
#'
#' @param ... Put in any graphic option from [graphic_visuals()], [graphic_axes()],
#' [graphic_dimensions()], [graphic_output()] or [graphic_fine_tuning()]
#' with the new value.
#' @param save_file A full file path to an RDS file in which global graphic options
#' should be stored.
#'
#' @return
#' [set_graphic_options()]: Returns modified global graphic options.
#'
#' @examples
#' # This function can process any parameter from graphic_visuals(), graphic_axes(),
#' # graphic_dimensions(), graphic_output() or graphic_fine_tuning() and sets the
#' # option globally.
#' set_graphic_options(title_font_size = 12,
#'                     label_font_size = 10)
#'
#' @rdname graphic_options
#'
#' @export
set_graphic_options <- function(..., save_file = NULL){
    # Translate ... into a list if possible
    style_list <- tryCatch({
        list(...)
    }, error = function(e){
        NULL
    })

    current_options <- list(.qol_options[["graphic_visuals"]],
                            .qol_options[["graphic_axes"]],
                            .qol_options[["graphic_dimensions"]],
                            .qol_options[["graphic_fine_tuning"]],
                            .qol_options[["graphic_number_formats"]],
                            .qol_options[["graphic_output"]])

    if (is.null(style_list)){
        print_message("ERROR", c("Unknown object found. See 'graphic_visuals()', 'graphic_axes()',",
                                 "'graphic_dimensions()', 'graphic_output()' and 'graphic_fine_tuning()'",
                                 "for valid function parameters. Global graphic options remain unchanged."))
        return(invisible(current_options))
    }

    if (length(style_list) == 0){
        print_message("ERROR", c("Empty list found. See 'graphic_visuals()', 'graphic_axes()',",
                                 "'graphic_dimensions()', 'graphic_output()' and 'graphic_fine_tuning()'",
                                 "for valid function parameters. Global graphic options remain unchanged."))
        return(invisible(current_options))
    }

    # Set up a vector for every option type to be able to sort them later back
    # to where they belong.
    visuals <- c("font", "color_theme", "color_usage", "theme_override",
                 "title_font_color", "footnote_font_color",
                 "primary_axes_font_color", "secondary_axes_font_color",
                 "variable_axes_font_color", "label_font_color",
                 "origin_font_color", "other_font_color",
                 "title_font_face", "footnote_font_face",
                 "primary_axes_font_face", "secondary_axes_font_face",
                 "variable_axes_font_face", "value_font_face",
                 "label_font_face", "origin_font_face", "other_font_face",
                 "title_alignment", "footnote_alignment", "hbar_alignment",
                 "other_alignment", "reverse_colors",
                 "primary_axes_color", "secondary_axes_color",
                 "variable_axes_color", "graphic_background_color",
                 "diagram_background_color", "graphic_border_color",
                 "diagram_border_color", "segment_border_color",
                 "line_markers", "line_markers_change",
                 "guiding_lines_y", "guiding_lines_x",
                 "guiding_line_type", "guiding_line_color",
                 "major_separation_line_type", "major_separation_line_color",
                 "minor_separation_line_type", "minor_separation_line_color",
                 "segment_line_type", "segment_line_color", "rotate_segment_labels",
                 "segment_label_rotation", "remove_small_values", "display_values",
                 "bar_values_inside", "rotate_values", "value_rotation", "display_plus_symbol",
                 "label_type", "label_group", "legend_x_pos", "legend_y_pos",
                 "legend_columns", "legend_symbol_size", "origin",
                 "tooltip_font_color", "tooltip_background_color",
                 "tooltip_border_color", "tooltip_border_width",
                 "tooltip_background_opacity", "tooltip_x_padding",
                 "tooltip_y_padding", "tooltip_corner_radius",
                 "segment_hover_opacity", "group_hover_color",
                 "group_hover_opacity")

    axes <- c("primary_axes_max", "primary_axes_min",
              "primary_axes_steps", "primary_axes_decimals",
              "primary_axes_big_mark", "primary_axes_decimal_mark",
              "primary_axes_prefix", "primary_axes_suffix",
              "primary_axes_scale",
              "primary_values_decimals", "primary_values_big_mark",
              "primary_values_decimal_mark", "primary_values_prefix",
              "primary_values_suffix",
              "secondary_axes_max", "secondary_axes_min",
              "secondary_axes_steps", "secondary_axes_decimals",
              "secondary_axes_big_mark", "secondary_axes_decimal_mark",
              "secondary_axes_prefix", "secondary_axes_suffix",
              "secondary_axes_scale",
              "secondary_values_decimals", "secondary_values_big_mark",
              "secondary_values_decimal_mark", "secondary_values_prefix",
              "secondary_values_suffix",
              "variable_axes_interval")

    dimensions <- c("graphic_width", "graphic_height",
                    "diagram_start_top", "diagram_width", "diagram_height",
                    "margins",
                    "title_font_size", "footnote_font_size",
                    "axes_font_size", "value_font_size",
                    "label_font_size", "origin_font_size",
                    "other_font_size", "tooltip_font_size",
                    "line_height", "space_between_bars", "bar_overlap",
                    "line_thickness", "segment_line_thickness",
                    "major_separation_line_thickness", "minor_separation_line_thickness",
                    "axes_line_thickness", "guiding_line_thickness",
                    "graphic_outline_thickness", "diagram_outline_thickness",
                    "segment_line_length", "segment_line_offset", "textbox_width")

    output_params <- c("save_path", "file", "resolution",
                       "by_as_grid", "interactive")

    fine_tuning <- c("diagram_margin",
                     "values_vjust_positive", "values_vjust_negative",
                     "values_vjust_90_positive", "values_vjust_90_negative",
                     "value_overlap_factor", "shrink_segment_width",
                     "values_hjust", "values_hjust_90",
                     "values_hjust_90_plus", "values_zero_line_offset",
                     "values_below_axes_just", "values_below_axes_90_just",
                     "tick_length", "value_axes_margin", "y_axes_scaling",
                     "swap_direction_threshold",
                     "segment_line_treshhold", "segment_label_hjust",
                     "max_segment_label_shift",
                     "cm_to_inch_factor", "svg_anchor_adjust",
                     "svg_line_height_adjust")

    # Define expected types for validation
    logicals <- c("reverse_colors", "line_markers", "line_markers_change",
                  "guiding_lines_y", "guiding_lines_x",
                  "remove_small_values", "display_values",
                  "bar_values_inside", "rotate_values", "display_plus_symbol",
                  "by_as_grid", "interactive", "rotate_segment_labels")

    numerics <- c("primary_axes_steps", "primary_axes_decimals",
                  "primary_axes_scale", "primary_values_decimals",
                  "secondary_axes_steps", "secondary_axes_decimals",
                  "secondary_axes_scale", "secondary_values_decimals",
                  "variable_axes_interval",
                  "graphic_width", "graphic_height",
                  "margins",
                  "title_font_size", "footnote_font_size",
                  "axes_font_size", "value_font_size",
                  "label_font_size", "origin_font_size",
                  "other_font_size", "tooltip_font_size",
                  "line_height", "space_between_bars", "bar_overlap",
                  "line_thickness", "segment_line_thickness", "major_separation_line_thickness",
                  "minor_separation_line_thickness", "axes_line_thickness",
                  "guiding_line_thickness", "graphic_outline_thickness",
                  "diagram_outline_thickness", "segment_line_length",
                  "segment_line_offset", "textbox_width",
                  "resolution", "legend_columns", "legend_symbol_size",
                  "tooltip_border_width", "tooltip_background_opacity",
                  "tooltip_x_padding", "tooltip_y_padding",
                  "tooltip_corner_radius", "segment_hover_opacity",
                  "group_hover_opacity",
                  "diagram_margin",
                  "values_vjust_positive", "values_vjust_negative",
                  "values_vjust_90_positive", "values_vjust_90_negative",
                  "value_overlap_factor", "shrink_segment_width",
                  "value_rotation", "values_hjust", "values_hjust_90",
                  "values_hjust_90_plus", "values_zero_line_offset",
                  "values_below_axes_just", "values_below_axes_90_just",
                  "tick_length", "value_axes_margin", "y_axes_scaling",
                  "swap_direction_threshold",
                  "segment_line_treshhold", "segment_label_hjust",
                  "max_segment_label_shift",
                  "cm_to_inch_factor", "svg_anchor_adjust",
                  "svg_line_height_adjust", "segment_label_rotation")

    characters <- c("font", "color_theme",
                    "title_font_face", "footnote_font_face",
                    "primary_axes_font_face", "secondary_axes_font_face",
                    "variable_axes_font_face", "value_font_face",
                    "label_font_face", "origin_font_face", "other_font_face",
                    "title_alignment", "footnote_alignment", "hbar_alignment",
                    "other_alignment",
                    "guiding_line_type",
                    "major_separation_line_type",
                    "minor_separation_line_type", "segment_line_type",
                    "label_type",
                    "origin",
                    "primary_axes_big_mark", "primary_axes_decimal_mark",
                    "primary_axes_prefix", "primary_axes_suffix",
                    "primary_values_big_mark", "primary_values_decimal_mark",
                    "primary_values_prefix", "primary_values_suffix",
                    "secondary_axes_big_mark", "secondary_axes_decimal_mark",
                    "secondary_axes_prefix", "secondary_axes_suffix",
                    "secondary_values_big_mark", "secondary_values_decimal_mark",
                    "secondary_values_prefix", "secondary_values_suffix",
                    "save_path", "file")

    colors <- c("title_font_color", "footnote_font_color",
                "primary_axes_font_color", "secondary_axes_font_color",
                "variable_axes_font_color", "label_font_color",
                "origin_font_color", "other_font_color",
                "primary_axes_color", "secondary_axes_color",
                "variable_axes_color",
                "graphic_background_color", "diagram_background_color",
                "graphic_border_color", "diagram_border_color",
                "segment_border_color",
                "guiding_line_color",
                "major_separation_line_color",
                "minor_separation_line_color", "segment_line_color",
                "tooltip_font_color", "tooltip_background_color",
                "tooltip_border_color", "group_hover_color")

    lists <- c("color_usage", "theme_override")

    flexible <- c("primary_axes_max", "primary_axes_min",
                  "secondary_axes_max", "secondary_axes_min",
                  "label_group", "legend_x_pos", "legend_y_pos",
                  "diagram_start_top", "diagram_width", "diagram_height")

    # Loop through passed arguments and check if they are of valid type
    for (option in names(style_list)){
        value <- style_list[[option]]

        if (option %in% logicals && !all(is.logical(value))){
            print_message("WARNING", "'[option]' must be <logical>. Option will be omitted.", option = option)
            style_list[[option]] <- NULL
        }
        else if (option %in% numerics && !all(is.numeric(value))){
            print_message("WARNING", "'[option]' must be <numeric>. Option will be omitted.", option = option)
            style_list[[option]] <- NULL
        }
        else if (option %in% characters && !all(is.character(value))){
            print_message("WARNING", "'[option]' must be <character>. Option will be omitted.", option = option)
            style_list[[option]] <- NULL
        }
        else if (option %in% colors && !all(grepl("^#[A-Fa-f0-9]{6}$", value)) && value != ""){
            print_message("WARNING", "'[option]' must be a 6 character <hex code>. Option will be omitted.", option = option)
            style_list[[option]] <- NULL
        }
        else if (option %in% lists && !is.list(value)){
            print_message("WARNING", "'[option]' must be a <list>. Option will be omitted.", option = option)
            style_list[[option]] <- NULL
        }
        else if (option %in% flexible && !(is.character(value) || is.numeric(value))){
            print_message("WARNING", "'[option]' must be <character> or <numeric>. Option will be omitted.", option = option)
            style_list[[option]] <- NULL
        }
        else if (!option %in% c(visuals, axes, dimensions, output_params, fine_tuning)){
            print_message("WARNING", c("'[option]' is not a valid graphic option.",
                                       "See 'graphic_visuals()', 'graphic_axes()',",
                                       "'graphic_dimensions()', 'graphic_output()' and 'graphic_fine_tuning()'",
                                       "for valid function parameters. Option will be omitted."),
                          option = option)
            style_list[[option]] <- NULL
        }
    }

    # Route parameters to their respective groups
    visuals_updated <- style_list[names(style_list) %in% visuals]
    axes_updated    <- style_list[names(style_list) %in% axes]
    dims_updated    <- style_list[names(style_list) %in% dimensions]
    output_updated  <- style_list[names(style_list) %in% output_params]
    ft_updated      <- style_list[names(style_list) %in% fine_tuning]

    # Update each sub-list in the global options
    if (length(visuals_updated) > 0){
        .qol_options[["graphic_visuals"]] <- utils::modifyList(.qol_options[["graphic_visuals"]], visuals_updated)

        # An unnamed list can't be updaten via modifyList, therefore do it directly afterwards
        if (!is.null(visuals_updated[["color_usage"]])){
            .qol_options[["graphic_visuals"]][["color_usage"]] <- visuals_updated[["color_usage"]]
        }
    }

    if (length(axes_updated) > 0){
        .qol_options[["graphic_axes"]] <- utils::modifyList(.qol_options[["graphic_axes"]], axes_updated)
    }

    if (length(dims_updated) > 0){
        .qol_options[["graphic_dimensions"]] <- utils::modifyList(.qol_options[["graphic_dimensions"]], dims_updated)
    }

    if (length(output_updated) > 0){
        .qol_options[["graphic_output"]] <- utils::modifyList(.qol_options[["graphic_output"]], output_updated)
    }

    if (length(ft_updated) > 0){
        .qol_options[["graphic_fine_tuning"]] <- utils::modifyList(.qol_options[["graphic_fine_tuning"]], ft_updated)
    }

    if (length(style_list) > 0){
        print_message("NOTE", "Global graphic options successfully changed.")
    }

    # Save global graphic options as physical file
    if (!is.null(save_file)){
        if (!file.exists(dirname(save_file))){
            print_message("ERROR", "Path does not exist: [path]", path = save_file)
        }
        else{
            extension <- tolower(tools::file_ext(save_file))

            if (extension == ""){
                save_file <- paste0(save_file, ".rds")
                extension <- "rds"
            }

            if (!extension == "rds"){
                save_file <- gsub(extension, "rds", save_file)
            }

            saveRDS(list("graphic_visuals"     = .qol_options[["graphic_visuals"]],
                         "graphic_axes"        = .qol_options[["graphic_axes"]],
                         "graphic_dimensions"  = .qol_options[["graphic_dimensions"]],
                         "graphic_output"      = .qol_options[["graphic_output"]],
                         "graphic_fine_tuning" = .qol_options[["graphic_fine_tuning"]]),
                    file = save_file)

            print_message("NOTE", "Global graphic options have been saved to: [save_file]",
                          save_file = save_file)
        }
    }

    invisible(list("graphic_visuals"     = .qol_options[["graphic_visuals"]],
                   "graphic_axes"        = .qol_options[["graphic_axes"]],
                   "graphic_dimensions"  = .qol_options[["graphic_dimensions"]],
                   "graphic_output"      = .qol_options[["graphic_output"]],
                   "graphic_fine_tuning" = .qol_options[["graphic_fine_tuning"]]))
}


#' Get Global Graphic Options
#'
#' @description
#' [get_graphic_options()] Prints out the currently set global graphic options.
#'
#' @param from_file A full file path to an RDS file in which global graphic options
#' are stored.
#'
#' @return
#' [get_graphic_options()]: List of global graphic options.
#'
#' @examples
#' get_graphic_options()
#'
#' @rdname graphic_options
#'
#' @export
get_graphic_options <- function(from_file = NULL){
    # If a saved file is specified, check if it exists and read it in
    if (!is.null(from_file)){
        if (!file.exists(from_file)){
            print_message("ERROR", "File does not exist: [file]", file = from_file)
        }
        else{
            graphic_list <- readRDS(file = from_file)
            set_graphic_options(graphic_list)
        }
    }

    # Collect all graphic option sub-lists
    current_options <- c(.qol_options[["graphic_visuals"]],
                         .qol_options[["graphic_axes"]],
                         .qol_options[["graphic_dimensions"]],
                         .qol_options[["graphic_output"]],
                         .qol_options[["graphic_fine_tuning"]])

    option_names <- names(current_options)

    # Add padding to names to make the values display in one column
    padded_names <- sprintf(paste0("%-", max(nchar(option_names)), "s"), option_names)

    # Join complete lines and print
    output_vector <- paste0(padded_names, " : ", current_options)

    print_message("NEUTRAL", output_vector)

    invisible(list("graphic_visuals"     = .qol_options[["graphic_visuals"]],
                   "graphic_axes"        = .qol_options[["graphic_axes"]],
                   "graphic_dimensions"  = .qol_options[["graphic_dimensions"]],
                   "graphic_output"      = .qol_options[["graphic_output"]],
                   "graphic_fine_tuning" = .qol_options[["graphic_fine_tuning"]]))
}


#' Reset Global Graphic Options
#'
#' @description
#' [reset_graphic_options()] Resets global graphic options to the default parameters.
#' This includes all options set with [set_graphic_options()], [set_labels()],
#' [set_titles()] and [set_footnotes()].
#'
#' @return
#' [reset_graphic_options()]: Returns default global graphic options.
#'
#' @examples
#' reset_graphic_options()
#'
#' @rdname graphic_options
#'
#' @export
reset_graphic_options <- function(){
    .qol_options[["graphic_visuals"]]     <- graphic_visuals()
    .qol_options[["graphic_axes"]]        <- graphic_axes()
    .qol_options[["graphic_dimensions"]]  <- graphic_dimensions()
    .qol_options[["graphic_output"]]      <- graphic_output()
    .qol_options[["graphic_fine_tuning"]] <- graphic_fine_tuning()
    .qol_options[["var_labels"]]          <- list()
    .qol_options[["stat_labels"]]         <- list()
    .qol_options[["titles"]]              <- c()
    .qol_options[["footnotes"]]           <- c()

    print_message("NOTE", c("Global graphic options have been reset."))

    invisible(list("graphic_visuals"     = .qol_options[["graphic_visuals"]],
                   "graphic_axes"        = .qol_options[["graphic_axes"]],
                   "graphic_dimensions"  = .qol_options[["graphic_dimensions"]],
                   "graphic_output"      = .qol_options[["graphic_output"]],
                   "graphic_fine_tuning" = .qol_options[["graphic_fine_tuning"]]))
}
