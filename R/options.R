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
#'
#' @return
#' [set_style_options()]: Returns modified global styling options.
#'
#' @seealso
#' Functions that use global styling optionss: [any_table()], [frequencies()],
#' [crosstabs()].
#'
#' Functions that also use global variable labels: [export_with_style()].
#'
#' @examples
#' set_style_options(save_path    = "C:/My Projects/",
#'                   sum_decimals = 8)
#'
#' @rdname style_options
#'
#' @export
set_style_options <- function(...){
    # Translate ... into a list if possible
    style_list <- tryCatch({
        # Force evaluation to see if it exists
        list(...)
    }, error = function(e) {
        # Evaluation failed
        NULL
    })

    if (is.null(style_list)){
        message(" X ERROR: Unknown object found. See 'excel_output_style()' and 'number_format_style()'\n",
                "          for valid function parameters. Global style remains unchanged.")
        return(invisible(.qol_options[["excel_style"]]))
    }

    if (length(style_list) == 0){
        message(" X ERROR: Empty list found. See 'excel_output_style()' and 'number_format_style()'\n",
                "          for valid function parameters. Global style remains unchanged.")
        return(invisible(.qol_options[["excel_style"]]))
    }

    # Define expected types for validation
    number_numerics <- c("pct_decimals", "freq_decimals", "freq.g0_decimals", "sum_decimals", "sum.wgt_decimals",
                         "mean_decimals", "median_decimals", "mode_decimals", "min_decimals", "max_decimals",
                         "sd_decimals", "variance_decimals", "first_decimals", "last_decimals", "p_decimals",
                         "missing_decimals")

    number_characters <- c("pct_exce", "freq_excel", "freq.g0_excel", "sum_excel", "sum.wgt_excel", "mean_excel",
                           "median_excel", "mode_excel", "min_excel", "max_excel", "sd_excel", "variance_excel",
                           "first_excel", "last_excel", "p_excel", "missing_excel")

    logicals <- c("freeze_col_header", "freeze_row_header", "filters", "grid_lines", "header_font_bold",
                  "header_borders", "cat_col_font_bold", "cat_col_borders", "table_font_bold", "table_borders",
                  "box_font_bold", "box_borders", "title_font_bold", "footnote_font_bold", "as_heatmap")

    numerics <- c("start_row", "start_column", "header_font_size", "header_indent",
                  "cat_col_font_size", "cat_col_indent", "table_font_size", "table_indent",
                  "box_font_size", "box_indent", "title_font_size", "footnote_font_size",
                  "title_heights", "header_heights", "table_heights", "footnote_heights",
                  "column_widths", "row_heights")

    characters  <- c("sheet_name", "font", "header_alignment", "header_wrap", "cat_col_alignment",
                     "cat_col_wrap", "table_alignment", "box_alignment", "box_wrap", "title_alignment",
                     "footnote_alignment", "na_symbol", "save_path", "file")

    colors <- c("header_back_color", "header_font_color", "header_border_color", "cat_col_back_color", "cat_col_font_color",
                "cat_col_border_color", "table_back_color", "table_font_color", "table_border_color",
                "box_back_color", "box_font_color", "box_border_color", "title_font_color", "footnote_font_color",
                "heatmap_low_color", "heatmap_middle_color", "heatmap_high_color")

    # Loop through passed arguments and check if they are of valid type
    for (style_option in names(style_list)){
        value <- style_list[[style_option]]

        if (style_option %in% logicals && !is.logical(value)){
            message(" ! WARNING: '", style_option, "' must be <logical>. Option will be omitted.")
            style_list[[style_option]] <- NULL
        }
        else if (style_option %in% c(numerics, number_numerics) && !is.numeric(value)){
            message(" ! WARNING: '", style_option, "' must be <numeric>. Option will be omitted.")
            style_list[[style_option]] <- NULL
        }
        else if (style_option %in% c(characters, number_characters) && !is.character(value)){
            message(" ! WARNING: '", style_option, "' must be <character>. Option will be omitted.")
            style_list[[style_option]] <- NULL
        }
        else if (style_option %in% colors && !grepl("^[A-Fa-f0-9]{6}$", value)){
            message(" ! WARNING: '", style_option, "' must be a 6 character <hex code>. Option will be omitted.")
            style_list[[style_option]] <- NULL
        }
        else if (!style_option %in% c(number_numerics, number_characters, logicals, numerics, characters, colors)){
            message(" ! WARNING: '", style_option, "' is not a valid style option. See 'excel_output_style()' and 'number_format_style()'\n",
                    "            for valid function parameters. Option will be omitted.")
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
    .qol_options[["excel_style"]] <- utils::modifyList(.qol_options[["excel_style"]], style_list)
    invisible(.qol_options[["excel_style"]])
}


#' Reset Global Styling Options For Excel Workbooks
#'
#' @description
#' [reset_style_options()] resets global style options to the default parameters.
#'
#' @return
#' [reset_style_options()]: Returns default global styling options.
#'
#' @examples
#' reset_style_options()
#'
#' @rdname style_options
#'
#' @export
reset_style_options <- function(){
    .qol_options[["excel_style"]] <- excel_output_style()
    .qol_options[["var_labels"]]  <- list()
    .qol_options[["stat_labels"]] <- list()

    invisible(.qol_options)
}


#' Get Global Styling Options For Excel Workbooks
#'
#' @description
#' [get_style_options()] prints out the currently set global styling options.
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
get_style_options <- function(){
    .qol_options[["excel_style"]]
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
#' close_file()
#'
#' @rdname style_options
#'
#' @export
close_file <- function(){
    .qol_options[["excel_style"]][["file"]] <- NULL

    invisible(.qol_options[["excel_style"]][["file"]])
}


#' Set Global Variable Labels
#'
#' @description
#' [set_variable_labels()]: Can set variable labels globally so that they don't
#' have to be provided in every output function separately.
#'
#' @return
#' [set_variable_labels()]: List of variable labels.
#'
#' @seealso
#' Functions that use global variable and statistic labels: [any_table()], [frequencies()],
#' [crosstabs()].
#'
#' Functions that also use global variable labels: [export_with_style()].
#'
#' @examples
#' set_variable_labels(age_gr = "Group of ages",
#'                     status = "Current status")
#'
#' @rdname style_options
#'
#' @export
set_variable_labels <- function(...){
    # Translate ... into a list if possible
    label_list <- tryCatch({
        # Force evaluation to see if it exists
        list(...)
    }, error = function(e) {
        # Evaluation failed
        NULL
    })

    if (is.null(label_list)){
        message(" X ERROR: Unknown object found. Global style remains unchanged.")
        return(invisible(.qol_options[["var_labels"]]))
    }

    if (length(label_list) == 0){
        message(" X ERROR: Empty list found. Global style remains unchanged.")
        return(invisible(.qol_options[["var_labels"]]))
    }

    .qol_options[["var_labels"]] <- label_list

    invisible(.qol_options[["var_labels"]])
}


#' Get Global Variable Labels
#'
#' @description
#' [get_variable_labels()]: Get the globally stored variable labels.
#'
#' @return
#' [get_variable_labels()]: List of variable labels.
#'
#' @examples
#' get_variable_labels()
#'
#' @rdname style_options
#'
#' @export
get_variable_labels <- function(){
    .qol_options[["var_labels"]]
}


#' Set Global Statistic Labels
#'
#' @description
#' [set_stat_labels()]: Can set statistic labels globally so that they don't
#' have to be provided in every output function separately.
#'
#' @return
#' [set_stat_labels()]: List of statistic labels.
#'
#' @examples
#' set_stat_labels(pct  = "%",
#'                 freq = "Count")
#'
#' @rdname style_options
#'
#' @export
set_stat_labels <- function(...){
    # Translate ... into a list if possible
    statistic_list <- tryCatch({
        # Force evaluation to see if it exists
        list(...)
    }, error = function(e) {
        # Evaluation failed
        NULL
    })

    if (is.null(statistic_list)){
        message(" X ERROR: Unknown object found. Global style remains unchanged.")
        return(invisible(.qol_options[["stat_labels"]]))
    }

    if (length(statistic_list) == 0){
        message(" X ERROR: Empty list found. Global style remains unchanged.")
        return(invisible(.qol_options[["stat_labels"]]))
    }

    .qol_options[["stat_labels"]] <- statistic_list

    invisible(.qol_options[["stat_labels"]])
}


#' Get Global Statistic Labels
#'
#' @description
#' [get_stat_labels()]: Get the globally stored statistic labels.
#'
#' @return
#' [get_stat_labels()]: List of statistic labels.
#'
#' @examples
#' get_stat_labels()
#'
#' @rdname style_options
#'
#' @export
get_stat_labels <- function(){
    .qol_options[["stat_labels"]]
}


#' Set Global Print Option
#'
#' @name qol_options
#'
#' @description
#' [set_print()]: Set the print option globally for the tabulation and export to
#' Excel functions.
#'
#' @param ... Put in TRUE or FALSE to activate or deactivate the option.
#'
#' @return
#' [set_print()]: Changed global print option.
#'
#' @examples
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
    }, error = function(e) {
        # Evaluation failed
        NULL
    })

    if (is.null(print_option)){
        message(" X ERROR: Unknown object found. Global option remains unchanged.")
        return(invisible(.qol_options[["print"]]))
    }

    if (!is.logical(print_option)){
        message(" X ERROR: Print option can only be TRUE or FALSE. Global option remains unchanged.")
        return(invisible(.qol_options[["print"]]))
    }

    .qol_options[["print"]] <- print_option

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
#' @return
#' [set_monitor()]: Changed global monitor option.
#'
#' @examples
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
    }, error = function(e) {
        # Evaluation failed
        NULL
    })

    if (is.null(monitor_option)){
        message(" X ERROR: Unknown object found. Global option remains unchanged.")
        return(invisible(.qol_options[["monitor"]]))
    }

    if (!is.logical(monitor_option)){
        message(" X ERROR: Monitor option can only be TRUE or FALSE. Global option remains unchanged.")
        return(invisible(.qol_options[["monitor"]]))
    }

    .qol_options[["monitor"]] <- monitor_option

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
#' @return
#' [set_na.rm()]: Changed global na.rm option.
#'
#' @examples
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
    }, error = function(e) {
        # Evaluation failed
        NULL
    })

    if (is.null(na_option)){
        message(" X ERROR: Unknown object found. Global option remains unchanged.")
        return(invisible(.qol_options[["na.rm"]]))
    }

    if (!is.logical(na_option)){
        message(" X ERROR: NA removal option can only be TRUE or FALSE. Global option remains unchanged.")
        return(invisible(.qol_options[["na.rm"]]))
    }

    .qol_options[["na.rm"]] <- na_option

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


#' Set Global Output Option
#'
#' @description
#' [set_output()]: Set the output option globally for each function that can output
#' results to "console", "text", "excel" or "excel_nostyle".
#'
#' @return
#' [set_output()]: Changed global output option.
#'
#' @examples
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
    }, error = function(e) {
        # Evaluation failed
        NULL
    })

    if (is.null(output_option)){
        message(" X ERROR: Unknown object found. Global option remains unchanged.")
        return(invisible(.qol_options[["output"]]))
    }

    if (!is.character(output_option)){
        message(" X ERROR: Output can only be 'console', 'text', 'excel' or 'excel_nostyle'. Global option remains unchanged.")
        return(invisible(.qol_options[["output"]]))
    }

    if (!tolower(output_option) %in% c("console", "text", "excel", "excel_nostyle")){
        message(" X ERROR: Output can only be 'console', 'text', 'excel' or 'excel_nostyle'. Global option remains unchanged.")
        return(invisible(.qol_options[["output"]]))
    }

    .qol_options[["output"]] <- tolower(output_option)

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
#' @return
#' [set_titles()]: Changed global titles.
#'
#' @examples
#' set_titles("This is title number 1 link: https://cran.r-project.org/",
#'            "This is title number 2",
#'            "This is title number 3")
#'
#' @rdname qol_options
#'
#' @export
set_titles <- function(...){
    # Translate ... into a list if possible
    titles_option <- tryCatch({
        # Force evaluation to see if it exists
        unlist(list(...))
    }, error = function(e) {
        # Evaluation failed
        "ERROR"
    })

    if (is.null(titles_option)){
        .qol_options[["titles"]] <- c()
        return(invisible(.qol_options[["titles"]]))
    }

    if (length(titles_option) == 1 && titles_option == "ERROR"){
        message(" X ERROR: Unknown object found. Global titles remain unchanged.")
        return(invisible(.qol_options[["titles"]]))
    }

    if (!is.character(titles_option)){
        message(" X ERROR: Titles must be provided as character. Global titles remain unchanged.")
        return(invisible(.qol_options[["titles"]]))
    }

    .qol_options[["titles"]] <- titles_option

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
    .qol_options[["titles"]]
}


#' Set Global Footnotes
#'
#' @description
#' [set_footnotes()]: Set the footnotes globally for each function that can print footnotes
#' above the output table.
#'
#' @return
#' [set_footnotes()]: Changed global footnotes.
#'
#' @examples
#' set_footnotes("This is title number 1 link: https://cran.r-project.org/",
#'            "This is title number 2",
#'            "This is title number 3")
#'
#' @rdname qol_options
#'
#' @export
set_footnotes <- function(...){
    # Translate ... into a list if possible
    footnotes_option <- tryCatch({
        # Force evaluation to see if it exists
        unlist(list(...))
    }, error = function(e) {
        # Evaluation failed
        "ERROR"
    })

    if (is.null(footnotes_option)){
        .qol_options[["footnotes"]] <- c()
        return(invisible(.qol_options[["footnotes"]]))
    }

    if (length(footnotes_option) == 1 && footnotes_option == "ERROR"){
        message(" X ERROR: Unknown object found. Global footnotes remain unchanged.")
        return(invisible(.qol_options[["footnotes"]]))
    }

    if (!is.character(footnotes_option)){
        message(" X ERROR: Footnotes must be provided as character. Global footnotes remain unchanged.")
        return(invisible(.qol_options[["footnotes"]]))
    }

    .qol_options[["footnotes"]] <- footnotes_option

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
    .qol_options[["footnotes"]]
}


#' Reset Global Options
#'
#' @description
#' [reset_qol_options()] resets global options to the default parameters.
#'
#' @return
#' [reset_qol_options()]: Returns default global options.
#'
#' @examples
#' reset_qol_options()
#'
#' @rdname style_options
#'
#' @export
reset_qol_options <- function(){
    .qol_options[["print"]]     <- TRUE
    .qol_options[["monitor"]]   <- FALSE
    .qol_options[["na.rm"]]     <- FALSE
    .qol_options[["output"]]    <- "console"
    .qol_options[["titles"]]    <- c()
    .qol_options[["footnotes"]] <- c()

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
qol_news <- function() {
    utils::browseURL("https://s3rdia.github.io/qol/news/index.html")
}
