#' Set Global Styling Options For Excel Workbooks
#'
#' @name qol_options
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
#' @examples
#' set_style_options(save_path    = "C:/My Projects/",
#'                   sum_decimals = 8)
#'
#' @rdname qol_options
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
                  "box_font_bold", "box_borders", "title_font_bold", "footnote_font_bold")

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
                "box_back_color", "box_font_color", "box_border_color", "title_font_color", "footnote_font_color")

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
#' @rdname qol_options
#'
#' @export
reset_style_options <- function(){
    .qol_options[["excel_style"]] <- excel_output_style()

    invisible(.qol_options[["excel_style"]])
}


#' Get Global Styling Options For Excel Workbooks
#'
#' @description
#' [get_style_options()] prints out the currently set global styling options.
#'
#' @return
#' [get_style_options()]: Printed output.
#'
#' @examples
#' get_style_options()
#'
#' @rdname qol_options
#'
#' @export
get_style_options <- function(){
    print(.qol_options[["excel_style"]])
}
