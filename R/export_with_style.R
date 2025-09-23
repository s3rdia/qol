#' Export Data Frame With Style
#'
#' @description
#' [export_with_style()] prints a data frame as an individually styled 'Excel' table. Titles,
#' footnotes and labels for variable names can optionally be added.
#'
#' @param data_frame A data frame to print.
#' @param titles Specify one or more table titles.
#' @param footnotes Specify one or more table footnotes.
#' @param var_labels A list in which is specified which label should be printed for
#' which variable instead of the variable name.
#' @param workbook Insert a previously created workbook to expand the sheets instead of
#' creating a new file.
#' @param style A list of options can be passed to control the appearance of excel outputs.
#' Styles can be created with [excel_output_style()].
#' @param output The following output formats are available: excel and excel_nostyle.
#' @param print TRUE by default. If TRUE prints the output, if FALSE doesn't print anything. Can be used
#' if one only wants to catch the output workbook.
#' @param monitor FALSE by default. If TRUE outputs two charts to visualize the functions time consumption.
#'
#' @details
#' [export_with_style()] is based on the 'SAS' procedure Proc Print, which outputs the data frame
#' as is into a styled table.
#'
#' @return
#' Returns a formatted 'Excel' workbook.
#'
#' @seealso
#' Creating a custom table style: [excel_output_style()], [modify_output_style()],
#' [number_format_style()], [modify_number_formats()].
#'
#' Functions that can handle styles: [frequencies()], [crosstabs()], [any_table()].
#'
#' @examples
#' # Example data frame
#' my_data <- dummy_data(1000)
#'
#' # Define style
#' my_style <- excel_output_style(column_widths = c(2, 15, 15, 15, 9))
#'
#' # Define titles and footnotes. If you want to add hyperlinks you can do so by
#' # adding "link:" followed by the hyperlink to the main text.
#' titles <- c("This is title number 1 link: https://cran.r-project.org/",
#'             "This is title number 2",
#'             "This is title number 3")
#' footnotes <- c("This is footnote number 1",
#'                "This is footnote number 2",
#'                "This is footnote number 3 link: https://cran.r-project.org/")
#'
#' # Print styled data frame
#' my_data |> export_with_style(titles    = titles,
#'                              footnotes = footnotes,
#'                              style     = my_style)
#'
#' # Retrieve formatted workbook for further usage
#' wb <- my_data |>
#'     export_with_style(titles    = titles,
#'                       footnotes = footnotes,
#'                       style     = my_style)
#'
#' @export
export_with_style <- function(data_frame,
                              titles     = c(),
                              footnotes  = c(),
                              var_labels = list(),
                              workbook   = NULL,
                              style      = excel_output_style(),
                              output     = "excel",
                              print      = TRUE,
                              monitor    = FALSE){
    start_time <- Sys.time()

    # Prepare table format for output
    monitor_df <- NULL |> monitor_start("Excel prepare", "Format")

    # Setup styling in new workbook if no other is provided
    if (is.null(workbook)){
        workbook <- openxlsx2::wb_workbook() |>
            prepare_styles(style)
    }
    # Update style options in provided workbook
    else{
        workbook <- workbook |>
            prepare_styles(style)
    }

    monitor_df <- monitor_df |> monitor_end()

    # Style data frame for export
    wb_list <- format_df_excel(workbook, data_frame, titles, footnotes, var_labels,
                               style, output, monitor_df)

    wb         <- wb_list[[1]]
    monitor_df <- wb_list[[2]]

    # Output formatted table into different formats
    if (print){
        monitor_df <- monitor_df |> monitor_next("Output tables", "Output tables")

        if (is.null(style[["file"]])){
            wb$open()
        }
        else{
            wb$save(file = style[["file"]], overwrite = TRUE)
        }
    }

    end_time <- round(difftime(Sys.time(), start_time, units = "secs"), 3)
    message("\n- - - 'export_with_style' execution time: ", end_time, " seconds\n")

    monitor_df <- monitor_df |> monitor_end()
    monitor_df |> monitor_plot(draw_plot = monitor)

    invisible(wb)
}


###############################################################################
# Format data frame for excel output
###############################################################################
#' Format Data Frame Output (Excel Based)
#'
#' @description
#' Format data frame according to the provided style options.
#'
#' @param wb An already created workbook to add more sheets to.
#' @param data_frame The data frame which contains the information for this cross
#' table..
#' @param titles Character vector of titles to display above the table.
#' @param footnotes Character vector of footnotes to display under the table.
#' @param var_labels List which contains column variable names and their respective labels.
#' @param style A list containing the styling elements.
#' @param output Determines whether to style the output or to just quickly paste
#' the data.
#' @param monitor_df Data frame which stores the monitoring values.
#'
#' @return
#' Returns a list containing a formatted Excel workbook as well as the monitoring
#' data frame.
#'
#' @noRd
format_df_excel <- function(wb,
                            data_frame,
                            titles,
                            footnotes,
                            var_labels,
                            style,
                            output,
                            monitor_df){
    monitor_df <- monitor_df |> monitor_start("Excel prepare", "Format")

    # Get table ranges
    df_ranges <- get_df_ranges(data_frame, titles, footnotes, style)

    data_frame <- data_frame |> set_labels_as_names(var_labels)

    # Add sheet
    wb$add_worksheet(style[["sheet_name"]], grid_lines = style[["grid_lines"]])

    # Add table data and format according to style options
    monitor_df <- monitor_df |> monitor_next("Excel data", "Format")

    wb$add_data(x           = data_frame,
                start_col   = style[["start_column"]],
                start_row   = df_ranges[["header.row"]],
                col_names   = TRUE,
                with_filter = style[["filters"]],
                na.strings  = style[["na_symbol"]])

    # Format titles and footnotes if there are any
    monitor_df <- monitor_df |> monitor_next("Excel titles/footnotes", "Format")
    wb <- wb |>
        format_titles_foot_excel(titles, footnotes, df_ranges, style, output)

    # Only do the formatting when user specified it. With the excel_nostyle
    # option this whole part gets omitted to get a very quick unformatted
    # excel output.
    if (output == "excel"){
        # Style table
        monitor_df <- monitor_df |> monitor_next("Excel cell styles", "Format")
        wb <- wb |> handle_cell_styles(df_ranges, style)

        if (df_ranges[["num_format.length"]] > 0){
            monitor_df <- monitor_df |> monitor_next("Excel number formats", "Format")

            # Set up inner table number formats
            for (i in 1:df_ranges[["num_format.length"]]){
                wb$add_cell_style(dims                = df_ranges[[paste0("df_col_ranges", i)]],
                                  apply_number_format = TRUE,
                                  num_fmt_id          = wb$styles_mgr$get_numfmt_id(paste0(df_ranges[[paste0("df_col_types", i)]], "_numfmt")))
            }
        }

        # Freeze headers. If both options are true they have to be set together, otherwise one
        # option would overwrite the other.
        if (style[["freeze_col_header"]] & style[["freeze_row_header"]]){
            wb$freeze_pane(first_active_col = df_ranges[["header.column"]] + df_ranges[["cat_col.width"]],
                           first_active_row = df_ranges[["table.row"]])
        }
        else if (style[["freeze_col_header"]]){
            wb$freeze_pane(first_active_col = df_ranges[["header.column"]] + df_ranges[["cat_col.width"]])
        }
        else if (style[["freeze_row_header"]]){
            wb$freeze_pane(first_active_row = df_ranges[["table.row"]])
        }

        # Adjust table dimensions
        monitor_df <- monitor_df |> monitor_next("Excel widths/heights", "Format")

        wb <- wb |> handle_col_row_dimensions(df_ranges,
                                              ncol(data_frame) + (style[["start_column"]] - 1),
                                              nrow(data_frame) + (style[["start_row"]] - 1),
                                              style) |>
            handle_any_auto_dimensions(df_ranges, style) |>
            handle_header_table_dim(df_ranges, style)
    }

    monitor_df <- monitor_df |> monitor_end()

    # Return workbook
    list(wb, monitor_df)
}


#' Insert Variable Labels
#'
#' @description
#' Give the variables in the column header a custom label.
#'
#' @param column_header The complete column multi header.
#' @param var_labels A list in which is specified which label should be printed for
#' which variable instead of the variable name.
#'
#' @return
#' Returns a multi layered column header with replaced variable texts.
#'
#' @noRd
set_labels_as_names <- function(data_frame, var_labels){
    if (length(var_labels) == 0){
        return(data_frame)
    }

    # Loop through all provided labels
    for (i in seq_along(var_labels)){
        name  <- names(var_labels)[i]
        label <- var_labels[[i]]

        # Omit label with missing variable name
        if (is.null(name) | name == ""){
            next
        }

        # Replace variable texts with provided labels
        names(data_frame) <- gsub(name, label, names(data_frame))
    }

    data_frame
}
