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
#' @param style A list of options can be passed to control the appearance of 'Excel' outputs.
#' Styles can be created with [excel_output_style()].
#' @param column_align Individually align the data frame columns in the final 'Excel' output.
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
#' Returns a list with the data table containing the results for the table, the formatted
#' 'Excel' workbook and the meta information needed for styling the final table.
#'
#' @seealso
#' Creating a custom table style: [excel_output_style()], [modify_output_style()],
#' [number_format_style()], [modify_number_formats()].
#'
#' Global style options: [set_style_options()], [set_labels()].
#'
#' Other global options: [set_titles()], [set_footnotes()], [set_print()], [set_monitor()],
#' [set_na.rm()], [set_print()], [set_print_miss()], [set_output()].
#'
#' Combine Excel workbooks: [combine_into_workbook()].
#'
#' Functions that can handle styles: [frequencies()], [crosstabs()], [any_table()].
#'
#' @examples
#' # Example data frame
#' my_data <- dummy_data(100)
#' set_style_options(background_color = "")
#'
#' # Define style
#' set_style_options(column_widths = c(2, 15, 15, 15, 9))
#'
#' # Define titles and footnotes. If you want to add hyperlinks you can do so by
#' # adding "link:" followed by the hyperlink to the main text. Linking to another
#' # cell works with "cell:". To link to a file use "file:" an pass the full file
#' # path afterwards.
#' set_titles("This is title number 1",
#'            "This is title number 2 link: https://cran.r-project.org/",
#'            "This is title number 3 cell: W22",
#'            "This is title number 4 file: C:/MyFolder/MyFile.txt")
#'
#' set_footnotes("This is footnote number 1",
#'               "This is footnote number 2 file: C:/MyFolder/MyFile.txt",
#'               "This is footnote number 3 cell: W22",
#'               "This is footnote number 4 link: https://cran.r-project.org/")
#'
#' # Print styled data frame
#' my_data |> export_with_style()
#'
#' # Retrieve formatted workbook, original table and meta information for further usage
#' export_list <- my_data |> export_with_style()
#'
#' # To save a table as xlsx file you have to set the path and filename in the
#' # style element
#' # Example files paths
#' table_file <- tempfile(fileext = ".xlsx")
#'
#' # Note: Normally you would directly input the path ("C:/MyPath/") and
#' #       name ("MyFile.xlsx").
#' #       With the set_style_options you can also set a table style globally.
#' set_style_options(save_path  = dirname(table_file),
#'                   file       = basename(table_file),
#'                   sheet_name = "MyTable")
#'
#' my_data |> export_with_style()
#'
#' # In case you have a good amount of tables, you want to combine in a single
#' # workbook, you can also catch the outputs and combine them afterwards in one go.
#' set_style_options(sheet_name = "sheet1")
#' tab1 <- my_data |> export_with_style(print = FALSE)
#'
#' set_titles(NULL)
#' set_style_options(sheet_name = "sheet2")
#' tab2 <- my_data |> export_with_style(print = FALSE)
#'
#' set_footnotes(NULL)
#' set_style_options(sheet_name = "sheet3")
#' tab3 <- my_data |> export_with_style(print = FALSE, output = "excel_nostyle")
#'
#' # Every of the above tabs is a list, which contains the data table, an unstyled
#' # workbook and the meta information needed for the individual styling. These tabs
#' # can be input into the following function, which reads the meta information,
#' # styles each table individually and combines them as separate sheets into a
#' # single workbook.
#' combine_into_workbook(tab1, tab2, tab3)
#' combine_into_workbook(tab1, tab2, tab3, file = table_file)
#'
#' # Manual cleanup for example
#' unlink(table_file)
#'
#' # Global options are permanently active until the current R session is closed.
#' # There are also functions to reset the values manually.
#' reset_style_options()
#' reset_qol_options()
#' close_file()
#'
#' set_print(TRUE)
#' set_style_options(background_color = "FFFFFF")
#'
#' @export
export_with_style <- function(data_frame,
                              titles       = .qol_options[["titles"]],
                              footnotes    = .qol_options[["footnotes"]],
                              var_labels   = .qol_options[["var_labels"]],
                              workbook     = NULL,
                              style        = .qol_options[["excel_style"]],
                              column_align = NULL,
                              output       = .qol_options[["output"]],
                              print        = .qol_options[["print"]],
                              monitor      = .qol_options[["monitor"]]){
    print_start_message()
	print_step("GREY", "Error handling")

    #-------------------------------------------------------------------------#
    monitor_df <- NULL |> monitor_start("Error handling", "Error handling")
    #-------------------------------------------------------------------------#

    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # Workbook
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    # Check whether the whole result list of one of the tabulation functions was passed
    if (!is.null(workbook) && is.list(workbook)){
        # If the workbook object is part of the list, extract it
        if ("workbook" %in% names(workbook)){
            workbook <- workbook[["workbook"]]
        }
        # If the list is some other list without workbook object, abort
        else{
            print_message("ERROR", c("Workbook object is invalid. You have to provide a workbook object",
                                     "created with one of the tabulation functions. Tabulation will be aborted."))
            return(invisible(NULL))
        }
    }

    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # Output
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    # Silent conversion of global options, which are invalid for any_table
    if (tolower(output) %in% c("console", "text")){
        output <- "excel"
    }

    # Check for invalid output option
    if (!tolower(output) %in% c("excel", "excel_nostyle")){
        print_message("WARNING", "<Output> format '[output]' not available. Using 'excel' instead.", output = output)

        output <- "excel"
    }
    else{
        output <- tolower(output)
    }

    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # Resolve macros
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    titles     <- apply_macro(titles)
    footnotes  <- apply_macro(footnotes)
    var_labels <- apply_macro(var_labels)

    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # Prepare table format for output
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    # Grab all information, which is necessary to format the workbook. This list will be
    # returned at the end and can be grabbed by the workbook combine function.
    meta <- c(mget(c("titles", "footnotes", "var_labels", "style", "column_align", "output")), "DATA")

    #-------------------------------------------------------------------------#
    monitor_df <- monitor_df |> monitor_next("Excel prepare", "Format")
    #-------------------------------------------------------------------------#

    # Setup styling in new workbook if no other is provided
    if (is.null(workbook)){
        workbook <- openxlsx2::wb_workbook() |>
            prepare_styles(list("title" = titles, "footnote" = footnotes), style)
    }
    # Update style options in provided workbook
    else{
        workbook <- workbook |>
            prepare_styles(list("title" = titles, "footnote" = footnotes), style)
    }

    monitor_df <- monitor_df |> monitor_end()

    # Style data frame for export
    wb_list <- format_df_excel(workbook, data_frame, titles, footnotes, var_labels,
                               style, column_align, output, monitor_df)

    wb         <- wb_list[[1]]
    monitor_df <- wb_list[[2]]

    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # Output formatted table into different formats
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    if (print){
        #---------------------------------------------------------------------#
        monitor_df <- monitor_df |> monitor_next("Output tables", "Output tables")
        #---------------------------------------------------------------------#
        print_step("MAJOR", "Output")

        # Check if only save path or file name is specified. If only one is specified
        # print a note. Otherwise the file would not be saved and opened without a
        # hint to why the file wasn't saved.
        if (is.null(style[["save_path"]]) + is.null(style[["file"]]) == 1){
            if (is.null(style[["save_path"]])){
                print_message("NOTE", c("No save path specified. Both save path and file name with extension",
                                        "need to be specified in the global options or style parameter for",
                                        "the file to be saved. File won't be saved."))
            }
            else{
                print_message("NOTE", c("No file name specified. Both save path and file name with extension",
                                        "need to be specified in the global options or style parameter for",
                                        "the file to be saved. File won't be saved."))
            }
        }

        # If no save path or file provided just open workbook
        if (is.null(style[["save_path"]]) || is.null(style[["file"]])){
            if (interactive()){
                wb$open()
            }
        }
        else{
            # If no save path or file provided just open workbook
            if (!file.exists(style[["save_path"]])){
                print_message("WARNING", "Path does not exist: [style]", style = style[["save_path"]])

                if (interactive()){
                    wb$open()
                }
            }
            # Save file
            else{
                # NOTE: A CSV export like in any_table, crosstabs or frequencies makes no sense
                #       here. export_data should be used for that, since this function here is
                #       explicitly for styled data frame export.
                wb$save(file = paste0(style[["save_path"]], "/", style[["file"]]), overwrite = TRUE)
            }
        }
    }

    print_closing(5)

    #-------------------------------------------------------------------------#
    monitor_df <- monitor_df |> monitor_end()
    monitor_df |> monitor_plot(draw_plot = monitor)
    #-------------------------------------------------------------------------#

    invisible(list("table"    = data_frame,
                   "workbook" = wb,
                   "meta"     = meta))
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
#' @param column_align Individually align the data frame columns in the final 'Excel' output.
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
                            column_align,
                            output,
                            monitor_df){

    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # Add data
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    #-------------------------------------------------------------------------#
    monitor_df <- monitor_df |> monitor_start("Excel prepare", "Format")
    #-------------------------------------------------------------------------#

    # Get table ranges
    df_ranges <- get_df_ranges(data_frame, titles, footnotes, style)

    data_frame <- data_frame |> set_labels_as_names(var_labels)

    # Add sheet
    wb$add_worksheet(style[["sheet_name"]], grid_lines = style[["grid_lines"]])

    # Add table data and format according to style options
    #-------------------------------------------------------------------------#
    monitor_df <- monitor_df |> monitor_next("Excel data", "Format")
    #-------------------------------------------------------------------------#
    print_step("MAJOR", "Writing data to workbook")

    wb$add_data(x           = data_frame,
                start_col   = style[["start_column"]],
                start_row   = df_ranges[["header.row"]],
                col_names   = TRUE,
                with_filter = style[["filters"]],
                na.strings  = style[["na_symbol"]])

    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # Apply style
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    #-------------------------------------------------------------------------#
    monitor_df <- monitor_df |> monitor_next("Excel titles/footnotes", "Format")
    #-------------------------------------------------------------------------#
    print_step("MAJOR", "Formatting data")
    print_step("MINOR", "Format titles and footnotes")

    # Format titles and footnotes if there are any
    wb <- wb |>
        format_titles_foot_excel(titles, footnotes, df_ranges, style, output)

    # Only do the formatting when user specified it. With the excel_nostyle
    # option this whole part gets omitted to get a very quick unformatted
    # excel output.
    if (output == "excel"){
        #---------------------------------------------------------------------#
        monitor_df <- monitor_df |> monitor_next("Excel cell styles", "Format")
        #---------------------------------------------------------------------#
        print_step("MINOR", "Add cell styles")

        wb <- wb |> handle_cell_styles(df_ranges, style)

        # Fill all cells with background color
        if (style[["background_color"]] != ""){
            #-----------------------------------------------------------------#
            monitor_df <- monitor_df |> monitor_next("Excel background", "Format")
            #-----------------------------------------------------------------#
            print_step("MINOR", "Fill background")

            wb$add_fill(color = openxlsx2::wb_color(style[["background_color"]]))
            wb$set_cell_style_across(cols = "A:XFD", style = wb$get_cell_style(dims = "A1"))

            # Titles and footnotes need to get an extra fill
            wb$add_fill(dims = df_ranges[["title_range"]],    color = openxlsx2::wb_color(style[["background_color"]]))
            wb$add_fill(dims = df_ranges[["footnote_range"]], color = openxlsx2::wb_color(style[["background_color"]]))
        }

        # Format individual column alignments
        if (!is.null(column_align)){
            print_step("MINOR", "Individual column alignment")

            single_columns <- split_up_ranges(df_ranges[["table_range"]])
            column_align   <- fill_or_trim(column_align, length(single_columns))

            for (i in seq_len(length(single_columns))){
                wb$add_cell_style(dims       = single_columns[i],
                                  horizontal = column_align[i])
            }
        }

        # Apply number formats
        if (df_ranges[["num_format.length"]] > 0){
            #-----------------------------------------------------------------#
            monitor_df <- monitor_df |> monitor_next("Excel number formats", "Format")
            #-----------------------------------------------------------------#
            print_step("MINOR", "Format numbers")

            # Set up inner table number formats
            for (i in 1:df_ranges[["num_format.length"]]){
                wb$add_cell_style(dims                = df_ranges[[paste0("df_col_ranges", i)]],
                                  apply_number_format = TRUE,
                                  num_fmt_id          = wb$styles_mgr$get_numfmt_id(paste0(df_ranges[[paste0("df_col_types", i)]], "_numfmt")))
                wb$add_ignore_error(dims = df_ranges[[paste0("df_col_ranges", i)]], number_stored_as_text = TRUE)
            }
        }

        # Draw inner table cells as heat map with conditional formatting
        if (style[["as_heatmap"]]){
            #-----------------------------------------------------------------#
            monitor_df <- monitor_df |> monitor_next("Excel format heatmap", "Format")
            #-----------------------------------------------------------------#
            print_step("MINOR", "Add heatmap")

            wb$add_conditional_formatting(dims  = df_ranges[["table_range"]],
                                          style = c(style[["heatmap_low_color"]],
                                                    style[["heatmap_middle_color"]],
                                                    style[["heatmap_high_color"]]),
                                          type  = "colorScale")
        }

        # Freeze headers. If both options are true they have to be set together, otherwise one
        # option would overwrite the other.
        if (style[["freeze_col_header"]] && style[["freeze_row_header"]]){
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
        #---------------------------------------------------------------------#
        monitor_df <- monitor_df |> monitor_next("Excel widths/heights", "Format")
        #---------------------------------------------------------------------#
        print_step("MINOR", "Adjust row heights and column widths")

        wb <- wb |> handle_col_row_dimensions(df_ranges,
                                              collapse::fncol(data_frame) + (style[["start_column"]] - 1),
                                              collapse::fnrow(data_frame) + (style[["start_row"]] - 1),
                                              style) |>
            handle_any_auto_dimensions(df_ranges, style) |>
            handle_header_table_dim(df_ranges, style)

        wb$add_ignore_error(dims = df_ranges[["header_range"]], number_stored_as_text = TRUE)

        wb$add_named_region(dims = df_ranges[["whole_tab_range"]], name = "table", local_sheet = TRUE)
        wb$add_named_region(dims = df_ranges[["table_range"]],     name = "data",  local_sheet = TRUE)
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

    valid_labels <- data_frame |> part_of_df(names(var_labels))

    # If no valid variable names for renaming are provided, return
    if (length(valid_labels) == 0){
        return(data_frame)
    }

    # Only keep valid labels
    var_labels <- var_labels[names(var_labels) %in% valid_labels]

    # Rename variables according to specified labels
    collapse::frename(data_frame,
                      stats::setNames(names(var_labels),
                                      as.character(var_labels)),
                      .nse = FALSE)
}
