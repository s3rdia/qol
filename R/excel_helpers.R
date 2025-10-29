###############################################################################
# Conversion
###############################################################################

#' Converts Numbers into 'Excel' Ranges
#'
#' @description
#' Converts a column number into the according letter to form a cell reference
#' like it is used in 'Excel' (e.g "A1"). Also can compute a range from cell to
#' cell (e.g. "A1:BY22").
#'
#' @param row Single row number.
#' @param column Single column number.
#' @param from_row Range start row.
#' @param from_column Range start column.
#' @param to_row Range end row.
#' @param to_column Range end column.
#'
#' @return
#' Returns a character with an 'Excel' range.
#'
#' @examples
#' single_cell <- get_excel_range(row = 1, column = 6)
#' range       <- get_excel_range(from_row = 1, from_column = 6,
#'                                  to_row = 5,   to_column = 35)
#'
#' @export
get_excel_range <- function(row      = NULL, column      = NULL,
                            from_row = NULL, from_column = NULL,
                            to_row   = NULL, to_column   = NULL) {

    # Get single cell
    if (!is.null(row) && !is.null(column)){
        if (row <= 0 || column <= 0){
            message(" X ERROR: Row and column must be greater than 0.")
            return(NULL)
        }

        return(openxlsx2::wb_dims(rows = row, cols = column))
    }

    # Else get cell range
    if (!is.null(from_row) && !is.null(from_column) &&
        !is.null(to_row) && !is.null(to_column)){
            if (from_column <= 0 || from_row <= 0 || to_row <= 0 || to_column <= 0){
                # No error message here because any_table runs into this regularly if
                # e.g. there are no titles set.
                return(NULL)
            }

        return(openxlsx2::wb_dims(rows = seq.int(from_row, to_row),
                                  cols = seq.int(from_column, to_column)))
    }

    NULL
}

###############################################################################
# Getting table parts for any_table
###############################################################################
#' Get Different Table Parts as Excel Ranges
#'
#' @description
#' Identifies the different table parts (like titles, header, inner table cells)
#' and returns their Ranges in Excel format to be used by openxlsx2.
#'
#' @param table The data frame which holds the table information.
#' @param multi_header The multi layered column header produced by any_table.
#' @param titles Titles if there are any.
#' @param footnotes Footnotes if there are any.
#' @param style A list of style elements to format the table.
#'
#' @return
#' Returns a list with table ranges.
#'
#' @noRd
get_any_table_ranges <- function(table,
                                 multi_header,
                                 stats_row,
                                 titles     = NULL,
                                 footnotes  = NULL,
                                 style      = excel_output_style()){
    # If titles are provided put them in the starting row
    if (!is.null(titles)){
        title.row     <- style[["start_row"]]
        title.column  <- style[["start_column"]]
        title.length  <- length(titles)

        header.row    <- title.row + length(titles) + 1
        header.column <- title.column
    }
    # If no titles are provided the table headers are in the starting row
    else{
        title.row     <- 0
        title.column  <- 0
        title.length  <- 0

        header.row    <- style[["start_row"]]
        header.column <- style[["start_column"]]
    }

    # Get the basic starting cells, as well as table dimensions
    header.width  <- ncol(multi_header)
    header.length <- nrow(multi_header)

    table.row     <- header.row + header.length
    table.length  <- nrow(table)
    table.width   <- ncol(table)
    table.end     <- header.column + table.width - 1
    cat_col.width <- ncol(table) - ncol(multi_header)

    footnote.row <- table.row + table.length + 1

    if (!is.null(footnotes)){
        footnote.length <- length(footnotes)
    }
    else{
        footnote.length <- 0
    }

    # Individually compute cell ranges in Excel format
    title_range <- get_excel_range(from_row  = title.row, from_column = title.column,
                                   to_row    = title.row + (title.length - 1),
                                   to_column = title.column)

    whole_tab_range <- get_excel_range(from_row  = header.row, from_column = header.column,
                                       to_row    = table.row     + table.length - 1,
                                       to_column = header.column + (cat_col.width - 1) + header.width)

    header_range <- get_excel_range(from_row  = header.row, from_column = header.column,
                                    to_row    = header.row    + (header.length - 1),
                                    to_column = header.column + (cat_col.width - 1) + header.width)

    box_range <- get_excel_range(from_row  = header.row, from_column = header.column,
                                 to_row    = header.row    + (header.length - 1),
                                 to_column = header.column + (cat_col.width - 1))

    cat_col_range <- get_excel_range(from_row  = table.row, from_column = header.column,
                                     to_row    = table.row     + (table.length - 1),
                                     to_column = header.column + (cat_col.width - 1))

    table_range <- get_excel_range(from_row    = table.row,
                                   from_column = header.column + cat_col.width,
                                   to_row      = table.row     + (table.length - 1),
                                   to_column   = header.column + (cat_col.width - 1) + header.width)

    footnote_range <- get_excel_range(from_row    = footnote.row,
                                      from_column = header.column,
                                      to_row      = footnote.row  + (footnote.length - 1),
                                      to_column   = header.column)

    # Output ranges for further use
    all    <- as.list(environment())
    ranges <- names(formals(sys.function()))

    all[setdiff(names(all), ranges)]
}


#' Get All The Table Ranges of Any Table as Excel Ranges
#'
#' @description
#' Identifies the different table ranges and returns them as ranges in Excel format
#' to be used by 'openxlsx2'.
#'
#' @param any_tab The data frame for any table.
#' @param multi_header The multi layered column header produced by any_table.
#' @param col_header_dimensions A list containing the column variable names and the
#' column header dimensions.
#' @param row_header_dimensions A list containing the row variable names and the
#' row header dimensions.
#' @param titles Titles if there are any.
#' @param footnotes Footnotes if there are any.
#' @param style A list of style elements to format the table.
#'
#' @return
#' Returns a list with table ranges.
#'
#' @noRd
get_any_tab_ranges <- function(any_tab,
                               multi_header,
                               stats_row,
                               titles     = NULL,
                               footnotes  = NULL,
                               style      = excel_output_style()){
    # Get basic table ranges
    table_ranges <- get_any_table_ranges(any_tab, multi_header, stats_row,
                                         titles, footnotes, style)

    # Get specific parts of mean table
    any_col_ranges <- list()

    chunks        <- rle(stats_row)
    chunk_lengths <- chunks[["lengths"]]

    col_start <- table_ranges[["header.column"]] + table_ranges[["cat_col.width"]]


    for (i in seq_along(chunk_lengths)){
        from_col <- col_start + sum(chunk_lengths[seq_len(i - 1)])
        to_col   <- col_start + sum(chunk_lengths[seq_len(i)])

        range_name <- paste0("any_col_ranges", i)

        any_col_ranges[[range_name]] <-
                get_excel_range(from_row    = table_ranges[["table.row"]],
                                from_column = from_col,
                                to_row      = table_ranges[["table.row"]] +
                                             (table_ranges[["table.length"]] - 1),
                                to_column   = to_col - 1)
    }

    # Output ranges for further use
    c(table_ranges, any_col_ranges)
}


#' Merge Column Header Labels With The Same Name
#'
#' @description
#' If columns have the same name or if there are empty header cells, merge them
#' together to enhance the visual appeal of the table header.
#'
#' @param wb The currently processed workbook.
#' @param column_header The multi layered column header.
#' @param ranges Table ranges.
#'
#' @return
#' Returns a workbook with merged column headers.
#'
#' @noRd
handle_col_header_merge <- function(wb, column_header, ranges){
    # Get all values in order of appearance with their respective lengths
    row_values <- lapply(seq_len(nrow(column_header)), function(row){
        rle(as.character(column_header[row, ]))
    })

    # Define offsets in Excel table
    row_offset <- ranges[["header.row"]] - 1
    col_offset <- ranges[["header.column"]] + (ranges[["cat_col.width"]] - 1)

    # Loop through all rows
    number_of_rows <- length(row_values)

    for (row in seq_along(row_values)){
        current_row <- row_values[[row]]
        start_col <- 1

        # Loop through all column chunks
        for (i in seq_along(current_row[["values"]])){
            value <- current_row[["values"]][i]
            space <- current_row[["lengths"]][i]

            # If no empty value and there is an actual need for merging because more
            # than one column has the same value
            if (value != ""){
                from_row <- row
                from_col <- start_col
                to_row   <- row
                to_col   <- start_col + (space - 1)

                # Expand down if upcoming rows are empty in the same part
                if (row < number_of_rows){
                    for (next_row in (row + 1):length(row_values)){
                        slice <- as.character(column_header[next_row, from_col:to_col])

                        # If upcoming row in the same part is empty, expand cell merging
                        # further down
                        if (all(slice == "")){
                            to_row <- next_row
                        }
                        # If row part is not empty exit loop. No further expand of
                        # cell merging.
                        else{
                            break
                        }
                    }
                }

                # Merge cells
                if (from_row == to_row && from_col == to_col){
                    start_col <- start_col + space
                    next
                }

                wb$merge_cells(dims = get_excel_range(from_row    = row_offset + from_row,
                                                      from_column = col_offset + from_col,
                                                      to_row      = row_offset + to_row,
                                                      to_column   = col_offset + to_col))
            }

            start_col <- start_col + max(1, space)
        }
    }

    wb
}


#' Merge Row Header Labels With The Same Name
#'
#' @description
#' If rows have the same name or if there are empty header cells, merge them
#' together to enhance the visual appeal of the table header.
#'
#' @param wb The currently processed workbook.
#' @param row_header The multi layered row header.
#' @param ranges Table ranges.
#'
#' @return
#' Returns a workbook with merged column headers.
#'
#' @noRd
handle_row_header_merge <- function(wb, row_header, ranges){
    # Make sure row_header is treated as a matrix, even though there can be only one column
    if (is.null(dim(row_header))) {
        dim(row_header) <- c(length(row_header), 1)
    }

    # Get all values in order of appearance with their respective lengths (per column)
    col_values <- lapply(seq_len(ncol(row_header)), function(column){
        rle(as.character(row_header[, column]))
    })

    # Define offsets in Excel table
    row_offset <- ranges[["table.row"]] - 1
    col_offset <- ranges[["header.column"]] - 1

    # Loop through all columns
    number_of_columns <- length(col_values)

    for (column in seq_along(col_values)){
        current_col <- col_values[[column]]
        start_row   <- 1

        # Loop through all row chunks
        for (i in seq_along(current_col[["values"]])){
            value <- current_col[["values"]][i]
            space <- current_col[["lengths"]][i]

            # If no empty value and there is an actual need for merging because more
            # than one column has the same value
            if (value != ""){
                from_col <- column
                from_row <- start_row
                to_col   <- column
                to_row   <- start_row + (space - 1)

                # Expand right if upcoming columns are empty in the same block
                if (column < number_of_columns){
                    for (next_column in seq.int(column + 1, number_of_columns)){
                        slice <- as.character(row_header[from_row:to_row, next_column])

                        # If upcoming column in the same part is empty, expand cell merging
                        # further right
                        if (all(slice == "")){
                            to_col <- next_column
                        }
                        # If column part is not empty exit loop. No further expand of
                        # cell merging.
                        else{
                            break
                        }
                    }
                }

                # Merge cells
                if (from_row == to_row && from_col == to_col){
                    start_row <- start_row + space
                    next
                }

                wb$merge_cells(dims = get_excel_range(from_row    = row_offset + from_row,
                                                      from_column = col_offset + from_col,
                                                      to_row      = row_offset + to_row,
                                                      to_column   = col_offset + to_col))
            }

            start_row <- start_row + space
        }
    }

    wb
}

###############################################################################
# Get ranges for export with style
###############################################################################
#' Get Data Frame Ranges
#'
#' @description
#' Identifies the different table ranges of a data frame and returns them as ranges
#' in Excel format to be used by 'openxlsx2'.
#'
#' @param data_frame The data frame to get the ranges from.
#' @param titles Titles if there are any.
#' @param footnotes Footnotes if there are any.
#' @param style A list of style elements to format the table.
#'
#' @return
#' Returns a list with table ranges.
#'
#' @noRd
get_df_ranges <- function(data_frame,
                          titles     = NULL,
                          footnotes  = NULL,
                          style      = excel_output_style()){
    # Get basic table ranges
    table_ranges <- get_table_ranges(data_frame, titles, footnotes, style)

    # Get specific parts of mean table
    df_col_ranges <- list()
    df_col_types  <- list()

    col_start <- table_ranges[["header.column"]] - 1

    extensions <- c("sum", "pct", "group", "total", "value", "freq",
                    "g0", "mean", "median", "mode", "min", "max", "first",
                    "last", "wgt", "p", "sd", "variance", "missing")

    format_index <- 1

    for (i in seq_len(ncol(data_frame))){
        # If a variable doesn't have a statistics extension, it is likely not
        # a variable that needs a number format.
        var_end <- sub("p[0-9]+$", "p", utils::tail(strsplit(names(data_frame)[[i]], "_")[[1]], 1))

        if (!var_end %in% extensions){
            next
        }

        # Add number format
        col_to_format <- col_start + i

        range_name <- paste0("df_col_ranges", format_index)
        type_name  <- paste0("df_col_types", format_index)

        df_col_ranges[[range_name]] <-
            get_excel_range(from_row    = table_ranges[["table.row"]],
                            from_column = col_to_format,
                            to_row      = table_ranges[["table.row"]] +
                                (table_ranges[["table.length"]] - 1),
                            to_column   = col_to_format)

        df_col_types[[type_name]] <- var_end

        format_index <- format_index + 1
    }

    table_ranges[["num_format.length"]] <- format_index - 1

    # Output ranges for further use
    c(table_ranges, df_col_ranges, df_col_types)
}


###############################################################################
# Getting table parts
###############################################################################
#' Get Different Table Parts as Excel Ranges
#'
#' @description
#' Identifies the different table parts (like titles, header, inner table cells)
#' and returns their Ranges in Excel format to be used by openxlsx2.
#'
#' @param table The data frame which holds the table information.
#' @param titles Titles if there are any.
#' @param footnotes Footnotes if there are any.
#' @param style A list of style elements to format the table.
#'
#' @return
#' Returns a list with table ranges.
#'
#' @noRd
get_table_ranges <- function(table,
                             titles     = NULL,
                             footnotes  = NULL,
                             style      = excel_output_style()){
    # If titles are provided put them in the starting row
    if (!is.null(titles)){
        title.row     <- style[["start_row"]]
        title.column  <- style[["start_column"]]
        title.length  <- length(titles)

        header.row    <- title.row + length(titles) + 1
        header.column <- title.column
    }
    # If no titles are provided the table headers are in the starting row
    else{
        title.row     <- 0
        title.column  <- 0
        title.length  <- 0

        header.row    <- style[["start_row"]]
        header.column <- style[["start_column"]]
    }

    # Get the basic starting cells, as well as table dimensions
    header.width  <- ncol(table) - 1

    table.row     <- header.row + 1
    table.length  <- nrow(table)
    table.width   <- ncol(table)
    table.end     <- header.column + table.width - 1
    cat_col.width <- 1

    footnote.row <- table.row + table.length + 1

    if (!is.null(footnotes)){
        footnote.length <- length(footnotes)
    }
    else{
        footnote.length <- 0
    }

    # Individually compute cell ranges in Excel format
    title_range <- get_excel_range(from_row  = title.row, from_column = title.column,
                                   to_row    = title.row + (title.length - 1),
                                   to_column = title.column)

    whole_tab_range <- get_excel_range(from_row  = header.row, from_column = header.column,
                                       to_row    = header.row    + table.length,
                                       to_column = header.column + header.width)

    header_range <- get_excel_range(from_row  = header.row, from_column = header.column,
                                    to_row    = header.row,
                                    to_column = header.column + header.width)

    box_range <- get_excel_range(from_row  = header.row, from_column = header.column,
                                 to_row    = header.row,
                                 to_column = header.column)

    cat_col_range <- get_excel_range(from_row  = table.row, from_column = header.column,
                                     to_row    = header.row    + table.length,
                                     to_column = header.column)

    table_range <- get_excel_range(from_row    = table.row,
                                   from_column = header.column + 1,
                                   to_row      = header.row    + table.length,
                                   to_column   = header.column + header.width)

    footnote_range <- get_excel_range(from_row    = footnote.row,
                                      from_column = header.column,
                                      to_row      = footnote.row  + (footnote.length - 1),
                                      to_column   = header.column)

    # Output ranges for further use
    all    <- as.list(environment())
    ranges <- names(formals(sys.function()))

    all[setdiff(names(all), ranges)]
}


#' Get Columns of Mean Table as Excel Ranges
#'
#' @description
#' Identifies the different table columns and returns their ranges in Excel format
#' to be used by 'openxlsx2'.
#'
#' @param mean_tab The data frame for the mean table produced by frequencies.
#' @param style A list of style elements to format the table.
#'
#' @return
#' Returns a list with table ranges.
#'
#' @noRd
get_mean_tab_ranges <- function(mean_tab,
                                style = excel_output_style()){
    # Get basic table ranges
    table_ranges <- get_table_ranges(mean_tab, style = style)

    # Get specific parts of mean table
    mean_col_ranges <- c()

    for (column in 1:6){
        mean_col_ranges <- c(mean_col_ranges,
                             get_excel_range(from_row    = table_ranges[["header.row"]] + 1,
                                             from_column = table_ranges[["header.column"]] + column,
                                             to_row      = table_ranges[["header.row"]] + table_ranges[["table.length"]],
                                             to_column   = table_ranges[["header.column"]] + column))
    }

    # Output ranges for further use
    c(table_ranges,
      mean_col_ranges = mean_col_ranges)
}


#' Get Columns of Freq Table as Excel Ranges
#'
#' @description
#' Identifies the different table columns and returns their ranges in Excel format
#' to be used by 'openxlsx2'.
#'
#' @param freq_tab The data frame for the freq table produced by frequencies.
#' @param titles Titles if there are any.
#' @param footnotes Footnotes if there are any.
#' @param style A list of style elements to format the table.
#'
#' @return
#' Returns a list with table ranges.
#'
#' @noRd
get_freq_tab_ranges <- function(freq_tab,
                                titles     = NULL,
                                footnotes  = NULL,
                                style      = excel_output_style()){
    # Get basic table ranges
    table_ranges <- get_table_ranges(freq_tab, titles, footnotes, style)

    # Get specific parts of mean table
    freq_col_ranges <- c()

    for (column in 1:6){
        freq_col_ranges <- c(freq_col_ranges,
                             get_excel_range(from_row    = table_ranges[["header.row"]] + 1,
                                             from_column = table_ranges[["header.column"]] + column,
                                             to_row      = table_ranges[["header.row"]] + table_ranges[["table.length"]],
                                             to_column   = table_ranges[["header.column"]] + column))
    }

    # Output ranges for further use
    c(table_ranges,
      freq_col_ranges = freq_col_ranges)
}

###############################################################################
# Format titles and footnotes if there are any
###############################################################################
#' Format Titles and Footnotes
#'
#' @description
#' Format titles and footnotes if there are any.
#'
#' @param wb The workbook to modify.
#' @param ranges Table ranges to format.
#' @param style A list of style elements to format the table.
#'
#' @return
#' Returns a further formatted workbook.
#'
#' @noRd
format_titles_foot_excel <- function(wb, titles, footnotes, ranges, style, output){
    if (length(titles) > 0){
        # Check for hyperlinks
        links    <- c()
        link_pos <- c()

        for (i in seq_along(titles)){
            title <- titles[[i]]

            # If there is a link in the text
            if (grepl("link:", title)){
                # Get the link and the title range
                links    <- c(links, sub(".*link:", "", title))
                link_pos <- c(link_pos,
                              get_excel_range(row    = ranges[["title.row"]] + (i - 1),
                                            column = ranges[["title.column"]]))

                # Remove link from the text
                titles[[i]] <- sub("link:.*", "", title)
            }
        }

        # Paste titles
        wb$add_data(x         = titles,
                    start_col = style[["start_column"]],
                    start_row = style[["start_row"]])

        # Format
        if (output == "excel"){
            # Apply cell styles
            wb$add_cell_style(dims       = ranges[["title_range"]],
                              horizontal = style[["title_alignment"]],
                              vertical   = "center",
                              wrap_text  = "1",
                              apply_font = TRUE,
                              font_id    = wb$styles_mgr$get_font_id("title_font"))

            # Merge the titles over the span of the table
            for (title in seq_along(titles)){
                wb$merge_cells(dims = openxlsx2::wb_dims(
                    cols = style[["start_column"]]:ranges[["table.end"]],
                    rows = style[["start_row"]] + (title - 1)))
            }

            # Set row heights for titles if specified
            title_heights <- style[["title_heights"]]

            if (!is.null(title_heights)){
                number_of_rows <- length(titles)
                start_row      <- ranges[["title.row"]]
                end_row        <- start_row + number_of_rows - 1

                title_heights <- fill_or_trim(title_heights,
                                              number_of_rows)

                wb$set_row_heights(rows    = start_row:end_row,
                                   heights = title_heights)
            }

            # Add hyperlinks
            for (i in seq_along(links)){
                wb$add_hyperlink(dims   = link_pos[[i]],
                                 target = links[[i]])

                wb$add_cell_style(dims       = link_pos[[i]],
                                  horizontal = style[["title_alignment"]],
                                  vertical   = "center",
                                  wrap_text  = "1",
                                  apply_font = TRUE,
                                  font_id    = wb$styles_mgr$get_font_id("title_link_font"))
            }
        }
    }

    # Format footnotes if there are any
    if (length(footnotes) > 0){
        # Check for hyperlinks
        links    <- c()
        link_pos <- c()

        for (i in seq_along(footnotes)){
            footnote <- footnotes[[i]]

            # If there is a link in the text
            if (grepl("link:", footnote)){
                # Get the link and the title range
                links    <- c(links, sub(".*link:", "", footnote))
                link_pos <- c(link_pos,
                              get_excel_range(row    = ranges[["footnote.row"]] + (i - 1),
                                            column = ranges[["title.column"]]))

                # Remove link from the text
                footnotes[[i]] <- sub("link:.*", "", footnote)
            }
        }

        # Paste titles
        wb$add_data(x         = footnotes,
                    start_col = style[["start_column"]],
                    start_row = ranges[["footnote.row"]])

        # Format
        if (output == "excel"){
            # Apply cell styles
            wb$add_cell_style(dims       = ranges[["footnote_range"]],
                              horizontal = style[["footnote_alignment"]],
                              vertical   = "center",
                              wrap_text  = "1",
                              apply_font = TRUE,
                              font_id    = wb$styles_mgr$get_font_id("footnote_font"))

            # Format first footnote row special with a separating line
            first_foot <- get_excel_range(row    = ranges[["footnote.row"]],
                                          column = style[["start_column"]])

            wb$add_cell_style(dims         = first_foot,
                              horizontal   = style[["footnote_alignment"]],
                              vertical     = "center",
                              wrap_text    = "1",
                              apply_font   = TRUE,
                              font_id      = wb$styles_mgr$get_font_id("footnote_font"),
                              apply_border = TRUE,
                              border_id    = wb$styles_mgr$get_border_id("footnote_borders"))

            # Merge the footnotes over the span of the table
            for (footnote in seq_along(footnotes)){
                wb$merge_cells(dims = openxlsx2::wb_dims(
                    cols = style[["start_column"]]:ranges[["table.end"]],
                    rows = ranges[["footnote.row"]] + (footnote - 1)))
            }

            # Set row heights for footnotes if specified
            footnote_heights <- style[["footnote_heights"]]

            if (!is.null(footnote_heights)){
                number_to_rows   <- length(footnotes)
                start_row        <- ranges[["footnote.row"]]
                end_row          <- start_row + number_to_rows - 1

                footnote_heights <- fill_or_trim(footnote_heights,
                                                 number_of_rows)

                wb$set_row_heights(rows    = start_row:end_row,
                                   heights = footnote_heights)
            }

            # Add hyperlinks
            for (i in seq_along(links)){
                wb$add_hyperlink(dims   = link_pos[[i]],
                                 target = links[[i]])

                wb$add_cell_style(dims       = link_pos[[i]],
                                  horizontal = style[["footnote_alignment"]],
                                  vertical   = "center",
                                  wrap_text  = "1",
                                  apply_font = TRUE,
                                  font_id    = wb$styles_mgr$get_font_id("footnote_link_font"))
            }
        }
    }

    wb
}

###############################################################################
# Handle style elements
###############################################################################
#' Handle Cell Styles
#'
#' @description
#' Apply cell styles to a range with all style elements in one go.
#'
#' @param wb The workbook to modify.
#' @param ranges Table ranges to format.
#' @param style A list of style elements to format the table.
#'
#' @return
#' Returns a further formatted workbook.
#'
#' @noRd
handle_cell_styles <- function(wb,
                               ranges,
                               style = excel_output_style()){
    # Apply individual styles for each table part
    for (type in c("header", "box", "cat_col", "table")){
        wb$add_cell_style(dims         = ranges[[paste0(type, "_range")]],
                          horizontal   = style[[paste0(type, "_alignment")]],
                          vertical     = "center",
                          wrap_text    = style[[paste0(type, "_wrap")]],
                          indent       = style[[paste0(type, "_indent")]],
                          apply_font   = TRUE,
                          font_id      = wb$styles_mgr$get_font_id(paste0(type, "_font")),
                          apply_border = TRUE,
                          border_id    = wb$styles_mgr$get_border_id(paste0(type, "_borders")),
                          apply_fill   = TRUE,
                          fill_id      = wb$styles_mgr$get_fill_id(paste0(type, "_fill")))
    }

    wb
}


#' Set Up Different Fill Styles
#'
#' @description
#' Pass fill styles for the different parts of the table into the style manager
#' to apply them more efficiently later.
#'
#' @param wb The workbook to modify.
#' @param style A list of style elements to format the table.
#'
#' @return
#' Returns a workbook with added style elements.
#'
#' @noRd
handle_fill_styles <- function(wb, style = excel_output_style()){
    # Add individual fill styles for each table part
    for (type in c("header", "box", "cat_col", "table")){
        # With color fill
        if (style[[paste0(type, "_back_color")]] != ""){
            wb$styles_mgr$add(
                openxlsx2::create_fill(pattern_type = "solid",
                                       fg_color     = openxlsx2::wb_color(style[[paste0(type, "_back_color")]])),
                paste0(type, "_fill"))
        }
        # With transparent background
        else{
            wb$styles_mgr$add(
                openxlsx2::create_fill(pattern_type = "",
                                       fg_color     = NULL),
                paste0(type, "_fill"))
        }
    }

    wb
}


#' Set Up Different Font Styles
#'
#' @description
#' Pass font styles for the different parts of the table into the style manager
#' to apply them more efficiently later.
#'
#' @param wb The workbook to modify.
#' @param style A list of style elements to format the table.
#'
#' @return
#' Returns a workbook with added style elements.
#'
#' @noRd
handle_font_styles <- function(wb, style = excel_output_style()){
    # Font can only be set globally here. Below in create_font seems to be bugged.
    wb$set_base_font(font_name = style[["font"]])

    # Add individual font styles for each table part
    for (type in c("title", "footnote", "header", "box", "cat_col", "table")){
        wb$styles_mgr$add(
            openxlsx2::create_font(sz    = style[[paste0(type, "_font_size")]],
                                   color = openxlsx2::wb_color(hex = style[[paste0(type, "_font_color")]]),
                                   b     = style[[paste0(type, "_font_bold")]]),
            paste0(type, "_font"))

        # Add hyperlink style
        if (type %in% c("title", "footnote")){
            wb$styles_mgr$add(
                openxlsx2::create_font(sz    = style[[paste0(type, "_font_size")]],
                                       color = openxlsx2::wb_color(hex = "0000FF"),
                                       u     = "single"),
                paste0(type, "_link_font"))
        }
    }

    wb
}


#' Set Up Different Border Styles
#'
#' @description
#' Pass border styles for the different parts of the table into the style manager
#' to apply them more efficiently later.
#'
#' @param wb The workbook to modify.
#' @param style A list of style elements to format the table.
#'
#' @return
#' Returns a workbook with added style elements.
#'
#' @noRd
handle_border_styles <- function(wb, style = excel_output_style()){
    if (style[["header_borders"]]){
        wb$styles_mgr$add(
            openxlsx2::create_border(
                      bottom = "thin",
                      top    = "thin",
                      left   = "thin",
                      bottom_color = openxlsx2::wb_color(hex = style[["header_border_color"]]),
                      top_color    = openxlsx2::wb_color(hex = style[["header_border_color"]]),
                      left_color   = openxlsx2::wb_color(hex = style[["header_border_color"]])),
            "header_borders")
    }
    if (style[["box_borders"]]){
        wb$styles_mgr$add(
            openxlsx2::create_border(
                      bottom = "thin",
                      top    = "thin",
                      right  = "thin",
                      bottom_color = openxlsx2::wb_color(hex = style[["box_border_color"]]),
                      top_color    = openxlsx2::wb_color(hex = style[["box_border_color"]]),
                      right_color  = openxlsx2::wb_color(hex = style[["box_border_color"]])),
            "box_borders")
    }
    if (style[["cat_col_borders"]]){
        wb$styles_mgr$add(
            openxlsx2::create_border(
                      top   = "thin",
                      right = "thin",
                      top_color   = openxlsx2::wb_color(hex = style[["cat_col_border_color"]]),
                      right_color = openxlsx2::wb_color(hex = style[["cat_col_border_color"]])),
            "cat_col_borders")
    }
    if (style[["table_borders"]]){
        wb$styles_mgr$add(
            openxlsx2::create_border(
                      top  = "thin",
                      left = "thin",
                      top_color  = openxlsx2::wb_color(hex = style[["table_border_color"]]),
                      left_color = openxlsx2::wb_color(hex = style[["table_border_color"]])),
            "table_borders")
    }

    wb$styles_mgr$add(
        openxlsx2::create_border(
            top  = "thin",
            top_color  = openxlsx2::wb_color(hex = style[["table_border_color"]])),
        "footnote_borders")

    wb
}

#' Set Up Different Number Format Styles
#'
#' @description
#' Pass number format styles for the different statistics into the style manager
#' to apply them more efficiently later.
#'
#' @param wb The workbook to modify.
#' @param style A list of style elements to format the table.
#'
#' @return
#' Returns a workbook with added style elements.
#'
#' @noRd
handle_number_styles <- function(wb, style = excel_output_style()){
    # Add individual font styles for each table part
    index <- 900

    for (type in c("pct", "freq", "freq.g0", "sum", "sum.wgt", "mean", "median",
                   "mode", "min", "max", "sd", "variance", "first", "last", "p",
                   "missing")){
        wb$styles_mgr$add(
            openxlsx2::create_numfmt(numFmtId   = index,
                                     formatCode = style[["number_formats"]][[paste0(type, "_excel")]]),
            paste0(type, "_numfmt"))

        index <- index + 1
    }

    wb
}


#' Set Up Styles
#'
#' @description
#' Set up all styles in the style manager to apply them more efficiently later.
#'
#' @param wb The workbook to modify.
#' @param style A list of style elements to format the table.
#'
#' @return
#' Returns a workbook with added style elements.
#'
#' @noRd
prepare_styles <- function(wb, style = excel_output_style()){
    wb |>
        handle_fill_styles(style) |>
        handle_font_styles(style) |>
        handle_border_styles(style) |>
        handle_number_styles(style)
}

###############################################################################
# Handle cell dimensions
###############################################################################
#' Handle Column Widths and Row Heights
#'
#' @description
#' Handle column width and row heights depending on specified options.
#'
#' @param wb The workbook to modify.
#' @param ranges Table ranges to format.
#' @param number_of_columns The number of columns that should be formatted.
#' @param number_of_rows The number of rows that should be formatted.
#' @param style A list of style elements to format the table.
#'
#' @return
#' Returns a further formatted workbook.
#'
#' @noRd
handle_col_row_dimensions <- function(wb,
                                      ranges,
                                      number_of_columns,
                                      number_of_rows,
                                      style = excel_output_style()){
    column_widths <- style[["column_widths"]]
    start_column  <- 1
    end_column    <- style[["start_column"]] + (number_of_columns - 1)

    # If specific column widths are specified
    if (length(column_widths) > 1){
        column_widths <- fill_or_trim(column_widths,
                                      end_column)
    }
    # If only one column width is specified
    else if (column_widths != "auto"){
        column_widths <- fill_or_trim(column_widths,
                                      end_column)
    }
    # On auto format only format row header columns. On large workbooks this
    # is very slow so only make sure the text in front are readable.
    else{
        start_column      <- ranges[["header.column"]]
        number_of_columns <- ranges[["header.column"]] + (ranges[["cat_col.width"]] - 1)
    }

    row_heights <- style[["row_heights"]]
    start_row   <- 1
    end_row     <- style[["start_row"]] + (number_of_rows - 1)

    # If specific column widths are specified
    if (length(row_heights) > 1){
        row_heights <- fill_or_trim(row_heights,
                                    end_row)
    }
    # If only one row height is specified
    else if (row_heights != "auto"){
        row_heights <- fill_or_trim(row_heights,
                                    end_row)
    }
    else{
        row_heights <- NULL
    }

    wb$set_col_widths(cols   = start_column:end_column,
                      widths = column_widths)
    wb$set_row_heights(rows    = start_row:end_row,
                       heights = row_heights)

    wb
}


#' Handle Automatic Dimensions
#'
#' @description
#' Handle column width and row heights if they should be automatically adjusted.
#'
#' @param wb The workbook to modify.
#' @param ranges Table ranges to format.
#' @param style A list of style elements to format the table.
#'
#' @return
#' Returns a further formatted workbook.
#'
#' @noRd
handle_auto_dimensions <- function(wb,
                                   ranges,
                                   style = excel_output_style()){
    # If autofit columns is selected, set a manual size after the first table
    # column which is sufficient in most cases. The integrated autofit often
    # gives to narrow columns.
    column_width <- style[["column_widths"]]

    if (length(column_width) == 1){
        if (column_width == "auto"){
            start_col <- style[["start_column"]] + 1

            wb$set_col_widths(cols   = start_col:ranges[["table.end"]],
                              widths = 10)
        }
    }

    # If autofit heights is selected, set a manual size for the table header,
    # since this is the only row to safely say that it has multi row text.
    # There is no good solution to handle the other row. One could only make
    # a vague guess because the actual space each individual font takes at different
    # sizes is unclear. For now the user has to handle multi row texts manually.
    row_heights <- style[["row_heights"]]

    if (length(row_heights) == 1){
        if (row_heights == "auto"){
            wb$set_row_heights(rows    = ranges[["header.row"]],
                               heights = 30)
        }
    }

    wb
}


#' Handle Automatic Dimensions For Any_Table
#'
#' @description
#' Handle column width and row heights if they should be automatically adjusted in
#' an any_table.
#'
#' @param wb The workbook to modify.
#' @param ranges Table ranges to format.
#' @param style A list of style elements to format the table.
#'
#' @return
#' Returns a further formatted workbook.
#'
#' @noRd
handle_any_auto_dimensions <- function(wb,
                                       ranges,
                                       style = excel_output_style()){
    # If autofit columns is selected, set a manual size after the table row header
    # columns which is sufficient in most cases. The integrated autofit often
    # gives to narrow columns.
    column_width <- style[["column_widths"]]

    if (length(column_width) == 1){
        if (column_width == "auto"){
            start_col <- style[["start_column"]] + ranges[["cat_col.width"]]

            wb$set_col_widths(cols   = start_col:ranges[["table.end"]],
                              widths = 9)
        }
    }

    wb
}


#' Handle Header and Table Heights
#'
#' @description
#' Handle header and table row heights if they should be adjusted individually.
#'
#' @param wb The workbook to modify.
#' @param ranges Table ranges to format.
#' @param style A list of style elements to format the table.
#'
#' @return
#' Returns a further formatted workbook.
#'
#' @noRd
handle_header_table_dim <- function(wb,
                                    ranges,
                                    style = excel_output_style()){
    # Set row heights for header if specified
    header_heights <- style[["header_heights"]]

    if (!is.null(header_heights)){
        number_of_rows <- max(ranges[["header.length"]], 1)
        start_row      <- ranges[["header.row"]]
        end_row        <- start_row + max((number_of_rows - 1), 0)

        header_heights <- fill_or_trim(header_heights,
                                       number_of_rows)

        wb$set_row_heights(rows    = start_row:end_row,
                           heights = header_heights)
    }

    # Set row heights for table if specified
    table_heights <- style[["table_heights"]]

    if (!is.null(table_heights)){
        number_of_rows <- ranges[["table.length"]]
        start_row      <- ranges[["table.row"]]
        end_row        <- start_row + (number_of_rows - 1)

        table_heights <- fill_or_trim(table_heights,
                                      number_of_rows)

        wb$set_row_heights(rows    = start_row:end_row,
                           heights = table_heights)
    }

    wb
}


#' Adjust Format Vector to Needed Length
#'
#' @description
#' Either fill up or trim down the provided format vector to the needed number
#' of columns/rows which have to be formatted.
#'
#' @param format_vector Format vector to be adjusted.
#' @param number_to_format Needed length to be adjusted.
#'
#' @return
#' Returns a an adjusted format vector.
#'
#' @noRd
fill_or_trim <- function(format_vector,
                         number_to_format){
    # If length of provided vector is lesser than there are places to format
    if (length(format_vector) < number_to_format){
        # Repeat the last vector element until the number to format is reached
        format_vector <- c(format_vector,
                           rep(utils::tail(format_vector, 1),
                               number_to_format - length(format_vector)))
    }
    # If length of provided vector is equal or greater than there are places to format
    else{
        # Cut elements down to number of places to format
        format_vector <- utils::head(format_vector, number_to_format)
    }

    format_vector
}

###############################################################################
# Style options
###############################################################################
#' Style for 'Excel' Table Outputs
#'
#' @description
#' Set different options which define the visual output of 'Excel' tables produced
#' by [frequencies()], [crosstabs()] and [any_table()].
#'
#' @param file If NULL, opens the output as temporary file. If a filename with path
#' is specified, saves the output to the specified path.
#' @param sheet_name Name of the sheet inside the workbook to which the output shall be written.
#' If multiple outputs are produced in one go, the sheet name additionally receives a running number.
#' @param font Set the font to be used for the entire output.
#' @param column_widths Specify whether column widths should be set automatically and individually or
#' if a numeric vector is passed each column width can be specified manually. If a table has more
#' columns than column widths are provided, the last given column width will be repeated until the
#' end of the table.
#' @param row_heights Specify whether row heights should be set automatically and individually or
#' if a numeric vector is passed each row height can be specified manually. If a table has more
#' rows than row heights are provided, the last given row height will be repeated until the
#' end of the table.
#' @param title_heights Set individual row heights for the titles only.
#' @param header_heights Set individual row heights for the table header only.
#' @param table_heights Set individual row heights for the table body only.
#' @param footnote_heights Set individual row heights for the footnotes only.
#' @param start_row The row in which the table starts.
#' @param start_column The column in which the table starts.
#' @param freeze_col_header Whether to freeze the column header so that it is always visible
#' while scrolling down the document.
#' @param freeze_row_header Whether to freeze the row header so that it is always visible
#' while scrolling sideways in the document.
#' @param filters Whether to set filters in the column header, when exporting a data frame.
#' @param grid_lines Whether to show grid lines or not.
#' @param header_back_color Background cell color of the table header.
#' @param header_font_color Font color of the table header.
#' @param header_font_size Font size of the table header.
#' @param header_font_bold Whether to print the table header in bold letters.
#' @param header_alignment Set the text alignment of the table header.
#' @param header_wrap Whether to wrap the texts in the table header.
#' @param header_indent Indentation level of the table header.
#' @param header_borders Whether to draw borders around the table header cells.
#' @param header_border_color Borders colors of the table header cells.
#' @param cat_col_back_color Background cell color of the category columns inside the table.
#' @param cat_col_font_color Font color of the category columns inside the table.
#' @param cat_col_font_size Font size of the category columns inside the table.
#' @param cat_col_font_bold Whether to print the category columns inside the table in bold letters.
#' @param cat_col_alignment Set the text alignment of the category columns inside the table.
#' @param cat_col_wrap Whether to wrap the texts in the category columns inside the table.
#' @param cat_col_indent Indentation level of the category columns inside the table.
#' @param cat_col_borders Whether to draw borders around the category columns inside the table.
#' @param cat_col_border_color Borders colors of the category columns inside the table.
#' @param table_back_color Background color of the inner table cells.
#' @param table_font_color Font color of the inner table cells.
#' @param table_font_size Font size of the inner table cells.
#' @param table_font_bold Whether to print the inner table cells in bold numbers
#' @param table_alignment Set the text alignment of the inner table cells.
#' @param table_indent Indentation level of the inner table cells.
#' @param table_borders Whether to draw borders around the inner table cells.
#' @param table_border_color Borders colors of the inner table cells.
#' @param box_back_color Background color of the left box in table header.
#' @param box_font_color Font color of the left box in table header.
#' @param box_font_size Font size of the left box in table header.
#' @param box_font_bold Whether to print the left box in table header in bold letters.
#' @param box_alignment Set the text alignment of the left box in table header.
#' @param box_wrap Whether to wrap the texts in the left box in table header.
#' @param box_indent Indentation level of the left box in table header.
#' @param box_borders Whether to draw borders around the left box in table header.
#' @param box_border_color Borders colors of the left box in table header.
#' @param number_formats Put in a list of number formats which should be assigned to
#' the different stats. Number formats can be created with [number_format_style()].
#' @param title_font_color Font color of the titles.
#' @param title_font_size Font size of the tables titles.
#' @param title_font_bold Whether to print the tables titles in bold letters.
#' @param title_alignment Set the text alignment of the titles.
#' @param footnote_font_color Font color of the footnotes
#' @param footnote_font_size Font size of the tables footnotes
#' @param footnote_font_bold Whether to print the tables footnotes in bold letters.
#' @param footnote_alignment Set the text alignment of the footnotes.
#' @param na_symbol Define the symbol that should be used for NA values.
#'
#' @details
#' [excel_output_style()] is based on the Output Delivery System (ODS) in 'SAS',
#' which provides efficient and readable ways to set up different table styles.
#'
#' With the output style you have full control over the table design. There is no
#' need to think about calculating the right place to input a background color or a
#' border of a certain type and how to do this in a loop for multiple cells. Just
#' input colors, borders, font styles, etc. for the different table parts and everything
#' else is handled by the functions capable of using styles.
#'
#' The concept basically is: design over complex calculations.
#'
#' @return
#' Returns a list of named style options.
#'
#' @seealso
#' Creating a custom table style: [modify_output_style()],
#' [number_format_style()], [modify_number_formats()].
#'
#' Functions that can handle styles: [frequencies()], [crosstabs()], [any_table()],
#' [export_with_style()]
#'
#' @examples
#' # For default values
#' excel_style <- excel_output_style()
#'
#' # Set specific options, the rest will be set to default values
#' excel_style <- excel_output_style(font       = "Calibri",
#'                                   sheet_name = "My_Output")
#'
#' # For cells with no background color pass an empty string
#' excel_style <- excel_output_style(table_back_color = "")
#'
#' @export
excel_output_style <- function(file                 = NULL,
                               sheet_name           = "Table",
                               font                 = "Arial",
                               column_widths        = "auto",
                               row_heights          = "auto",
                               title_heights        = NULL,
                               header_heights       = NULL,
                               table_heights        = NULL,
                               footnote_heights     = NULL,
                               start_row            = 2,
                               start_column         = 2,
                               freeze_col_header    = FALSE,
                               freeze_row_header    = FALSE,
                               filters              = TRUE,
                               grid_lines           = TRUE,
                               header_back_color    = "FFFFFF",
                               header_font_color    = "000000",
                               header_font_size     = 10,
                               header_font_bold     = TRUE,
                               header_alignment     = "center",
                               header_wrap          = "1",
                               header_indent        = 0,
                               header_borders       = TRUE,
                               header_border_color  = "000000",
                               cat_col_back_color   = "FFFFFF",
                               cat_col_font_color   = "000000",
                               cat_col_font_size    = 10,
                               cat_col_font_bold    = FALSE,
                               cat_col_alignment    = "left",
                               cat_col_wrap         = "1",
                               cat_col_indent       = 1,
                               cat_col_borders      = TRUE,
                               cat_col_border_color = "000000",
                               table_back_color     = "FFFFFF",
                               table_font_color     = "000000",
                               table_font_size      = 10,
                               table_font_bold      = FALSE,
                               table_alignment      = "right",
                               table_indent         = 1,
                               table_borders        = FALSE,
                               table_border_color   = "000000",
                               box_back_color       = "FFFFFF",
                               box_font_color       = "000000",
                               box_font_size        = 10,
                               box_font_bold        = TRUE,
                               box_alignment        = "center",
                               box_wrap             = "1",
                               box_indent           = 0,
                               box_borders          = TRUE,
                               box_border_color     = "000000",
                               number_formats       = number_format_style(),
                               title_font_color     = "000000",
                               title_font_size      = 10,
                               title_font_bold      = TRUE,
                               title_alignment      = "left",
                               footnote_font_color  = "000000",
                               footnote_font_size   = 8,
                               footnote_font_bold   = FALSE,
                               footnote_alignment   = "left",
                               na_symbol            = "."){

    as.list(environment())
}


#' Modify Style for 'Excel' Table Outputs
#'
#' @description
#' Modify a previously created style with [excel_output_style()].
#'
#' @param style_to_modify A pre created style where only certain elements should be
#' modified while the rest is kept as is.
#' @param ... Pass in names and corresponding new values for existing style elements.
#'
#' @details
#' [modify_output_style()] is based on the Output Delivery System (ODS) in 'SAS',
#' which provides efficient and readable ways to set up different table styles.
#'
#' With the output style you have full control over the table design. There is no
#' need to think about calculating the right place to input a background color or a
#' border of a certain type and how to do this in a loop for multiple cells. Just
#' input colors, borders, font styles, etc. for the different table parts and everything
#' else is handled by the functions capable of using styles.
#'
#' The concept basically is: design over complex calculations.
#'
#' @return
#' Returns a modified list of named style options.
#'
#' @seealso
#' Creating a custom table style: [excel_output_style()],
#' [number_format_style()], [modify_number_formats()].
#'
#' Functions that can handle styles: [frequencies()], [crosstabs()], [any_table()],
#' [export_with_style()]
#'
#' @examples
#' # For default values
#' excel_style <- excel_output_style()
#'
#' # Set specific options, the rest will be kept as is
#' excel_style <- excel_style |> modify_output_style(sheet_name      = "Sheet",
#'                                                   title_font_bold = FALSE)
#'
#' # For cells with no background color pass an empty string
#' excel_style <- excel_style |> modify_output_style(table_back_color = "")
#'
#' @export
modify_output_style <- function(style_to_modify, ...){
    style_elements <- list(...)

    # Loop through all elements to modify and if exists then set new value
    for (element in seq_along(style_elements)){
        name <- names(style_elements)[element]

        if (!name %in% names(style_to_modify)){
            message(" ! WARNING: Style element '", name, "' is invalid and will be omitted.")
        }

        style_to_modify[[name]] <- style_elements[[element]]
    }

    style_to_modify
}


###############################################################################
# Number formats for any_table
###############################################################################
#' Number Formats Used by [any_table()]
#'
#' @description
#' Set individual number formats for the different statistics in tables produced
#' with [any_table()].
#'
#' @param pct_excel Number format for percentage applied in Excel workbook.
#' @param freq_excel Number format for frequency applied in Excel workbook.
#' @param freq.g0_excel Number format for frequency greater zero applied in Excel workbook.
#' @param sum_excel Number format for sum applied in Excel workbook.
#' @param sum.wgt_excel Number format for sum of weights applied in Excel workbook.
#' @param mean_excel Number format for mean applied in Excel workbook.
#' @param median_excel Number format for median applied in Excel workbook.
#' @param mode_excel Number format for mode applied in Excel workbook.
#' @param min_excel Number format for min applied in Excel workbook.
#' @param max_excel Number format for max applied in Excel workbook.
#' @param sd_excel Number format for sd applied in Excel workbook.
#' @param variance_excel Number format for variance applied in Excel workbook.
#' @param first_excel Number format for first applied in Excel workbook.
#' @param last_excel Number format for last applied in Excel workbook.
#' @param missing_excel Number format for missing applied in Excel workbook.
#' @param p_excel Number format for percentile applied in Excel workbook.
#' @param pct_decimals Number of decimals for percentage.
#' @param freq_decimals Number of decimals for frequency.
#' @param freq.g0_decimals Number of decimals for frequency greater zero.
#' @param sum_decimals Number of decimals for sum.
#' @param sum.wgt_decimals Number of decimals for sum of weights.
#' @param mean_decimals Number of decimals for mean.
#' @param median_decimals Number of decimals for median.
#' @param mode_decimals Number of decimals for mode.
#' @param min_decimals Number of decimals for min.
#' @param max_decimals Number of decimals for max.
#' @param sd_decimals Number of decimals for sd.
#' @param variance_decimals Number of decimals for variance.
#' @param first_decimals Number of decimals for first.
#' @param last_decimals Number of decimals for last.
#' @param missing_decimals Number of decimals for missing.
#' @param p_decimals Number of decimals for percentile.
#'
#' @details
#' [number_format_style()] is based on 'SAS' number formats and the Output Delivery
#' System (ODS), which provides efficient and readable ways to set up different table
#' styles.
#'
#' With the number format style you have full control over formatting numbers according
#' to the different statistics. There is no need to think about calculating the right
#' place to input the number formats and how to do this in a loop for multiple cells.
#' Just input the different number formats and decimals for the different statistics
#' and everything else is handled by the functions capable of using number styles.
#'
#' The concept basically is: design over complex calculations.
#'
#' @return
#' Returns a list of named number format options.
#'
#' @seealso
#' Creating a custom table style: [excel_output_style()], [modify_output_style()],
#' [modify_number_formats()].
#'
#' Functions that can handle styles: [frequencies()], [crosstabs()], [any_table()],
#' [export_with_style()]
#'
#' @examples
#' # For default values
#' format_list <- number_format_style()
#'
#' # Set specific options, the rest will be set to default values
#' format_list <- number_format_style(pct_excel    = "0.00000000",
#'                                    pct_decimals = 8)
#'
#' # IMPORTANT: Don't forget to add individual formats to an excel style, otherwise
#' # they won't come into affect.
#' excel_style <- excel_output_style(number_formats = format_list)
#'
#' @export
number_format_style <- function(pct_excel         = "0.0",
                                freq_excel        = "#,###,##0",
                                freq.g0_excel     = "#,###,##0",
                                sum_excel         = "#,###,##0",
                                sum.wgt_excel     = "#,###,##0",
                                mean_excel        = "#,###,##0",
                                median_excel      = "#,###,##0",
                                mode_excel        = "#,###,##0",
                                min_excel         = "#,###,##0",
                                max_excel         = "#,###,##0",
                                sd_excel          = "#,###,##0.000",
                                variance_excel    = "#,###,##0.000",
                                first_excel       = "#,###,##0",
                                last_excel        = "#,###,##0",
                                p_excel           = "#,###,##0",
                                missing_excel     = "#,###,##0",
                                pct_decimals      = 1,
                                freq_decimals     = 0,
                                freq.g0_decimals  = 0,
                                sum_decimals      = 3,
                                sum.wgt_decimals  = 3,
                                mean_decimals     = 2,
                                median_decimals   = 2,
                                mode_decimals     = 2,
                                min_decimals      = 2,
                                max_decimals      = 2,
                                sd_decimals       = 3,
                                variance_decimals = 3,
                                first_decimals    = 0,
                                last_decimals     = 0,
                                p_decimals        = 2,
                                missing_decimals  = 0){

    as.list(environment())
}


#' Modify Number Formats Used by [any_table()]
#'
#' @description
#' Modify previously created number formats with [number_format_style()].
#'
#' @param formats_to_modify Pre created number formats where only certain elements
#' should be modified while the rest is kept as is.
#' @param ... Pass in names and corresponding new values for existing number formats.
#'
#' @details
#' [modify_number_formats()] is based on 'SAS' number formats and the Output Delivery
#' System (ODS), which provides efficient and readable ways to set up different table
#' styles.
#'
#' With the number format style you have full control over formatting numbers according
#' to the different statistics. There is no need to think about calculating the right
#' place to input the number formats and how to do this in a loop for multiple cells.
#' Just input the different number formats and decimals for the different statistics
#' and everything else is handled by the functions capable of using number styles.
#'
#' The concept basically is: design over complex calculations.
#'
#' @return
#' Returns a modified list of number format options.
#'
#' @seealso
#' Creating a custom table style: [excel_output_style()], [modify_output_style()],
#' [number_format_style()].
#'
#' Functions that can handle styles: [frequencies()], [crosstabs()], [any_table()],
#' [export_with_style()].
#'
#' @examples
#' # For default values
#' format_list <- number_format_style(pct_excel    = "0.00000000",
#'                                    pct_decimals = 8)
#'
#' # Set specific options, the rest will be kept as is
#' format_list <- format_list |> modify_number_formats(sum_excel = "#,###,##0.000")
#'
#' # IMPORTANT: Don't forget to add individual formats to an excel style, otherwise
#' # they won't come into affect.
#' excel_style <- excel_output_style(number_formats = format_list)
#'
#' @export
modify_number_formats <- function(formats_to_modify, ...){
    format_elements <- list(...)

    # Loop through all elements to modify and if exists then set new value
    for (element in seq_along(format_elements)){
        name <- names(format_elements)[element]

        if (!name %in% names(formats_to_modify)){
            message(" ! WARNING: Number format '", name, "' is invalid and will be omitted.")
        }

        formats_to_modify[[name]] <- format_elements[[element]]
    }

    formats_to_modify
}
