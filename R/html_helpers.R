#' Render Any Table As html
#'
#' @description
#' Renders a table created with [any_table()] as an html table in a new browser tab.
#' The table mimics the appearance of the 'Excel' output as closely as possible.
#'
#' @param table The result list created with [any_table()].
#' @param print TRUE by default. If TRUE prints the output, if FALSE doesn't print anything. Can be used
#' if one only wants to catch the output data frame and workbook with meta information.
#'
#' @return
#' Returns the html output as a single character string.
#'
#' @noRd
format_any_html <- function(table, print){
    any_tab <- table[["table"]]
    meta    <- table[["meta"]]

    # Build the html output
    html <- render_any_table_html(any_tab,
                                  rows        = meta[["rows"]],
                                  columns     = meta[["columns"]],
                                  statistics  = meta[["statistics"]],
                                  by          = meta[["by"]],
                                  titles      = meta[["titles"]],
                                  footnotes   = meta[["footnotes"]],
                                  var_labels  = meta[["var_labels"]],
                                  stat_labels = meta[["stat_labels"]],
                                  box         = meta[["box"]],
                                  any_header  = meta[["any_header"]],
                                  style       = meta[["style"]],
                                  na.rm       = meta[["na.rm"]],
                                  print_miss  = meta[["print_miss"]])

    # Display in the viewer or browser
    if (print){
        # The table is rendered from a temporary file. The temporary file is
        # only needed to display the table in the browser and is not meant to
        # be saved.
        file <- file.path(tempdir(), paste0("qol_table_", as.integer(Sys.time()), ".html"))

        writeLines(html, file, useBytes = TRUE)

        utils::browseURL(file)
    }

    invisible(html)
}


#' Render Any Table as html
#'
#' @description
#' Builds the full html representation of an [any_table()] output. The table
#' is constructed from the meta information and the styling options are translated
#' into CSS.
#'
#' @param any_tab The data frame which contains the information for this table.
#' @param rows The variable combinations that appear in the table rows.
#' @param columns The variable combinations that appear in the table columns.
#' @param statistics The computed statistics.
#' @param by Separate the table output by the expressions of the provided variables.
#' @param titles Character vector of titles to display above the table.
#' @param footnotes Character vector of footnotes to display under the table.
#' @param var_labels List which contains variable names and their respective labels.
#' @param stat_labels List which contains statistic names and their respective labels.
#' @param box The text that should appear in the upper left box of the table.
#' @param any_header The column header carrying the variable names.
#' @param style A list containing the styling elements.
#' @param na.rm If TRUE removes all NA values from the tabulation.
#' @param print_miss If TRUE outputs all possible categories of the grouping
#' variables based on the provided formats.
#'
#' @return
#' Returns the html output as a single character string.
#'
#' @noRd
render_any_table_html <- function(any_tab,
                                  rows        = "",
                                  columns     = "",
                                  statistics  = NULL,
                                  by          = c(),
                                  titles      = c(),
                                  footnotes   = c(),
                                  var_labels  = list(),
                                  stat_labels = list(),
                                  box         = "",
                                  any_header  = NULL,
                                  style       = excel_output_style(),
                                  na.rm       = FALSE,
                                  print_miss  = FALSE){

    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # Rebuild the multi layered column header
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    # Keep the original column names for the data selection in the body and
    # compute the adjusted header names separately.
    orig_names <- names(any_tab)

    # Cut down percentage names to just "pct"
    header_names <- orig_names
    header_names <- gsub("pct_group_", "pct group ",     header_names)
    header_names <- gsub("pct_total",  "pct total",      header_names)
    header_names <- gsub("pct_value",  "pct value",      header_names)
    header_names <- gsub("pct_block_", "pct block ",     header_names)
    header_names <- gsub("freq_g0",    "freq g0",        header_names)
    header_names <- gsub("sum_wgt",    "weight_sum wgt", header_names)

    # The by variables are rendered as separate blocks in the body and must not
    # be part of the column header construction. Like in the excel formatting
    # part they are dropped before the header is built. This is important because
    # "by_vars" contains an underscore and would otherwise end up in the header.
    by_vars             <- c()
    names_no_by         <- header_names
    number_of_by_blocks <- 0

    if (length(by) > 0){
        by_vars             <- intersect(c("BY", "by_vars"), names(any_tab))
        names_no_by         <- setdiff(names_no_by, intersect(by_vars, names_no_by))
        number_of_by_blocks <- collapse::fnrow(collapse::funique(collapse::get_vars(any_tab, by_vars)))
    }

    # Build header from variable names
    multi_header <- build_multi_header(names_no_by, any_header, var_labels, style)

    # Remove empty statistics rows, but keep multi_header with statistics because the information
    # is needed below for applying the correct number formats.
    column_header <- multi_header |> set_statistic_labels(stat_labels)
    column_header <- column_header[rowSums(column_header == "") != collapse::fncol(column_header), , drop = FALSE]

    stats_row <- multi_header[collapse::fnrow(multi_header), , drop = FALSE]

    # Get table ranges
    any_ranges <- get_any_tab_ranges(suppressMessages(any_tab |> dropp(by_vars)), column_header, stats_row,
                                     titles, footnotes, style)

    # Add empty columns to the header for the top left box at the beginning
    blank_columns <- matrix("", nrow = collapse::fnrow(column_header), ncol = any_ranges[["cat_col.width"]])
    column_header <- cbind(blank_columns, column_header)

    # Add box text
    if (box != ""){
        column_header[1, 1] <- box
    }
    # If no box text provided put in variable names of row headers
    else{
        column_header[1, 1] <- gsub("!!!", "_", paste(rows, collapse = "\n"))
    }

    # Insert empty rows and columns before the titles and the table
    start_row    <- max(style[["start_row"]][1]    - 1, 0)
    start_column <- max(style[["start_column"]][1] - 1, 0)

    # Calculate the the total rows the entire output has
    row_heights   <- style[["row_heights"]]
    has_titles    <- length(titles)    > 0
    has_footnotes <- length(footnotes) > 0

    # In case of auto measuring just set the row heights to NULL
    if (is.null(row_heights) || (is.character(row_heights) && row_heights == "auto")){
        row_heights <- NULL
    }
    # Otherwise set up the row heights for all output rows
    else{
        # Calculate titles and footnotes for every by variable expression plus
        # the extra gap lines to the table.
        number_of_titles    <- ifelse(has_titles,    (number_of_by_blocks + 1) * (length(titles)    + 1), 0)
        number_of_footnotes <- ifelse(has_footnotes, (number_of_by_blocks + 1) * (length(footnotes) + 1), 0)

        # Put together number of rows to know of many rows to format
        total_rows <- start_row + number_of_titles +
                      collapse::fnrow(multi_header) + collapse::fnrow(any_tab) +
                      number_of_footnotes + (number_of_by_blocks * 2)

        row_heights <- as.numeric(fill_or_trim(row_heights, total_rows))
    }

    # Split the row heights into the individual parts of the table
    if (is.null(row_heights)){
        row_heights_lead     <- NULL
        row_heights_title    <- NULL
        row_heights_mid      <- NULL
        row_heights_header   <- NULL
        row_heights_body     <- NULL
        row_heights_fn_spare <- NULL
        row_heights_footnote <- NULL
    }
    else{
        position <- 1

        row_heights_lead <- row_heights[position:(position + start_row - 1)]
        position         <- position + start_row

        row_heights_title <- row_heights[position:(position + length(titles) - 1)]
        position          <- position + length(titles)

        row_heights_mid <- if (has_titles) row_heights[position] else NULL
        position        <- position + 1

        row_heights_header <- row_heights[position:(position + collapse::fnrow(multi_header) - 1)]
        position           <- position + collapse::fnrow(multi_header)

        row_heights_body <- row_heights[position:(position + collapse::fnrow(any_tab) + number_of_by_blocks - 1)]
        position         <- position + collapse::fnrow(any_tab) + number_of_by_blocks

        row_heights_fn_spare <- if (has_footnotes) row_heights[position] else NULL
        position             <- position + 1

        row_heights_footnote <- row_heights[position:(position + length(footnotes) - 1)]
    }

    # Part specific row heights overrule the global row_heights for their respective part
    row_heights_title     <- get_part_heights(style[["title_heights"]],     length(titles),                row_heights_title)
    row_heights_header    <- get_part_heights(style[["header_heights"]],    collapse::fnrow(multi_header), row_heights_header)
    row_heights_footnote  <- get_part_heights(style[["footnote_heights"]],  length(footnotes),             row_heights_footnote)
    row_heights_subheader <- get_part_heights(style[["subheader_heights"]], number_of_by_blocks,           NULL)
    row_heights_table     <- get_part_heights(style[["table_heights"]],     collapse::fnrow(any_tab),      NULL)

    # Get the per column statistic to apply the correct number formats
    stats_row     <- as.character(unlist(multi_header[collapse::fnrow(multi_header), ]))
    stat_decimals <- vapply(stats_row, function(stat){
        style[["number_formats"]][[paste0(stat, "_decimals")]]
    }, numeric(1))

    # Generate a list with formatting information about each merged cell from the
    # column header.
    header_cells <- get_column_header_cell_tree(column_header,
                                                any_ranges[["cat_col.width"]],
                                                style[["header_stat_merging"]],
                                                any_ranges[["block_lengths"]],
                                                any_ranges[["block_values"]])

    # Separate row header and value columns
    value_start      <- any_ranges[["cat_col.width"]] + 1 + length(by_vars)
    row_header_names <- orig_names[1:any_ranges[["cat_col.width"]]]
    value_names      <- orig_names[value_start:collapse::fncol(any_tab)]

    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # Column widths
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    # In case of automatic column width just setup a general automatic styling
    table_columns <- collapse::fncol(any_tab) - length(by_vars)
    column_widths <- style[["column_widths"]]

    # In case of automatic column width just setup a general automatic styling
    if (is.null(column_widths) || is.character(column_widths) && column_widths == "auto"){
        colgroup_html    <- ""
        table_width_html <- ""

        if (start_column > 0){
            sheet_width_html <- paste0(' style="margin-left: ', start_column * 8, 'ch"')
        }
        else{
            sheet_width_html <- ""
        }
    }
    # If column widths are provided setup distinct column groups
    else{
        sheet_columns       <- start_column + table_columns
        column_widths       <- fill_or_trim(column_widths, sheet_columns)
        table_column_widths <- column_widths[(start_column + 1):sheet_columns]

        colgroup_html <- paste0("<colgroup>",
                                paste(vapply(as.numeric(table_column_widths), function(width){
                                                 paste0('<col style="width: ', width, 'ch;">')
                                             }, character(1)), collapse = ""),
                                "</colgroup>")

        # The browser only applies the specified column widths when the table
        # has a definite width.
        table_width_html <- paste0(' style="width: ', sum(as.numeric(table_column_widths)), 'ch"')

        if (start_column > 0){
            sheet_width_html <- paste0(' style="margin-left: ', sum(as.numeric(column_widths[1:start_column])), 'ch"')
        }
        else{
            sheet_width_html <- ""
        }
    }

    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # By variables as individual tables
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    # In case by variables are used and the by expressions should not be merged
    # into one table with subheaders, every by expression is rendered as its own
    # table with its own titles and footnotes.
    if (length(by) > 0 && !style[["by_as_subheaders"]]){
        return(build_by_tables_html(any_tab,
                                    header_cells,
                                    row_heights_header,
                                    row_header_names,
                                    value_names,
                                    stat_decimals,
                                    style,
                                    titles,
                                    footnotes,
                                    row_heights_title,
                                    row_heights,
                                    row_heights_mid,
                                    row_heights_footnote,
                                    row_heights_fn_spare,
                                    row_heights_table,
                                    row_heights_body,
                                    start_row,
                                    row_heights_lead,
                                    colgroup_html,
                                    table_width_html,
                                    sheet_width_html))
    }

    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # Build the table body
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    body_html <- build_body_html(any_tab,
                                 by,
                                 by_vars,
                                 row_header_names,
                                 value_names,
                                 stat_decimals,
                                 style,
                                 row_heights_subheader,
                                 row_heights,
                                 row_heights_table,
                                 row_heights_body)

    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # Put the html document together
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    # Collect all parts of the column header.
    thead_parts <- list("<thead>")

    # Process every header row individually
    for (rows in seq_along(header_cells)){
        # Convert every header cell of the current row into its html representation
        cell_html <- vapply(header_cells[[rows]], function(cell){
            attrs <- ""

            if (cell[["colspan"]] > 1){
                attrs <- paste0(attrs, ' colspan="', cell[["colspan"]], '"')
            }
            if (cell[["rowspan"]] > 1){
                attrs <- paste0(attrs, ' rowspan="', cell[["rowspan"]], '"')
            }

            # Assemble the complete <th> element
            paste0('<th', attrs, ' class="', cell[["cls"]], '">',
                   replace_html_escape(cell[["value"]]), '</th>')
        }, character(1))

        # Apply an explicit row height if supplied by the user
        if (is.null(row_heights_header)){
            row_height <- ""
        }
        else{
            row_height <- paste0(' style="height: ', row_heights_header[rows], 'pt"')
        }

        # Combine all cells into one html table row
        thead_parts[[length(thead_parts) + 1]] <- paste0("<tr", row_height, ">",
                                                         paste(cell_html, collapse = ""), "</tr>")
    }

    thead_parts[[length(thead_parts) + 1]] <- "</thead>"

    # Titles and footnotes
    titles_html <- ""

    if (length(titles) > 0){
        # Convert every title into its html representation
        title_parts <- vapply(seq_along(titles), function(i){
            build_title_html(titles[i],
                             style,
                             type   = "title",
                             i      = i,
                             height = if (is.null(row_heights_title)) NULL else row_heights_title[i])
        }, character(1))

        # Spacer between the titles and the table
        if (is.null(row_heights)){
            mid_spacer_html <- "<div class=\"qol-spacer\"></div>"
        }
        else{
            mid_spacer_html <- paste0('<div class="qol-spacer" style="height: ', row_heights_mid, 'pt"></div>')
        }

        # Combine all title elements into one html block
        titles_html <- paste0(paste(title_parts, collapse = "\n"), "\n", mid_spacer_html)
    }

    footnotes_html <- ""

    if (length(footnotes) > 0){
        # Convert every footnote into its html representation
        footnote_parts <- vapply(seq_along(footnotes), function(i){
            # The first footnote receives an additional CSS class so it can be styled
            # differently.
            if (i == 1){
                cls <- "qol-footnotes qol-footnotes-first"
            }
            else{
                cls <- "qol-footnotes"
            }

            build_title_html(footnotes[i],
                             style,
                             cls    = cls,
                             type   = "footnote",
                             i      = i,
                             height = if (is.null(row_heights_footnote)) NULL else row_heights_footnote[i])
        }, character(1))

        # Spacer between the table and the footnotes
        if (is.null(row_heights)){
            fn_spacer_html <- "<div class=\"qol-spacer\"></div>"
        }
        else{
            fn_spacer_html <- paste0('<div class="qol-spacer" style="height: ', row_heights_fn_spare, 'pt"></div>')
        }

        # Combine spacer and footnotes into one html block
        footnotes_html <- paste0(fn_spacer_html, "\n", paste(footnote_parts, collapse = "\n"))
    }

    # Leading empty rows created by start_row
    top_spacers_html <- ""

    if (start_row > 0){
        if (is.null(row_heights)){
            top_spacers_html <- paste(rep("<div class=\"qol-spacer\"></div>", start_row), collapse = "\n")
        }
        else{
            top_spacers_html <- paste(vapply(row_heights_lead, function(height){
                                                 paste0('<div class="qol-spacer" style="height: ', height, 'pt"></div>')
                                             }, character(1)), collapse = "\n")
        }
    }

    # Put together the entire table
    sheet_html <- paste0('<div class="qol-sheet"', sheet_width_html, '>')

    # The titles and footnotes must be aligned relative to the table width and
    # not to the page width. As the rendered table width is only known after
    # the browser laid out the table, a script measures the tables and applies
    # the measured width to the titles and footnotes of the same sheet. It runs
    # while the document is parsed. Longer titles and footnotes then wrap at the
    # table width.
    width_js_html <- ""

    if (length(titles) > 0 || length(footnotes) > 0){
        width_js_html <- js_measure_table_width()
    }

    # Put the html document together
    paste0("<!DOCTYPE html>
           <html lang=\"en\">
               <head>
                   <meta charset=\"utf-8\">
                   <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">
                   <title>qol table</title>
                   <style>
                       ", build_table_css(style), "
                   </style>
               </head>
               <body>
               ", top_spacers_html,
                    if (nzchar(top_spacers_html)) "\n",
                    sheet_html, "
               ", titles_html, "
               <div class=\"qol-wrap\">
                   <table class=\"qol-table\"", table_width_html, ">
                       ", colgroup_html, "
                       ", paste(thead_parts, collapse = "\n"), "
                       <tbody>
                       ", body_html, "
                       </tbody>
                   </table>
               </div>
               ", footnotes_html, "
               </div>
               ", width_js_html, "
               </body>
           </html>")
}


#' @description
#' In case by variables are used and the by expressions should not be merged
#' into one table with subheaders, every by expression is rendered as its own
#' table with its own titles and footnotes.
#'
#' @param any_tab The data frame which contains the information for this table.
#' @param header_cells The list of header cell formats.
#' @param row_heights_header Heights of each row of the column header.
#' @param row_header_names Names of the row header columns.
#' @param value_names Names of the value columns.
#' @param stat_decimals The number of decimals per value column.
#' @param style A list containing the styling elements.
#' @param titles Character vector of titles to display above the table.
#' @param footnotes Character vector of footnotes to display under the table.
#' @param row_heights_title Heights of the rows of the titles.
#' @param row_heights Heights of the table rows.
#' @param row_heights_mid Heights of the mid title row.
#' @param row_heights_footnote Heights of the rows of the footnotes.
#' @param row_heights_fn_spare Heights of the spare footnote row.
#' @param row_heights_table Heights of the table rows per table.
#' @param row_heights_body Heights of the table rows.
#' @param start_row Number of empty rows before the table.
#' @param row_heights_lead Heights of the leading empty rows.
#' @param colgroup_html Html code for the table column groups.
#' @param table_width_html Html code for the table width.
#' @param sheet_width_html Html code for the sheet width.
#'
#' @return The full html output for all by tables as a single character string.
#'
#' @noRd
build_by_tables_html <- function(any_tab,
                                 header_cells,
                                 row_heights_header,
                                 row_header_names,
                                 value_names,
                                 stat_decimals,
                                 style,
                                 titles,
                                 footnotes,
                                 row_heights_title,
                                 row_heights,
                                 row_heights_mid,
                                 row_heights_footnote,
                                 row_heights_fn_spare,
                                 row_heights_table,
                                 row_heights_body,
                                 start_row,
                                 row_heights_lead,
                                 colgroup_html,
                                 table_width_html,
                                 sheet_width_html){
    thead_html <- build_table_head(header_cells, row_heights_header)

    # Build one table per by expression. In case multiple by variables are
    # used, every by variable is processed after the other with the values of
    # its own by_vars column, so the variable names and values stay aligned.
    sheets   <- list()
    data_pos <- 0

    for (by_var in unique(any_tab[["BY"]])){
        # Extract the unique values of the current by variable
        by_values <- any_tab[["by_vars"]][any_tab[["BY"]] == by_var]

        if (anyNA(by_values)){
            by_values <- c(collapse::funique(collapse::na_omit(by_values)), NA)
        }
        else{
            by_values <- collapse::funique(by_values)
        }

        for (by_value in by_values){
            # Filter the data frame by the current by expression
            if (is.na(by_value)){
                block <- any_tab |>
                    collapse::fsubset(any_tab[["BY"]] == by_var & is.na(any_tab[["by_vars"]]))
            }
            else{
                block <- any_tab |>
                    collapse::fsubset(any_tab[["BY"]] == by_var & any_tab[["by_vars"]] == by_value)
            }

            # Split up by block into the row header columns and the value columns
            block_row_header <- block |> collapse::fselect(row_header_names)
            block_values     <- block |> collapse::fselect(value_names)

            # Determine the heights of the current block
            if (!is.null(row_heights_table)){
                block_heights <- row_heights_table[(data_pos + 1):(data_pos + collapse::fnrow(block))]
            }
            else if (!is.null(row_heights)){
                block_heights <- row_heights_body[(data_pos + 1):(data_pos + collapse::fnrow(block))]
            }
            else{
                block_heights <- NULL
            }

            body_html <- build_table_rows(block_row_header, block_values, stat_decimals, style,
                                       block_heights)

            # Setup by info
            by_value <- ifelse(is.na(by_value), "NA", by_value)
            by_info  <- paste0(by_var, " = ", by_value)

            # Add by info below the titles
            if (length(titles) > 0){
                titles_temp <- c(titles, "", by_info)
                titles_temp <- gsub("\\[by_var\\]", by_value, titles_temp)
            }
            # Or on top if there are no titles
            else{
                titles_temp <- by_info
            }

            # Replace by info in the footnotes
            if (length(footnotes) > 0){
                footnotes_temp <- gsub("\\[by_var\\]", by_value, footnotes)
            }
            # Otherwise just leave footnotes empty
            else{
                footnotes_temp <- footnotes
            }

            # Build html code for the individual table parts
            titles_html    <- text_block(titles_temp, style, row_heights_title, row_heights, row_heights_mid)
            footnotes_html <- text_block(footnotes_temp, style, row_heights_footnote, row_heights, row_heights_fn_spare, "footnote")
            table_html     <- wrap_up_table_html(thead_html, body_html, colgroup_html, table_width_html)

            # Wrap all parts together to a full table block
            sheets[[length(sheets) + 1]] <- wrap_up_full_html(titles_html, table_html, footnotes_html, sheet_width_html)

            data_pos <- data_pos + collapse::fnrow(block)
        }
    }

    # Add blank rows between the individual tables
    sheets_html <- paste(sheets, collapse = "\n<div class=\"qol-spacer\"></div>\n")

    # Leading empty rows created by start_row
    if (start_row > 0){
        if (is.null(row_heights)){
            top_spacers_html <- paste(rep("<div class=\"qol-spacer\"></div>", start_row), collapse = "\n")
        }
        else{
            top_spacers_html <- paste(vapply(row_heights_lead, function(height){
                                                 paste0('<div class="qol-spacer" style="height: ', height, 'pt"></div>')
                                             }, character(1)), collapse = "\n")
        }
    }
    else{
        top_spacers_html <- ""
    }

    # Build html table render
    paste0("<!DOCTYPE html>
           <html lang=\"en\">
               <head>
                   <meta charset=\"utf-8\">
                   <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">
                   <title>qol table</title>
                   <style>
                       ", build_table_css(style), "
                   </style>
               </head>
               <body>
               ", top_spacers_html,
                    if (nzchar(top_spacers_html)) "\n",
                    sheets_html, "
               ", js_measure_table_width(), "
               </body>
           </html>")
}


#' @description
#' In case by variables were used, every by expression gets its own block
#' with a subheader. Without by variables the table is rendered as is.
#'
#' @param any_tab The data frame which contains the information for this table.
#' @param by Vector of by variable names.
#' @param by_vars The by variable columns of the table.
#' @param row_header_names Names of the row header columns.
#' @param value_names Names of the value columns.
#' @param stat_decimals The number of decimals per value column.
#' @param style A list containing the styling elements.
#' @param row_heights_subheader Heights of the by block subheaders.
#' @param row_heights Heights of the table rows.
#' @param row_heights_table Heights of the table rows per table.
#' @param row_heights_body Heights of the table rows.
#'
#' @return The html code for the table body as a single character string.
#'
#' @noRd
build_body_html <- function(any_tab,
                            by,
                            by_vars,
                            row_header_names,
                            value_names,
                            stat_decimals,
                            style,
                            row_heights_subheader,
                            row_heights,
                            row_heights_table,
                            row_heights_body){
    # In case by variables were used, every by expression gets its own block
    # with a subheader, mimicking the separate sheets of the excel output.
    if (length(by) > 0){
        length_by  <- length(collapse::funique(any_tab[["BY"]]))
        body_parts <- list()
        body_pos   <- 0
        data_pos   <- 0
        block_pos  <- 0

        for (by_var in unique(any_tab[["BY"]])){
            # Extract the unique values of the current by variable
            by_values <- any_tab[["by_vars"]][any_tab[["BY"]] == by_var]

            if (anyNA(by_values)){
                by_values <- c(collapse::funique(collapse::na_omit(by_values)), NA)
            }
            else{
                by_values <- collapse::funique(by_values)
            }

            for (by_value in by_values){
                # Filter the data frame by the current by expression
                if (is.na(by_value)){
                    block <- any_tab |>
                        collapse::fsubset(any_tab[["BY"]] == by_var & is.na(any_tab[["by_vars"]]))
                }
                else{
                    block <- any_tab |>
                        collapse::fsubset(any_tab[["BY"]] == by_var & any_tab[["by_vars"]] == by_value)
                }

                # Remove by variables from the row header
                block_row_header <- block |> collapse::fselect(setdiff(row_header_names, by_vars))
                block_values     <- block |> collapse::fselect(value_names)

                # Add the subheader banner for the current by expression. With a
                # single by variable the banner shows only the by expression value.
                sub_text <- ifelse(is.na(by_value), "NA", by_value)

                # Resolve the subheader and data row heights for this block
                if (!is.null(row_heights_subheader)){
                    sub_height <- row_heights_subheader[block_pos + 1]
                }
                else if (!is.null(row_heights)){
                    sub_height <- row_heights_body[body_pos + 1]
                }
                else{
                    sub_height <- NULL
                }

                if (!is.null(row_heights_table)){
                    block_heights <- row_heights_table[(data_pos + 1):(data_pos + collapse::fnrow(block))]
                }
                else if (!is.null(row_heights)){
                    block_heights <- row_heights_body[(body_pos + 2):(body_pos + 1 + collapse::fnrow(block))]
                }
                else{
                    block_heights <- NULL
                }

                # Build html code for the single table parts
                body_parts[[length(body_parts) + 1]] <- build_subheader_row(sub_text,
                                                                            length(names(block_row_header)),
                                                                            length(value_names),
                                                                            sub_height)
                body_parts[[length(body_parts) + 1]] <- build_table_rows(block_row_header,
                                                                        block_values,
                                                                        stat_decimals,
                                                                        style,
                                                                        block_heights)

                body_pos  <- body_pos + 1 + collapse::fnrow(block)
                data_pos  <- data_pos + collapse::fnrow(block)
                block_pos <- block_pos + 1
            }
        }

        # Put together the html code for the entire table
        paste(unlist(body_parts), collapse = "\n")
    }
    # Without by variables render the table as is
    else{
        row_header <- any_tab |> collapse::fselect(row_header_names)
        values     <- any_tab |> collapse::fselect(value_names)

        build_table_rows(row_header, values, stat_decimals, style,
                        heights = if (!is.null(row_heights_table)) row_heights_table
                                  else if (!is.null(row_heights)) row_heights_body
                                  else NULL)
    }
}


###############################################################################
# Html helpers
###############################################################################
#' @description
#' Get specific table part heights.
#'
#' @param part The part of the table for which individual heights should be checked.
#' @param part_count Number of parts to receive heights.
#' @param default_heights Default heights if no specific table part heights are specified.
#'
#' @noRd
get_part_heights <- function(specific_heights,
                             part_count,
                             default_heights){
    if (is.null(specific_heights)){
        default_heights
    }
    else{
        as.numeric(fill_or_trim(specific_heights, part_count))
    }
}


#' @description
#' Escape html special characters.
#'
#' @param text_to_process A character vector to escape.
#'
#' @noRd
replace_html_escape <- function(text_to_process){
    text_to_process <- as.character(text_to_process)
    text_to_process[is.na(text_to_process)] <- "NA"
    text_to_process <- gsub("&", "&amp;",  text_to_process, fixed = TRUE)
    text_to_process <- gsub("<", "&lt;",   text_to_process, fixed = TRUE)
    text_to_process <- gsub(">", "&gt;",   text_to_process, fixed = TRUE)
    text_to_process <- gsub('"', "&quot;", text_to_process, fixed = TRUE)
    text_to_process
}


#' @description
#' Convert hex color to css color.
#'
#' @param color The hex color code.
#'
#' @noRd
get_css_color <- function(color){
    if (is.null(color) || length(color) == 0){
        return("transparent")
    }

    color <- as.character(color[1])

    if (is.na(color) || color == ""){
        return("transparent")
    }

    color <- sub("^#", "", color)

    if (!grepl("^[0-9A-Fa-f]{6}$", color)){
        return("transparent")
    }

    paste0("#", color)
}


###############################################################################
# Build header formatting list
###############################################################################
#' @description
#' Identifies all cells of the multi layered column header which need to be
#' merged. Repeated header texts are merged horizontally and empty cells below
#' them vertically.
#'
#' @param column_header The full column header.
#' @param cat_col_width The number of row header columns.
#' @param header_stat_merging How the last header row should be merged. Either
#' "block", "none" or any other value for the default rle merging.
#' @param block_lengths Lengths of the statistic blocks.
#' @param block_values Values of the statistic blocks.
#'
#' @return
#' Returns a list with one entry per header row. Each entry is a list of cell
#' specifications with the elements value, rowspan, colspan and cls.
#'
#' @noRd
get_column_header_cell_tree <- function(column_header,
                                        cat_col_width,
                                        header_stat_merging = "block",
                                        block_lengths       = NULL,
                                        block_values        = NULL){
    # Work on a plain character matrix so every comparison below behaves consistently
    header_matrix <- as.matrix(column_header)
    storage.mode(header_matrix) <- "character"

    number_of_rows    <- nrow(header_matrix)
    number_of_columns <- ncol(header_matrix)

    # Tracks which positions have already been used by a cell that merged over them,
    # so later cells know not to draw over the same spot again.
    covered_matrix <- matrix(FALSE, number_of_rows, number_of_columns)

    # One element per header row, each holding that row's list of cells
    rows_output <- vector("list", number_of_rows)

    # Construct list of column header formats
    for (row in seq_len(number_of_rows)){
        cells <- list()

        # The very first row gets a single "box" cell in the top-left corner that
        # spans the full category-column width.
        if (row == 1){
            cells[[1]] <- new_cell(header_matrix[1, 1],
                                   rowspan = number_of_rows,
                                   colspan = cat_col_width,
                                   cls     = "box")

            covered_matrix[, seq_len(cat_col_width)] <- TRUE
        }

        # Everything from here on starts right after the category columns.
        column <- cat_col_width + 1

        # Special case: on the last header row, if the caller wants statistic columns
        # grouped into labelled blocks (e.g. one "Summary" label spanning "min/max/mean"),
        # build those block cells directly instead of falling through to the general
        # merge logic below.
        if (row == number_of_rows && header_stat_merging == "block" && !is.null(block_lengths)){
            for (block in seq_along(block_lengths)){
                block_end_column <- column + block_lengths[block] - 1

                # Only add the block label, if at least one column in the block hasn't
                # already been covered by something else.
                if (!all(covered_matrix[row, column:block_end_column])){
                    cells[[length(cells) + 1]] <- new_cell(block_values[block],
                                                           colspan = block_lengths[block])

                    covered_matrix[row, column:block_end_column] <- TRUE
                }

                column <- block_end_column + 1
            }

            rows_output[[row]] <- cells

            # Skip the general merge logic for this row
            next
        }

        # General case: walk across the row left to right, merging equal neighbouring
        # labels horizontally and, where possible, merging further down into rows
        # that are blank underneath.
        while (column <= number_of_columns){
            # This spot was already claimed by an earlier wider cell.
            if (covered_matrix[row, column]){
                column <- column + 1
                next
            }

            value <- header_matrix[row, column]

            # An explicitly empty label just becomes its own empty cell, nothing
            # to merge horizontally or vertically here.
            if (value == ""){
                cells[[length(cells) + 1]]  <- new_cell("")
                covered_matrix[row, column] <- TRUE

                column <- column + 1

                next
            }

            # Horizontal merge: keep extending the span while the next column has
            # the exact same label and isn't already covered.
            column_span <- 1

            while (column + column_span <= number_of_columns &&
                   !covered_matrix[row, column + column_span] &&
                   header_matrix[row, column + column_span] == value){
                column_span <- column_span + 1
            }

            # Vertical merge: if every cell directly beneath this merged block is
            # blank, fold those rows into this cell too, so the label only needs
            # to be drawn once instead of repeated with blank rows underneath it.
            row_span <- 1

            if (row < number_of_rows){
                block_column_range <- column:(column + column_span - 1)

                for (check_row in (row + 1):number_of_rows){
                    # Merge blank rows
                    if (all(header_matrix[check_row, block_column_range] == "")){
                        row_span <- row_span + 1
                    }
                    # First non-blank row below stops the vertical merge
                    else{
                        break
                    }
                }
            }

            cells[[length(cells) + 1]] <- new_cell(value, rowspan = row_span, colspan = column_span)

            # Mark the whole rectangle this cell now covers so nothing else tries
            # to draw into it.
            covered_matrix[row:(row + row_span - 1), column:(column + column_span - 1)] <- TRUE

            column <- column + column_span
        }

        rows_output[[row]] <- cells
    }

    rows_output
}


#' @description
#' Small constructor so every cell has the same fields, instead of building
#' the list literal by hand everywhere.
#'
#' @return
#' Returns a list with the formatting value for a column header cell.
#'
#' @noRd
new_cell <- function(value,
                     rowspan = 1,
                     colspan = 1,
                     cls     = "header"){
    list(value = value, rowspan = rowspan, colspan = colspan, cls = cls)
}


###############################################################################
# Build html code based on formatting list
###############################################################################
#' @description
#' Converts the list-of-cells structure into the actual <thead> html.
#'
#' @param header_cells The list of header cell formats.
#' @param row_heights_header Heights of each row of the column header.
#'
#' @return
#' Returns a full html table header string.
#'
#' @noRd
build_table_head <- function(header_cells,
                             row_heights_header = NULL){
    # If no per-row heights were supplied, fall back to NA for every row so
    # render_row() simply omits the style attribute.
    if (is.null(row_heights_header)){
        row_heights <- rep(NA_real_, length(header_cells))
    }
    else{
        row_heights <- row_heights_header
    }

    paste(vapply(seq_along(header_cells), function(row_index){
                    render_row(header_cells[[row_index]], row_heights[row_index])
                }, character(1)),
          collapse = "\n")
}


#' @description
#' Builds the ' colspan="n" rowspan="n"' attribute string for a single cell.
#'
#' @param cell The list of attributes of a single table header cell.
#'
#' @return
#' Returns a full html table header string.
#'
#' @noRd
render_attrs <- function(cell){
    # Only include whichever of the two is actually needed
    attribute_parts <- c(if (cell[["colspan"]] > 1) sprintf('colspan="%d"', cell[["colspan"]]),
                         if (cell[["rowspan"]] > 1) sprintf('rowspan="%d"', cell[["rowspan"]]))

    # Return nothing if no attributes where set
    if (length(attribute_parts) == 0){
        ""
    }
    # Insert a leading space so it can be inserted directly after "<th" without
    # worrying about extra/missing whitespace.
    else{
        paste0(" ", paste(attribute_parts, collapse = " "))
    }
}


#' @description
#' Renders one <th> tag for one cell, escaping the value so any special html
#' characters in the label don't break the markup.
#'
#' @return
#' Returns a formatted cell.
#'
#' @noRd
render_cell <- function(cell){
    sprintf('<th%s class="%s">%s</th>',  render_attrs(cell), cell[["cls"]],
            replace_html_escape(cell[["value"]]))
}


#' @description
#' Renders one full <tr> from that row's list of cells.
#'
#' @param cells The list of attributes of a single table header row with all
#' it's cells.
#'
#' @return
#' Returns a formatted table row.
#'
#' @noRd
render_row <- function(cells, height){
    if (is.na(height)){
        row_style <- ""
    }
    else{
        row_style <- sprintf(' style="height:%spt"', height)
    }

    paste0("<tr", row_style, ">",
           paste(vapply(cells, render_cell, character(1)), collapse = ""),
           "</tr>")
}


###############################################################################
# Table formatting
###############################################################################
#' @description
#' Identifies all cells of the multi layered row header which need to be merged
#' vertically. Repeated row header texts are merged with a row span and expand
#' to the right over empty row header cells.
#'
#' @param row_header The row header as a matrix or data frame.
#'
#' @return
#' Returns a list with one entry per table row. Each entry is a list of cell
#' specifications with the elements value, rowspan and colspan.
#'
#' @noRd
get_row_header_cell_tree <- function(row_header){
    # Work on a plain character matrix so every comparison below behaves consistently
    header_matrix <- as.matrix(row_header)
    storage.mode(header_matrix) <- "character"

    number_of_rows    <- nrow(header_matrix)
    number_of_columns <- ncol(header_matrix)

    # Grid which stores the merge start cells and which cells are covered
    specs   <- matrix(rep(list(NULL), number_of_rows * number_of_columns),
                      nrow = number_of_rows,
                      ncol = number_of_columns)
    covered <- matrix(FALSE, number_of_rows, number_of_columns)

    # Get all values in order of appearance with their respective lengths per column
    column_texts <- lapply(seq_len(number_of_columns), function(column){
                            rle(as.character(header_matrix[, column]))
                           })

    # Identify the merge start cells per column
    for (column in seq_len(number_of_columns)){
        texts     <- column_texts[[column]]
        start_row <- 1

        for (i in seq_along(texts[["values"]])){
            values <- texts[["values"]][i]
            space  <- texts[["lengths"]][i]

            # Only merge non empty values
            if (values != ""){
                from_row  <- start_row
                to_row    <- start_row + space - 1
                to_column <- column

                # Expand right, if upcoming columns are empty in the same block
                if (column < number_of_columns){
                    for (next_column in seq.int(column + 1, number_of_columns)){
                        if (all(as.character(header_matrix[from_row:to_row, next_column]) == "")){
                            to_column <- next_column
                        }
                        else{
                            break
                        }
                    }
                }

                # Gather merging information
                specs[[start_row, column]] <- list(value    = values,
                                                   rowspan  = to_row - from_row + 1,
                                                   colspan  = to_column - column + 1)

                # Set flag for already covered cells
                covered[start_row:to_row, column:to_column] <- TRUE
            }

            # Directly jump over all equal rows
            start_row <- start_row + space
        }
    }

    # Put the cells together row by row, skipping covered cells
    rows_out <- vector("list", number_of_rows)

    for (row in seq_len(number_of_rows)){
        cells <- list()

        for (column in seq_len(number_of_columns)){
            if (!is.null(specs[[row, column]])){
                cells[[length(cells) + 1]] <- specs[[row, column]]
            }
            else if (!covered[row, column]){
                cells[[length(cells) + 1]] <- list(value = "", rowspan = 1, colspan = 1)
            }
        }

        rows_out[[row]] <- cells
    }

    rows_out
}


#' @description
#' Format values for the html output. The thousand separator is always a dot and
#' the decimal separator is always a comma, independent of the excel formatting.
#'
#' @param value A single value to format.
#' @param decimals The number of decimals to display.
#'
#' @noRd
html_format_number <- function(value, decimals){
    if (is.na(value)){
        return(NA_character_)
    }

    if (!is.numeric(value)){
        return(as.character(value))
    }

    format(round(value, decimals),
           big.mark     = ".",
           decimal.mark = ",",
           nsmall       = decimals,
           scientific   = FALSE,
           trim         = TRUE)
}


###############################################################################
# Heatmap
###############################################################################
#' @description
#' Compute the heatmap color for a value
#'
#' @param value The value to colorize.
#' @param min_value The minimum value of the color scale.
#' @param max_value The maximum value of the color scale.
#' @param low_color The color for the lowest values.
#' @param middle_color The color for the middle values.
#' @param high_color The color for the highest values.
#'
#' @noRd
html_heatmap_color <- function(value,
                               min_value,
                               max_value,
                               low_color,
                               middle_color,
                               high_color){
    if (is.na(value)){
        return(NULL)
    }

    # Determine transparency
    transparency <- (value - min_value) / (max_value - min_value)

    if (transparency <= 0.5){
        mix_color(low_color, middle_color, transparency / 0.5)
    }
    else{
        mix_color(middle_color, high_color, (transparency - 0.5) / 0.5)
    }
}


#' @description
#' Interpolate between two colors
#'
#' @param color1 The first hex color.
#' @param color2 The second hex color.
#' @param transparency The interpolation position between 0 and 1.
#'
#' @noRd
mix_color <- function(color1, color2, transparency){
    rgb1 <- as.vector(grDevices::col2rgb(get_css_color(color1)))
    rgb2 <- as.vector(grDevices::col2rgb(get_css_color(color2)))

    mixed <- rgb1 + (rgb2 - rgb1) * transparency

    sprintf("#%02X%02X%02X", round(mixed[1]), round(mixed[2]), round(mixed[3]))
}


###############################################################################
# Build html table
###############################################################################
#' @description
#' Build the html rows of the table body.
#'
#' @param row_header The row header data frame.
#' @param values The value data frame.
#' @param stat_decimals The number of decimals per value column.
#' @param style A list containing the styling elements.
#' @param heights User defined row heights.
#'
#' @noRd
build_table_rows <- function(row_header,
                             values,
                             stat_decimals,
                             style,
                             heights = NULL){
    row_cells         <- get_row_header_cell_tree(as.matrix(row_header))
    number_of_rows    <- nrow(values)
    number_of_columns <- ncol(values)

    # Get the range of all values for the heatmap coloring
    heatmap   <- style[["as_heatmap"]]
    min_value <- NULL
    max_value <- NULL

    # If heatmap should be applied, then get the extreme values between which the
    # color interpolation will happen.
    if (heatmap){
        all_values <- unlist(lapply(values, as.numeric))
        min_value  <- min(all_values, na.rm = TRUE)
        max_value  <- max(all_values, na.rm = TRUE)
    }

    # Translate the html tree into actual html code
    rows <- character(number_of_rows)

    for (row in seq_len(number_of_rows)){
        # Row header cells
        row_heights_html <- vapply(row_cells[[row]], function(cell){
            attrs <- ""

            if (cell[["colspan"]] > 1){
                attrs <- paste0(attrs, ' colspan="', cell[["colspan"]], '"')
            }
            if (cell[["rowspan"]] > 1){
                attrs <- paste0(attrs, ' rowspan="', cell[["rowspan"]], '"')
            }

            paste0('<th', attrs, ' class="cat">', replace_html_escape(cell[["value"]]), '</th>')
        }, character(1))

        # Value cells
        values_html <- character(number_of_columns)

        for (column in seq_len(number_of_columns)){
            value <- values[[column]][row]

            # Format numbers
            if (is.na(value)){
                text <- style[["na_symbol"]]
            }
            else{
                text <- html_format_number(value, stat_decimals[column])
            }

            # Apply heatmap
            color_style <- ""

            if (heatmap && !is.na(value)){
                bg_color <- html_heatmap_color(as.numeric(value),
                                               min_value,
                                               max_value,
                                               style[["heatmap_low_color"]],
                                               style[["heatmap_middle_color"]],
                                               style[["heatmap_high_color"]])

                if (!is.null(bg_color)){
                    color_style <- paste0(' style="background-color: ', bg_color, ';"')
                }
            }

            # Complete the cell formatting
            values_html[column] <- paste0('<td class="data"', color_style, '>',
                                          replace_html_escape(text), '</td>')
        }

        if (is.null(heights)){
            row_height <- ""
        }
        else{
            row_height <- paste0(' style="height: ', heights[row], 'pt"')
        }

        # Put together the entire row containing all previously gathered cells
        rows[row] <- paste0("<tr", row_height, ">", paste(c(row_heights_html, values_html), collapse = ""), "</tr>")
    }

    # Stack rows to get the entire table body
    paste(rows, collapse = "\n")
}


#' @description
#' Build a subheader row in case of by variables being displayed as subheaders in
#' one table.
#'
#' @param text The subheader text.
#' @param row_header_count The number of row header columns.
#' @param value_count The number of value columns.
#' @param height User provided subheader row height.
#'
#' @noRd
build_subheader_row <- function(text,
                                row_header_count,
                                value_count,
                                height = NULL){
    if (is.null(height)){
        height_style <- ""
    }
    else{
        height_style <- paste0(' style="height: ', height, 'pt"')
    }

    paste0("<tr", height_style, ">",
           '<td class="subheader" colspan="', row_header_count, '"></td>',
           '<td class="subheader" colspan="', value_count, '">',
           replace_html_escape(text), '</td>',
           "</tr>")
}


#' @description
#' Wrap up the html table code.
#'
#' @param thead_html The header html of the table.
#' @param body_html The body html of the table.
#' @param colgroup_html The column group html of the table.
#' @param table_width_html The style attribute containing the table width.
#'
#' @noRd
wrap_up_table_html <- function(thead_html,
                               body_html,
                               colgroup_html,
                               table_width_html){
    paste0('<table class="qol-table"', table_width_html, '>',
           "\n", colgroup_html, "\n", thead_html, "\n<tbody>\n",
           body_html, "\n</tbody>\n</table>")
}


#' @description
#' Build the full html containing titles, footnotes and the whole table body.
#'
#' @param titles_html The html of the titles.
#' @param table_html The html of the table.
#' @param footnotes_html The html of the footnotes.
#' @param sheet_width_html The style attribute containing the sheet margin.
#'
#' @noRd
wrap_up_full_html <- function(titles_html,
                              table_html,
                              footnotes_html,
                              sheet_width_html){
    paste0('<div class="qol-sheet"', sheet_width_html, '>',
           "\n", titles_html,
           "\n <div class=\"qol-wrap\">\n", table_html, "\n</div>\n",
           footnotes_html,
           "\n</div>")
}


###############################################################################
# Build html titles and footnotes
###############################################################################
#' @description
#' Build the html for a whole block of titles or footnotes.
#'
#' @param texts The texts to render.
#' @param style A list containing the styling elements.
#' @param row_heights_text The resolved footnote heights, NULL for auto heights.
#' @param row_heights The global row heights, NULL for auto heights.
#' @param row_heights_spare The height of the blank row between the table and the footnotes.
#' @param type Titles or footnotes
#'
#' @noRd
text_block <- function(texts,
                       style,
                       row_heights_text  = NULL,
                       row_heights       = NULL,
                       row_heights_spare = NULL,
                       type              = "title"){
    if (length(texts) == 0){
        return("")
    }

    # Format all texts and collect the parts
    text_parts <- vapply(seq_along(texts), function(i){
        height <- NULL

        if (!is.null(row_heights_text) && i <= length(row_heights_text)){
            height <- row_heights_text[i]
        }

        # The first footnotes gets a special treatment, since it gets a line
        # drawn above.
        if (i == 1 && type == "footnote"){
            cls <- paste0("qol-", type, "s qol-", type, "s-first")
        }
        else{
            cls <- paste0("qol-", type, "s")
        }

        build_title_html(texts[i], style, cls = cls, type = type, i = i, height = height)
    }, character(1))

    # Empty line for separation between texts and table
    if (is.null(row_heights)){
        spacer_html <- "<div class=\"qol-spacer\"></div>"
    }
    else{
        spacer_html <- paste0('<div class="qol-spacer" style="height: ', row_heights_spare, 'pt"></div>')
    }

    # Put together the whole title or footnote block
    if (type == "title"){
        paste0(paste(text_parts, collapse = "\n"), "\n", spacer_html)
    }
    else{
        paste0(spacer_html, "\n", paste(text_parts, collapse = "\n"))
    }
}


#' @description
#' Build the html code for a single title or footnote.
#'
#' @param text The title or footnote text.
#' @param style A list containing the styling elements.
#' @param cls The css class to use.
#' @param type The part of the table, either "title" or "footnote".
#' @param i The index of the title or footnote within the vector.
#' @param height The height in points, NULL for auto height.
#'
#' @noRd
build_title_html <- function(text,
                             style,
                             cls    = "qol-titles",
                             type   = "title",
                             i      = 1,
                             height = NULL){
    styles <- c()

    if (!is.null(height)){
        styles <- c(styles, paste0("height: ", height, "pt"))
    }

    # Font attributes can differ per title or footnote, so that individual fonts
    # can be applied to every title and footnote.
    font_size  <- style[[paste0(type, "_font_size")]][i]
    font_color <- style[[paste0(type, "_font_color")]][i]
    font_bold  <- style[[paste0(type, "_font_bold")]][i]
    alignment  <- style[[paste0(type, "_alignment")]][i]

    if (!is.null(font_size) && !is.na(font_size)){
        styles <- c(styles, paste0("font-size: ", font_size, "pt"))
    }
    if (!is.null(font_color) && !is.na(font_color)){
        styles <- c(styles, paste0("color: ", get_css_color(font_color)))
    }
    if (!is.null(font_bold) && !is.na(font_bold)){
        styles <- c(styles, paste0("font-weight: ", if (isTRUE(font_bold)) "bold" else "normal"))
    }
    if (!is.null(alignment) && !is.na(alignment)){
        styles <- c(styles, paste0("text-align: ", alignment))
    }

    if (length(styles) > 0){
        style_attr <- paste0(' style="', paste(styles, collapse = "; "), '"')
    }
    else{
        style_attr <- ""
    }

    # Convert the special hyperlink keywords into links. The 'cell:' keyword
    # links to excel cells and sheets which do not exist in html, so it is
    # rendered as a pseudo hyperlink that leads nowhere when clicked.
    if (grepl("(link|cell|file):", text)){
        pattern      <- "(link|cell|file):"
        matched      <- regexec(pattern, text)
        keyword      <- regmatches(text, matched)[[1]][2]
        current_link <- trimws(sub(paste0(".*", pattern), "", text))
        text         <- trimws(sub(paste0("\\s*", pattern, ".*"), "", text))

        # Create pseudo link for "cell" keyword to have at least a visual
        # representation of a formatted link.
        if (keyword == "cell"){
            href    <- "#"
            target  <- ""
            onclick <- ' onclick="return false;"'
        }
        # Otherwise for hyperlinks and files build a real link
        else{
            target  <- ' target="_blank"'
            onclick <- ""

            if (keyword == "link"){
                href <- replace_html_escape(current_link)
            }
            else{
                file_url <- gsub("\\\\", "/", current_link)

                if (grepl("^//", file_url)){
                    href <- replace_html_escape(paste0("file:", file_url))
                }
                else{
                    href <- replace_html_escape(paste0("file:///", file_url))
                }
            }
        }

        # Wrap up text
        paste0('<div class="', cls, '"', style_attr, '><a href="', href, '"',
               target, onclick, '>', replace_html_escape(text), '</a></div>')
    }
    # Just wrap up text without hyperlinks
    else{
        paste0('<div class="', cls, '"', style_attr, '>', replace_html_escape(text), '</div>')
    }
}


###############################################################################
# Build the JavaScript that measures the table width for Ttitles and footnotes
###############################################################################
#' @description
#' The titles and footnotes are aligned relative to the table width and not to
#' the page width. As the rendered table width is only known after the browser
#' laid out the table, a script measures every table and applies the measured
#' width to the titles and footnotes of the same sheet. It runs while the
#' document is parsed, so the widths are in place before the first display.
#' Longer titles and footnotes then wrap at the table width.
#'
#' @noRd
js_measure_table_width <- function(){
    paste0("\n<script>\n(function(){\n",
           "    var sheets = document.querySelectorAll(\".qol-sheet\");\n",
           "    for (var s = 0; s < sheets.length; s++){\n",
           "        var table = sheets[s].querySelector(\"table.qol-table\");\n",
           "        if (!table) continue;\n",
           "        var width = table.offsetWidth;\n",
           "        var parts = sheets[s].querySelectorAll(\".qol-titles, .qol-footnotes\");\n",
           "        for (var i = 0; i < parts.length; i++){\n",
           "            parts[i].style.width = width + \"px\";\n",
           "        }\n",
           "        var fn = sheets[s].querySelector(\".qol-footnotes-first\");\n",
           "        if (fn){\n",
           "            var catWidth = 0;\n",
           "            var rows = table.querySelectorAll(\"tbody tr\");\n",
           "            for (var r = 0; r < rows.length; r++){\n",
           "                var cells = rows[r].querySelectorAll(\"th.cat\");\n",
           "                if (cells.length > 0){\n",
           "                    for (var c = 0; c < cells.length; c++){\n",
           "                        catWidth += cells[c].offsetWidth;\n",
           "                    }\n",
           "                    break;\n",
           "                }\n",
           "            }\n",
           "            fn.style.setProperty(\"--qol-fn-line\", catWidth + \"px\");\n",
           "        }\n",
           "    }\n})();\n</script>")
}


#' @description
#' Build the css styling for the html table.
#'
#' @param style A list containing the styling elements.
#'
#' @noRd
build_table_css <- function(style){
    font <- style[["font"]]

    # Setup border styles
    if (style[["header_borders"]]){
        header_border <- paste0("1px solid ", get_css_color(style[["header_border_color"]]))
    }
    else{
        header_border <- "none"
    }

    if (style[["box_borders"]]){
        box_border <- paste0("1px solid ", get_css_color(style[["box_border_color"]]))
    }
    else{
        box_border <- "none"
    }

    if (style[["cat_col_borders"]]){
        cat_border <- paste0("1px solid ", get_css_color(style[["cat_col_border_color"]]))
    }
    else{
        cat_border <- "none"
    }

    if (style[["table_borders"]]){
        table_border <- paste0("1px solid ", get_css_color(style[["table_border_color"]]))
    }
    else{
        table_border <- "none"
    }

    if (style[["subheader_borders"]]){
        subheader_border <- paste0("1px solid ", get_css_color(style[["subheader_border_color"]]))
    }
    else{
        subheader_border <- "none"
    }

    # Setup text wrapping
    if (style[["header_wrap"]] == "1"){
        header_wrap <- "normal"
    }
    else{
        header_wrap <- "nowrap"
    }

    if (style[["cat_col_wrap"]] == "1"){
        cat_wrap <- "normal"
    }
    else{
        cat_wrap <- "nowrap"
    }

    if (style[["box_wrap"]] == "1"){
        box_wrap <- "normal"
    }
    else{
        box_wrap <- "nowrap"
    }

    if (style[["subheader_wrap"]] == "1"){
        subheader_wrap <- "normal"
    }
    else{
        subheader_wrap <- "nowrap"
    }

    # Cell indentation is mimicked with padding on the left side. Excel indents
    # a cell by a multiple of a character width, so the em unit is used which
    # scales with the font size of the cell.
    header_indent_css    <- indent_css(style[["header_indent"]])
    subheader_indent_css <- indent_css(style[["subheader_indent"]])
    cat_col_indent_css   <- indent_css(style[["cat_col_indent"]])
    table_indent_css     <- indent_css(style[["table_indent"]])
    box_indent_css       <- indent_css(style[["box_indent"]])

    # Custom column widths are only honored by the browser when the table uses
    # a fixed layout. Auto layout lets the browser autofit the columns instead.
    if (is.null(style[["column_widths"]]) ||
        (is.character(style[["column_widths"]]) && style[["column_widths"]] == "auto")){
        table_layout <- ""
    }
    else{
        table_layout <- " table-layout: fixed;"
    }

    # Setup displaying texts as bold
    if (style[["header_font_bold"]]){
        header_bold <- "bold"
    }
    else{
        header_bold <- "normal"
    }

    if (style[["box_font_bold"]]){
        box_bold <- "bold"
    }
    else{
        box_bold <- "normal"
    }

    if (style[["cat_col_font_bold"]]){
        cat_bold <- "bold"
    }
    else{
        cat_bold <- "normal"
    }

    if (style[["table_font_bold"]]){
        table_bold <- "bold"
    }
    else{
        table_bold <- "normal"
    }

    if (style[["subheader_font_bold"]]){
        subheader_bold <- "bold"
    }
    else{
        subheader_bold <- "normal"
    }

    if (style[["title_font_bold"]][1]){
        title_bold <- "bold"
    }
    else{
        title_bold <- "normal"
    }

    if (style[["footnote_font_bold"]][1]){
        footnote_bold <- "bold"
    }
    else{
        footnote_bold <- "normal"
    }

    # Get font sizes
    title_font_size     <- style[["title_font_size"]][1]
    title_alignment     <- style[["title_alignment"]][1]
    footnote_font_size  <- style[["footnote_font_size"]][1]
    footnote_alignment  <- style[["footnote_alignment"]][1]
    header_font_size    <- style[["header_font_size"]][1]
    header_alignment    <- style[["header_alignment"]][1]
    box_font_size       <- style[["box_font_size"]][1]
    box_alignment       <- style[["box_alignment"]][1]
    cat_col_font_size   <- style[["cat_col_font_size"]][1]
    cat_col_alignment   <- style[["cat_col_alignment"]][1]
    table_font_size     <- style[["table_font_size"]][1]
    table_alignment     <- style[["table_alignment"]][1]
    subheader_font_size <- style[["subheader_font_size"]][1]
    subheader_alignment <- style[["subheader_alignment"]][1]

    fn_line_border <- paste0("1px solid ", get_css_color(style[["cat_col_border_color"]]))

    # Build css styling
    paste0(
"body { margin: 0; padding: 10px; background: ", get_css_color(style[["background_color"]]), "; }
.qol-titles { padding: 2px 0; color: ", get_css_color(style[["title_font_color"]]),
        "; font-family: '", font, "', sans-serif; font-size: ", title_font_size,
        "pt; font-weight: ", title_bold, "; text-align: ", title_alignment,
        "; overflow-wrap: break-word; }
.qol-titles a { color: #0000FF; text-decoration: underline; }
.qol-footnotes { position: relative; padding: 2px 0; color: ", get_css_color(style[["footnote_font_color"]]),
        "; font-family: '", font, "', sans-serif; font-size: ", footnote_font_size,
        "pt; font-weight: ", footnote_bold, "; text-align: ", footnote_alignment,
        "; overflow-wrap: break-word; }
.qol-footnotes a { color: #0000FF; text-decoration: underline; }
.qol-footnotes-first::before { content: \"\"; position: absolute; top: 0; left: 0; height: 0; width: var(--qol-fn-line, 0px); border-top: ", fn_line_border, "; }
.qol-spacer { height: 1.4em; font-size: ", header_font_size, "pt; }
.qol-wrap { overflow: auto; }
.qol-sheet { font-family: '", font, "', sans-serif; font-size: ", table_font_size, "pt; }
table.qol-table { border-collapse: collapse; font-family: '", font, "', sans-serif;", table_layout, " }
table.qol-table th, table.qol-table td { padding: 2px 8px; white-space: nowrap; }
table.qol-table th.header { background: ", get_css_color(style[["header_back_color"]]),
        "; color: ", get_css_color(style[["header_font_color"]]),
        "; font-size: ", header_font_size, "pt; font-weight: ", header_bold,
        "; text-align: ", header_alignment, "; border: ", header_border,
        "; white-space: ", header_wrap, ";", header_indent_css, " }
table.qol-table th.box { background: ", get_css_color(style[["box_back_color"]]),
        "; color: ", get_css_color(style[["box_font_color"]]),
        "; font-size: ", box_font_size, "pt; font-weight: ", box_bold,
        "; text-align: ", box_alignment, "; border: ", box_border,
        "; white-space: ", box_wrap, ";", box_indent_css, " }
table.qol-table th.cat { background: ", get_css_color(style[["cat_col_back_color"]]),
        "; color: ", get_css_color(style[["cat_col_font_color"]]),
        "; font-size: ", cat_col_font_size, "pt; font-weight: ", cat_bold,
        "; text-align: ", cat_col_alignment, "; border: ", cat_border,
        "; white-space: ", cat_wrap, ";", cat_col_indent_css, " }
table.qol-table td.data { background: ", get_css_color(style[["table_back_color"]]),
        "; color: ", get_css_color(style[["table_font_color"]]),
        "; font-size: ", table_font_size, "pt; font-weight: ", table_bold,
        "; text-align: ", table_alignment, "; border: ", table_border,
        ";", table_indent_css, " }
table.qol-table td.subheader { background: ", get_css_color(style[["subheader_back_color"]]),
        "; color: ", get_css_color(style[["subheader_font_color"]]),
        "; font-size: ", subheader_font_size, "pt; font-weight: ", subheader_bold,
        "; text-align: ", subheader_alignment, "; border: ", subheader_border,
        "; white-space: ", subheader_wrap, ";", subheader_indent_css, " }
table.qol-table th.box { border-left: none; }
table.qol-table th.cat { border-left: none; border-bottom: none; }
table.qol-table th.header { border-right: none; }
table.qol-table td.data { border-right: none; border-bottom: none; }
table.qol-table td.subheader { border-left: none; border-right: none; }")
}


#' @description
#' Css indentation style.
#'
#' @param table_part Part of the table to be indented.
#'
#' @noRd
indent_css <- function(table_part){
    if (table_part[1] <= 0){
        return("")
    }

    paste0(" padding-left: ", round(0.6 * table_part[1], 2), "em;")
}
