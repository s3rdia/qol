#' Display Cross Table of Two Variables
#'
#' @description
#' [crosstabs()] produces a cross table of two variables. Statistics can be
#' weighted sums, unweighted frequencies or different percentages.
#'
#' @param data_frame A data frame in which are the variables to tabulate.
#' @param rows The variable that appears in the table rows.
#' @param columns The variable that appears in the table columns.
#' @param statistics The user requested statistics.Available functions:
#' - "sum"        -> Weighted and unweighted sum
#' - "freq"       -> Unweighted frequency
#' - "pct_row"    -> Weighted and unweighted row percentages
#' - "pct_column" -> Weighted and unweighted column percentages
#' - "pct_total"  -> Weighted and unweighted percentages compared to the grand total
#' @param formats A list in which is specified which formats should be applied to which variables.
#' @param by Compute tables stratified by the expressions of the provided variables.
#' @param weight Put in a weight variable to compute weighted results.
#' @param titles Specify one or more table titles.
#' @param footnotes Specify one or more table footnotes.
#' @param style A list of options can be passed to control the appearance of excel outputs.
#' Styles can be created with [excel_output_style()].
#' @param output The following output formats are available: console (default), text,
#' excel and excel_nostyle.
#' @param na.rm FALSE by default. If TRUE removes all NA values from the variables.
#' @param print TRUE by default. If TRUE prints the output, if FALSE doesn't print anything. Can be used
#' if one only wants to catch the output data frame.
#' @param monitor FALSE by default. If TRUE outputs two charts to visualize the functions time consumption.
#'
#' @details
#' [crosstabs()] is based on the 'SAS' procedure Proc Freq, which provides
#' efficient and readable ways to perform cross tabulations.
#'
#' To create a cross table you only need to provide a variable for the rows and columns.
#' Nothing special about this. The real power comes into play, when you output your
#' tables as a fully styled 'Excel' workbook. Setting up a custom, reusable style is
#' as easy as setting up options like: provide a color for the table header, set the
#' font size for the row header, should borders be drawn for the table cells yes/no,
#' and so on.
#'
#' You can not only output sums and frequencies, but also different percentages, all
#' set up in separate, evenly designed tables. For just a quick overview, rather than
#' fully designed tables, you can also just output the tables in ASCII style format.
#'
#' @return
#' Returns a data tables containing the results for the cross table.
#'
#' @seealso
#' Creating a custom table style: [excel_output_style()], [modify_output_style()],
#' [number_format_style()], [modify_number_formats()].
#'
#' Creating formats: [discrete_format()] and [interval_format()].
#'
#' Functions that can handle formats and styles: [frequencies()], [any_table()].
#'
#' Additional functions that can handle styles: [export_with_style()]
#'
#' Additional functions that can handle formats: [summarise_plus()], [recode()],
#' [recode_multi()]
#'
#' @examples
#' # Example data frame
#' my_data <- dummy_data(1000)
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
#' # Output cross tables
#' my_data |> crosstabs(age, sex)
#' my_data |> crosstabs(age, sex,
#'                      weight = "weight")
#'
#' # Also works with characters
#' my_data |> crosstabs("age", "sex")
#' my_data |> crosstabs("age", "sex",
#'                      weight = "weight")
#'
#' # Applying formats and titles
#' age. <- discrete_format(
#'     "Total"          = 0:100,
#'     "under 18"       = 0:17,
#'     "18 to under 25" = 18:24,
#'     "25 to under 55" = 25:54,
#'     "55 to under 65" = 55:64,
#'     "65 and older"   = 65:100)
#'
#' sex. <- discrete_format(
#'     "Total"  = 1:2,
#'     "Male"   = 1,
#'     "Female" = 2)
#'
#' my_data |> crosstabs(age, sex,
#'                      formats   = list(age = age., sex = sex.),
#'                      titles    = titles,
#'                      footnotes = footnotes)
#'
#' # Split cross table by expressions of another variable
#' my_data |> crosstabs(age, sex, by = education)
#'
#' # Compute different stats
#' my_data |> crosstabs(age, sex,
#'                      statistics = c("sum", "freq", "pct_row", "pct_column", "pct_total"))
#'
#' # Get a list with two data tables for further usage
#' result_list <- my_data |> crosstabs(age, sex,
#'                                     formats = list(age = age., sex = sex.))
#'
#' # Output in text file
#' my_data |> crosstabs(age, sex, output = "text")
#'
#' # Output to Excel
#' my_data |> crosstabs(age, sex, output = "excel")
#'
#' # With individual styling
#' my_style <- excel_output_style(header_back_color = "0077B6",
#'                                font              = "Times New Roman")
#'
#' my_data |> crosstabs(age, sex, output = "excel", style = my_style)
#'
#' @export
crosstabs <- function(data_frame,
                      rows,
                      columns,
                      statistics = c("sum"),
                      formats    = c(),
                      by         = c(),
                      weight     = NULL,
                      titles     = c(),
                      footnotes  = c(),
                      style      = excel_output_style(),
                      output     = "console",
                      na.rm      = FALSE,
                      print      = TRUE,
                      monitor    = FALSE){

    # Measure the time
    start_time <- Sys.time()

    monitor_df <- NULL |> monitor_start("Error handling", "Preparation")

    # First convert data frame to data table
    if (!data.table::is.data.table(data_frame)){
        data_frame <- data.table::as.data.table(data_frame)
    }

    # Evaluate formats early, otherwise apply formats can't evaluate them in unit
    # test situation.
    formats_list <- as.list(substitute(formats))[-1]

    formats <- stats::setNames(
        lapply(formats_list, function(expression){
            # Catch expression if passed as string
            if (is.character(expression)) {
                tryCatch(get(expression, envir = parent.frame()),
                         error = function(e) NULL)
            }
            # Catch expression if passed as symbol
            else{
                tryCatch(eval(expression, envir = parent.frame()),
                         error = function(e) NULL)
            }
        }),
        names(formats_list))

    # Look up variable names in format data frame to check whether there is an
    # interval or discrete format
    flag_interval <- FALSE

    for (current_var in names(formats)){
        format_df          <- formats[[current_var]]
        interval_variables <- c("from", "to")
        actual_variables   <- names(format_df)[1:2]

        if (identical(interval_variables, actual_variables)){
            flag_interval <- TRUE
            break
        }
    }

    ###########################################################################
    # Error handling
    ###########################################################################

    # Convert to character vectors
    rows_temp <- sub("^list\\(", "c(", gsub("\"", "", deparse(substitute(rows))))

    if (substr(rows_temp, 1, 2) == "c("){
        rows <- as.character(substitute(rows))
    }
    else if (!is_error(rows)){
        # Do nothing. In this case variables already contains the substituted variable names
        # while rows_temp is evaluated to the symbol passed into the function.
    }
    else{
        rows <- rows_temp
    }

    # Remove extra first character created with substitution
    rows <- rows[rows != "c"]

    provided_rows <- rows
    invalid_rows  <- rows[!rows %in% names(data_frame)]
    rows          <- rows[rows %in% names(data_frame)]

    if (length(invalid_rows) > 0){
        message(" ! WARNING: The provided row variable '", paste(invalid_rows, collapse = ", "), "' is not part of\n",
                "            the data frame. This variable will be omitted during computation.")
    }

    if (length(rows) == 0){
        message(" X ERROR: No valid row variable provided. Crosstabs will be aborted.")
        return(invisible(NULL))
    }

    if (length(rows) > 1){
        message(" X ERROR: Only one variable for rows allowed. Crosstabs will be aborted.")
        return(invisible(NULL))
    }

    # Convert to character vectors
    columns_temp <- sub("^list\\(", "c(", gsub("\"", "", deparse(substitute(columns))))

    if (substr(columns_temp, 1, 2) == "c("){
        columns <- as.character(substitute(columns))
    }
    else if (!is_error(columns)){
        # Do nothing. In this case variables already contains the substituted variable names
        # while columns_temp is evaluated to the symbol passed into the function.
    }
    else{
        columns <- columns_temp
    }

    # Remove extra first character created with substitution
    columns <- columns[columns != "c"]

    provided_columns <- columns
    invalid_columns  <- columns[!columns %in% names(data_frame)]
    columns          <- columns[columns %in% names(data_frame)]

    if (length(invalid_columns) > 0){
        message(" ! WARNING: The provided column variable '", paste(invalid_columns, collapse = ", "), "' is not part of\n",
                "            the data frame. This variable will be omitted during computation.")
    }

    if (length(columns) == 0){
        message(" X ERROR: No valid column variable provided. Crosstabs will be aborted.")
        return(invisible(NULL))
    }

    if (length(columns) > 1){
        message(" X ERROR: Only one variable for columns allowed. Crosstabs will be aborted.")
        return(invisible(NULL))
    }

    # Convert to character vectors
    by_temp <- sub("^list\\(", "c(", gsub("\"", "", deparse(substitute(by))))

    if (substr(by_temp, 1, 2) == "c("){
        by <- as.character(substitute(by))
    }
    else if (!is_error(by)){
        # Do nothing. In this case variables already contains the substituted variable names
        # while variables_temp is evaluated to the symbol passed into the function.
    }
    else{
        by <- by_temp
    }

    # Remove extra first character created with substitution
    by <- by[by != "c"]

    provided_by <- by
    invalid_by  <- by[!by %in% names(data_frame)]
    by          <- by[by %in% names(data_frame)]

    if (length(invalid_by) > 0){
        message(" ! WARNING: The provided by variable '", paste(invalid_by, collapse = ", "), "' is not part of\n",
                "            the data frame. This variable will be omitted during computation.")
    }

    variables  <- c(rows, columns)
    invalid_by <- by[by %in% variables]

    if (length(invalid_by) > 0){
        message(" X ERROR: The provided by variable '", paste(invalid_by, collapse = ", "), "' is also part of\n",
                "          the row and column variables which is not allowed. Crosstabs will be aborted.")
        return(invisible(NULL))
    }

    # Create temporary weight column if none is provided.
    # Also get the name of the weight variable as string.
    weight_temp <- sub("^list\\(", "c(", gsub("\"", "", deparse(substitute(weight))))

    if (weight_temp == "NULL" || substr(weight_temp, 1, 2) == "c("){
        weight_var <- ".temp_weight"
        data_frame[[".temp_weight"]] <- 1

        if (substr(weight_temp, 1, 2) == "c("){
            message(" ! WARNING: Only one variable for weight allowed. Evaluations will be unweighted.")
        }
    }
    else if (!is_numeric(data_frame[[weight_temp]])){
        weight_var <- ".temp_weight"
        data_frame[[".temp_weight"]] <- 1

        message(" ! WARNING: Provided weight variable is not numeric. Unweighted results will be computed.")
    }
    else{
        weight_var <- weight_temp

        # NA values in weight lead to errors therefor convert them to 0
        if (anyNA(data_frame[[weight_temp]])){
            message(" ~ NOTE: Missing values in weight variable '", weight_temp, "' will be converted to 0.")
        }
        data_frame[[weight_temp]] <- data.table::fifelse(is.na(data_frame[[weight_temp]]), 0, data_frame[[weight_temp]])

        # @Hack: so I don't have to check if .temp_weight exists later on
        data_frame[[".temp_weight"]] <- 1
    }

    # Check for invalid output option
    if (!tolower(output) %in% c("console", "text", "excel", "excel_nostyle")){
        message(" ! WARNING: Output format '", output, "' not available. Using 'console' instead.")

        output <- "console"
    }
    else{
        output <- tolower(output)
    }

    # Get the intersection of the requested statistics to make sure
    # only valid actions are passed down
    statistics <- statistics[unique(statistics) %in%
                             c("sum", "freq", "pct_row", "pct_column", "pct_total")]

    # If no valid statistics selected, default to sum
    if (length(statistics) == 0){
        statistics <- "sum"
    }

    ###########################################################################
    # Cross tabulation starts
    ###########################################################################

    monitor_df <- monitor_df |> monitor_next("Summary", "Summary")
    message("\n > Computing stats.")

    # Summarise results for a crosstabs output
    data_frame[["var"]] <- 1

    # Put together vector of grouping variables
    group_vars <- c(by, rows, columns)

    # In case no by variables are specified
    if (length(by) == 0){
        cross_tab <- suppressMessages(data_frame |>
             summarise_plus(class      = group_vars,
                            values     = "var",
                            statistics = c("sum", "pct_group", "pct_total", "freq"),
                            formats    = formats,
                            weight     = weight_var,
                            nesting    = "deepest",
                            notes      = FALSE,
                            na.rm      = na.rm)) |>
            drop_type_vars() |>
            collapse::frename(var_pct_group = var_pct_row)

        # Get the expressions of the column variable to use them later as column names
        column_names <- unique(as.character(cross_tab[[columns]]))

        # Add unique ids to column variables so that the pivoted variable names receive
        # running numbers instead of the values or labels.
        run_nr_df        <- data.table::data.table(column_names)
        names(run_nr_df) <- columns
        run_nr_df        <- run_nr_df |> running_number()

        cross_tab <- cross_tab |>
            collapse::join(run_nr_df,
                           on      = columns,
                           how     = "left",
                           verbose = FALSE)

        # Pivot to wider format, which basically is the final format to print the data
        cross_tab <- cross_tab |>
            collapse::pivot(id     = rows,
                            names  = "run_nr",
                            values = c("var_sum", "var_pct_row", "var_pct_total", "var_freq"),
                            how    = "wider")
    }
    # In case by variables are specified
    else{
        combinations <- as.vector(outer(by, paste0(rows, "+", columns), paste, sep = "+"))

        cross_tab <- suppressMessages(data_frame |>
            summarise_plus(class      = group_vars,
                           values     = "var",
                           statistics = c("sum", "pct_group", "pct_total", "freq"),
                           formats    = formats,
                           weight     = weight_var,
                           nesting    = "all",
                           types      = combinations,
                           notes      = FALSE,
                           na.rm      = na.rm)) |>
            collapse::frename(var_pct_group = var_pct_row) |>
            fuse_variables("by_vars", by)

        cross_tab[["BY"]] <- sub("\\+.*", "", cross_tab[["TYPE"]])

        # Get the expressions of the column variable to use them later as column names
        column_names <- unique(as.character(cross_tab[[columns]]))

        # Add unique ids to column variables so that the pivoted variable names receive
        # running numbers instead of the values or labels.
        run_nr_df        <- data.table::data.table(column_names)
        names(run_nr_df) <- columns
        run_nr_df        <- run_nr_df |> running_number()

        cross_tab <- cross_tab |>
            collapse::join(run_nr_df,
                           on      = columns,
                           how     = "left",
                           verbose = FALSE)

        # Pivot to wider format, which basically is the final format to print the data
        cross_tab <- cross_tab |>
            collapse::pivot(id     = c("BY", "by_vars", rows),
                            names  = "run_nr",
                            values = c("var_sum", "var_pct_row", "var_pct_total", "var_freq"),
                            how    = "wider")
    }

    if (is.null(cross_tab)){
        message(" X ERROR: Crosstab could not be computed.")
        return(invisible(NULL))
    }

    # Prepare table format for output
    message(" > Formatting tables.")

    if (output %in% c("console", "text")){
        monitor_df <- monitor_df |> monitor_next("Format tables", "Format tables")

        # In case no by variables are provided
        if (length(by) == 0){
            complete_table  <- format_cross_text(cross_tab, rows, columns, column_names,
                                                 statistics, formats, by, titles, footnotes)
        }
        # In case there are  by variables are provided
        else{
            complete_table <- format_cross_by_text(cross_tab, rows, columns, column_names,
                                                   statistics, formats, by, titles, footnotes, na.rm)
        }
    }
    else if (output == "excel" || output == "excel_nostyle"){
        wb <- openxlsx2::wb_workbook() |>
            prepare_styles(style)

        monitor_df <- monitor_df |> monitor_end()

        # In case no by variables are provided
        if (length(by) == 0){
            wb_list <- format_cross_excel(wb, cross_tab, rows, columns, column_names,
                                          statistics, formats, by, titles, footnotes, style, output,
                                          monitor_df = monitor_df)

            wb         <- wb_list[[1]]
            monitor_df <- wb_list[[2]]
        }
        # In case there are  by variables are provided
        else{
            wb_list <- format_cross_by_excel(cross_tab, rows, columns, column_names,
                                             statistics, formats, by, titles, footnotes,
                                             style, output, na.rm, wb, monitor_df)

            wb         <- wb_list[[1]]
            monitor_df <- wb_list[[2]]
        }
    }

    # Output formatted table into different formats
    if (print){
        monitor_df <- monitor_df |> monitor_next("Output tables", "Output tables")

        if (output %in% c("console")){
            cat(paste(complete_table, collapse = "\n"), "\n\n")
        }
        else if (output == "text"){
            temp_file <- tempfile(fileext = ".txt")
            writeLines(complete_table, temp_file)
            file.show(temp_file)
        }
        else if (output == "excel" || output == "excel_nostyle"){
            if (is.null(style[["file"]])){
                if(interactive()){
                    wb$open()
                }
            }
            else{
                wb$save(file = style[["file"]], overwrite = TRUE)
            }
        }
    }

    end_time <- round(difftime(Sys.time(), start_time, units = "secs"), 3)
    message("\n- - - 'crosstabs' execution time: ", end_time, " seconds\n")

    monitor_df <- monitor_df |> monitor_end()
    monitor_df |> monitor_plot(draw_plot = monitor)

    invisible(cross_tab)
}

###############################################################################
# Format cross table for console output
###############################################################################
#' Format Cross Table Output (Text Based)
#'
#' @description
#' Format a cross table with the provided row and column variables. Statistics
#' can be weighted sums, unweighted frequencies or different percentages.
#'
#' @param cross_tab The data frame which contains the information for this cross
#' table.
#' @param rows The variable that appears in the table rows.
#' @param columns The variable that appears in the table columns.
#' @param column_names The real column names that appear in the table.
#' @param statistics The user requested statistics.
#' @param formats A list in which is specified which formats should be applied to
#' which variable.
#' @param by Separate the frequency output by the expressions of the provided variables.
#' @param titles Character vector of titles to display above the table.
#' @param footnotes Character vector of footnotes to display under the table.
#'
#' @return
#' Returns a character vector with all formatted rows for the tables.
#'
#' @noRd
format_cross_text <- function(cross_tab,
                              rows,
                              columns,
                              column_names,
                              statistics,
                              formats,
                              by,
                              titles,
                              footnotes){
    complete_tabs <- c()

    # Set equal number of maximum column width
    sum_tab          <- setup_print_table(cross_tab, rows, "sum")
    longest_value    <- nchar(format(sum_tab[nrow(sum_tab), "total"], scientific = FALSE))
    max_column_width <- max(8, longest_value + 7)

    # Compute individual cross tables for each operation
    for (stat in statistics){
        # Setup cross table for formatting
        var_tab <- setup_print_table(cross_tab, rows, stat)

        # Get the maximum width of the provided variable names to determine the width
        # of the first column.
        first_column_width <- nchar(paste0(rows, " / ", columns))

        first_column_width <- max(first_column_width,
                                  nchar(stats::na.omit(as.character(var_tab[[rows]]))))

        # Use the max column width to evenly spread the columns.
        # Wrap the column headers according to this width.
        if (!is_multilabel(formats, columns)){
            multi_header   <- wrap_header(c(column_names, "total"), max_column_width)

            formatted_cols <- vector("list", length(c(column_names, "total")))
        }
        # In case of multilabels don't add total column
        else{
            multi_header   <- wrap_header(column_names, max_column_width)

            var_tab        <- var_tab |> dropp("total")
            formatted_cols <- vector("list", length(column_names))
        }

        # Format the first column and connect it with the rest of the header
        first_column <- c(paste0(format(paste0(rows, " / ", columns),
                                        width   = first_column_width,
                                        justify = "left"), " |"),
                          rep(paste0(format("",
                                            width   = first_column_width,
                                            justify = "left"), " |"),
                              nrow(multi_header) - 1))
        multi_header <- cbind(first_column, multi_header)

        # Visually separate total column if there is one
        total_col <- FALSE

        if (trimws(multi_header[1, ncol(multi_header)]) == "total"){
            total_col <- TRUE

            multi_header[1, ncol(multi_header)] <-
                paste0(" | ", multi_header[1, ncol(multi_header)])
        }

        # Convert header matrix into character vector with single lines
        complete_header <- apply(multi_header, 1, paste, collapse = " ")
        complete_header <- c(complete_header, strrep("-", nchar(complete_header[1])))

        # If row multilabel, remove automatically computed total observation
        if (is_multilabel(formats, rows)){
            var_tab <- var_tab |>
                collapse::fsubset(var_tab[[rows]] != "total")

            if (stat == "pct_row"){
                if (length(by) == 0){
                    message(" ~ NOTE: The format for variable '", rows, "' is a multilabel.\n",
                            "         In this case row percentages aren't computed properly.")
                }
            }
        }

        # Format table column by column. Basically concatenate first column text and
        # corresponding values together while keeping the individual maximum column
        # width in mind.
        first_column <- paste0(format(var_tab[[rows]],
                                      width   = first_column_width,
                                      justify = "left"), " | ")

        # Loop through columns and format each number in a readable way
        for (column in seq_len(length(formatted_cols))){
            # Percentages and Unweighted freq
            if (stat %in% c("pct_row", "pct_column", "pct_total")){
                formatted_cols[[column]] <- format_number(var_tab[column + 1],
                                                          width = max_column_width,
                                                          stat  = names(var_tab[column + 1]))
            }
            # Unweighted frequencies
            else if (stat %in% c("freq") ||
                     identical(as.numeric(cross_tab[["var_sum_1"]]),
                               as.numeric(cross_tab[["var_freq_1"]]))){
                formatted_cols[[column]] <- format_number(var_tab[column + 1],
                                                          width    = max_column_width,
                                                          stat     = names(var_tab[column + 1]),
                                                          decimals = 0)
            }
            # Weighted sums
            else if (stat %in% c("sum")){
                formatted_cols[[column]] <- format_number(var_tab[column + 1],
                                                          width    = max_column_width,
                                                          stat     = names(var_tab[column + 1]),
                                                          decimals = 3)
            }

            # Visually separate total column if there is one
            if (column == length(formatted_cols) && total_col){
                formatted_cols[[column]] <- paste0(" | ", formatted_cols[[column]])
            }
        }

        # Convert list to matrix
        formatted_matrix <- do.call(cbind, formatted_cols)

        # Convert matrix to single formatted rows
        all_rows <- paste0(first_column,
                           apply(formatted_matrix, 1, paste, collapse = " "))

        # If no multilabel formats are applied separate total row at the bottom from the rest
        if (!is_multilabel(formats, rows)){
            rows_temp <- length(all_rows)

            all_rows <- c(all_rows[1:(rows_temp - 1)],
                          strrep("-", nchar(complete_header[1])),
                          all_rows[rows_temp])
        }

        # Insert titles and footnotes
        complete_table <- c(complete_header, all_rows)

        if (length(titles) > 0){
            complete_table <- c(titles, "", complete_table)
        }
        if (length(footnotes) > 0){
            complete_table <- c(complete_table, "____________", footnotes)
        }

        # Output formatted result
        complete_tabs <- c(complete_tabs, "\n", complete_table, "\n")
    }

    # Output formatted result
    complete_tabs
}


#' Wrap Text Based Table Header
#'
#' @description
#' The width for each column gets a maximum value. After that maximum value the
#' header wraps to the next line to ensure the table doesn't gets too wide.
#'
#' @param cross_tab The data frame which contains the information for this cross
#' table.
#' @param rows The variables that appear in the table rows.
#' @param stat The currently computed stat.
#'
#' @return
#' Returns a data table trimmed down to the necessary variables and observations.
#'
#' @noRd
wrap_header <- function(column_names, width = 10){
    # Split each column name into single letters
    letter_list <- strsplit(column_names, "")

    wrapped <- lapply(letter_list, function(word){
        # How many chunks are needed for this label
        n_chunks <- ceiling(length(word) / (width - 1))

        # Extract substrings of max 'width' characters
        parts <- character(n_chunks)

        for (i in seq_len(n_chunks)){
            start    <- (i - 1) * (width - 1) + 1
            end      <- min(i * (width - 1), length(word))
            parts[i] <- paste(word[start:end], collapse = "")
        }

        format(parts, width = width, justify = "right")
    })

    # Find the maximum number of lines across all labels
    max_lines <- max(sapply(wrapped, length))

    # Pad shorter labels with empty strings so they all align
    wrapped_padded <- lapply(wrapped, function(parts){
        c(parts, rep(format("", width = width, justify = "right"), max_lines - length(parts)))
    })

    # Return as a matrix: rows = lines, cols = labels
    do.call(cbind, wrapped_padded)
}


#' Trim Cross Table to the Needed Parts
#'
#' @description
#' Cross tables are computed by stats. Here the stats get reduced the the current
#' stat in the loop. Column percentages get newly computed because they aren't
#' computed beforehand.
#'
#' @param cross_tab The data frame which contains the information for this cross
#' table.
#' @param rows The variables that appear in the table rows.
#' @param stat The currently computed stat.
#'
#' @return
#' Returns a data table trimmed down to the necessary variables and observations.
#'
#' @noRd
setup_print_table <- function(cross_tab, rows, stat){
    # Compute column percentages
    if (stat == "pct_column"){
        var_tab <- cross_tab |>
            collapse::get_vars(c(rows, "sum"), regex = TRUE)

        # Compute column percentages and rename variables
        for (column in names(var_tab[-1])){
            new_var <- sub("sum", "pct_column", column)
            var_tab[[new_var]] <- var_tab[[column]] * 100 / collapse::fsum(var_tab[[column]])
        }

        # Get only the needed variables for this iteration
        var_tab <- var_tab |>
            collapse::get_vars(c(rows, stat), regex = TRUE)
    }
    else{
        # Get only the needed variables for this iteration
        var_tab <- cross_tab |>
            collapse::get_vars(c(rows, stat), regex = TRUE)
    }

    # Compute row and column sums
    var_tab[["total"]]    <- rowSums(var_tab[, -1], na.rm = TRUE)

    column_totals         <- var_tab[1, ]
    column_totals[[rows]] <- "total"
    column_totals[-1]     <- colSums(var_tab[, -1], na.rm = TRUE)
    var_tab               <- rbind(var_tab, column_totals)

    # Set unneeded totals to NA
    if (stat == "pct_column"){
        var_tab[["total"]] <- NA
    }
    else if (stat == "pct_row"){
        var_tab[nrow(var_tab), -1] <- NA
    }

    var_tab
}

###############################################################################
# Format cross table for excel output
###############################################################################
#' Format Cross Table Output (Excel Based)
#'
#' @description
#' Format a cross table with the provided row and column variables. Statistics
#' can be weighted sums, unweighted frequencies or different percentages.
#'
#' @param wb An already created workbook to add more sheets to.
#' @param cross_tab The data frame which contains the information for this cross
#' table.
#' @param rows The variable that appears in the table rows.
#' @param columns The variable that appears in the table columns.
#' @param column_names The real column names that appear in the table.
#' @param statistics The user requested statistics.
#' @param formats A list in which is specified which formats should be applied to
#' which variable.
#' @param by Separate the cross table output by the expressions of the provided variables.
#' @param titles Character vector of titles to display above the table.
#' @param footnotes Character vector of footnotes to display under the table.
#' @param style A list containing the styling elements.
#' @param output Determines whether to style the output or to just quickly paste
#' the data.
#' @param by_info Text which contains the information which variable with which
#' expression is computed at the moment.Used for computation with by variables.
#' @param index Index of the current variable expression. Used for computation with
#' by variables.
#' @param monitor_df Data frame which stores the monitoring values.
#'
#' @return
#' Returns a list containing a formatted Excel workbook as well as the monitoring
#' data frame.
#'
#' @noRd
format_cross_excel <- function(wb,
                               cross_tab,
                               rows,
                               columns,
                               column_names,
                               statistics,
                               formats,
                               by,
                               titles,
                               footnotes,
                               style,
                               output,
                               by_info = NULL,
                               index   = NULL,
                               monitor_df){
    monitor_df <- monitor_df |> monitor_start("Excel prepare", "Format")

    # Compute individual cross tables for each operation
    for (stat in statistics){
        monitor_df <- monitor_df |> monitor_next("Excel prepare", "Format")

        # Setup cross table for formatting
        var_tab <- setup_print_table(cross_tab, rows, stat)

        # Round values according to style options
        if (stat %in% c("sum", "freq")){
            var_tab <- var_tab |>
                collapse::fmutate(across(is.numeric, round,
                                         digits = style[["number_formats"]][["sum_decimals"]]))
        }
        else{
            var_tab <- var_tab |>
                collapse::fmutate(across(is.numeric, round,
                                         digits = style[["number_formats"]][["pct_decimals"]]))
        }

        # If row multilabel, remove automatically computed total observation
        if (is_multilabel(formats, rows)){
            var_tab <- var_tab |>
                collapse::fsubset(var_tab[[rows]] != "total")

            if (stat == "pct_row"){
                if (length(by) == 0){
                    message(" ~ NOTE: The format for variable '", rows, "' is a multilabel.\n",
                            "         In this case row percentages aren't computed properly.")
                }
            }
        }

        column_names[is.na(column_names)] <- style[["na_symbol"]]

        # If column multilabel, remove automatically computed total column.
        # Set the real column names according to number of columns.
        if (is_multilabel(formats, columns)){
            var_tab <- var_tab |> dropp("total")

            names(var_tab) <- c(paste0(rows, " / ", columns), column_names)
        }
        else{
            names(var_tab) <- c(paste0(rows, " / ", columns), column_names, "total")
        }

        # Get table ranges
        cross_ranges <- get_table_ranges(var_tab, titles, footnotes, style)

        # If function is called with by variables the sheet names have to be differentiated
        # and by info has to be written besides the table.
        if (!is.null(by_info)){
            by_info_col <- style[["start_column"]] + length(var_tab) + 1

            wb$add_worksheet(paste0(stat, index),
                             grid_lines = style[["grid_lines"]])
            wb$add_data(x         = by_info,
                        start_col = by_info_col,
                        start_row = style[["start_row"]])
        }
        else{
            wb$add_worksheet(stat, grid_lines = style[["grid_lines"]])
        }

        # Format titles and footnotes if there are any
        monitor_df <- monitor_df |> monitor_next("Excel titles/footnotes", "Format")
        wb <- wb |>
            format_titles_foot_excel(titles, footnotes, cross_ranges, style, output)

        # Add table data and format according to style options
        monitor_df <- monitor_df |> monitor_next("Excel data", "Format")

        wb$add_data(x          = var_tab,
                    start_col  = style[["start_column"]],
                    start_row  = cross_ranges[["header.row"]],
                    na.strings = style[["na_symbol"]])

        # Only do the formatting when user specified it. With the excel_nostyle
        # option this whole part gets omitted to get a very quick unformatted
        # excel output.
        if (output == "excel"){
            monitor_df <- monitor_df |> monitor_next("Excel cell styles", "Format")
            wb <- wb |> handle_cell_styles(cross_ranges, style)

            if (stat == "sum" || stat== "freq"){
                wb$add_cell_style(dims                = cross_ranges[["table_range"]],
                                  apply_number_format = TRUE,
                                  num_fmt_id          = wb$styles_mgr$get_numfmt_id(paste0(stat, "_numfmt")))
            }
            else{
                wb$add_cell_style(dims                = cross_ranges[["table_range"]],
                                  apply_number_format = TRUE,
                                  num_fmt_id          = wb$styles_mgr$get_numfmt_id("pct_numfmt"))
            }

            # Adjust table dimensions
            monitor_df <- monitor_df |> monitor_next("Excel widths/heights", "Format")
            wb <- wb |> handle_col_row_dimensions(cross_ranges,
                                                  ncol(var_tab) + (style[["start_column"]] - 1),
                                                  nrow(var_tab) + (style[["start_row"]] - 1),
                                                  style)

            wb <- wb |> handle_auto_dimensions(cross_ranges,
                                               style)

            wb <- wb |> handle_header_table_dim(cross_ranges,
                                                style)

            wb$add_ignore_error(dims = cross_ranges[["header_range"]],  number_stored_as_text = TRUE)
            wb$add_ignore_error(dims = cross_ranges[["cat_col_range"]], number_stored_as_text = TRUE)

            wb$add_named_region(dims = cross_ranges[["whole_tab_range"]], name = "table", local_sheet = TRUE)
            wb$add_named_region(dims = cross_ranges[["table_range"]],     name = "data",  local_sheet = TRUE)
        }
    }

    monitor_df <- monitor_df |> monitor_end()

    # Return workbook
    list(wb, monitor_df)
}

###############################################################################
# Format grouped by tables for console output
###############################################################################
#' Format Cross Table Output with by Variables (Text Based)
#'
#' @description
#' Format a cross table with the provided row and column variables. Statistics
#' can be weighted sums, unweighted frequencies or different percentages.
#'
#' @param cross_tab The data frame which contains the information for this cross
#' table.
#' @param rows The variable that appears in the table rows.
#' @param columns The variable that appears in the table columns.
#' @param column_names The real column names that appear in the table.
#' @param statistics The user requested statistics.
#' @param formats A list in which is specified which formats should be applied to
#' which variable.
#' @param by Separate the output by the expressions of the provided variables.
#' @param titles Character vector of titles to display above the table.
#' @param footnotes Character vector of footnotes to display under the table.
#' @param na.rm If TRUE removes all NA values from the tabulation.
#'
#' @return
#' Returns a character vector with all formatted rows for the tables.
#'
#' @noRd
format_cross_by_text <- function(cross_tab,
                                 rows,
                                 columns,
                                 column_names,
                                 statistics,
                                 formats,
                                 by,
                                 titles,
                                 footnotes,
                                 na.rm){
    # Print message if multilabels are applied
    if (is_multilabel(formats, rows)){
        if ("pct_row" %in% statistics){
            message(" ~ NOTE: The format for variable '", rows, "' is a multilabel.\n",
                    "         In this case row percentages aren't computed properly.")
        }
    }

    complete_tabs <- c()

    # Loop through all by variables
    for (by_var in by){
        # Select by variables one by one
        cross_by <- cross_tab |>
            collapse::fsubset(cross_tab[["BY"]] == by_var)

        # Extract unique values
        if (anyNA(cross_by[["by_vars"]])){
            values <- c(unique(stats::na.omit(cross_by[["by_vars"]])), NA)
        }
        else{
            values <- unique(cross_by[["by_vars"]])
        }

        # Loop through all unique values to generate frequency tables per expression
        for (value in values){
            # In case NAs are removed
            if (is.na(value) && na.rm){
                next
            }

            # Put additional header together with the information which by variable
            # and which value is currently filtered.
            header <- paste0("| ", by_var, " = ", value, " |")

            complete_header <- c("\n", strrep("-", nchar(header)),
                                 header,
                                 strrep("-", nchar(header)))

            # Filter table by current by variable and value
            if (!is.na(value)){
                cross_temp <- cross_by |>
                    collapse::fsubset(cross_by[["by_vars"]] == value)
            }
            else{
                cross_temp <- cross_by |>
                    collapse::fsubset(is.na(cross_by[["by_vars"]]))
            }

            # Generate frequency tables as normal but base is filtered data frame
            current_cross <- format_cross_text(cross_temp,
                                               rows,
                                               columns,
                                               column_names,
                                               statistics,
                                               formats,
                                               by,
                                               titles,
                                               footnotes)

            # Output formatted result
            complete_tabs <- c(complete_tabs, complete_header, current_cross)
        }
    }

    # Output formatted result
    complete_tabs
}

###############################################################################
# Format grouped by tables for excel output
###############################################################################
#' Format Cross Table Output with by Variables (Excel Based)
#'
#' @description
#' Format a cross table with the provided row and column variables. Statistics
#' can be weighted sums, unweighted frequencies or different percentages.
#'
#' @param cross_tab The data frame which contains the information for this cross
#' table.
#' @param rows The variable that appears in the table rows.
#' @param columns The variable that appears in the table columns.
#' @param column_names The real column names that appear in the table.
#' @param statistics The user requested statistics.
#' @param formats A list in which is specified which formats should be applied to
#' which variable.
#' @param by Separate the output by the expressions of the provided variables.
#' @param titles Character vector of titles to display above the table.
#' @param footnotes Character vector of footnotes to display under the table.
#' @param style A list containing the styling elements.
#' @param output Determines whether to style the output or to just quickly paste
#' the data.
#' @param na.rm If TRUE removes all NA values from the tabulation.
#' @param monitor_df Data frame which stores the monitoring values.
#'
#' @return
#' Returns a list containing a formatted Excel workbook as well as the monitoring
#' data frame.
#'
#' @noRd
format_cross_by_excel <- function(cross_tab,
                                  rows,
                                  columns,
                                  column_names,
                                  statistics,
                                  formats,
                                  by,
                                  titles,
                                  footnotes,
                                  style,
                                  output,
                                  na.rm,
                                  wb,
                                  monitor_df){
    monitor_df <- monitor_df |> monitor_start("Excel prepare (by)", "Format by")

    # Print message if multilabels are applied
    if (is_multilabel(formats, rows)){
        if ("pct_row" %in% statistics){
            message(" ~ NOTE: The format for variable '", rows, "' is a multilabel.\n",
                    "         In this case row percentages aren't computed properly.")
        }
    }

    index <- 1

    # Loop through all by variables
    monitor_df <- monitor_df |> monitor_end()

    for (by_var in by){
        monitor_df <- monitor_df |> monitor_start(paste0("Excel prepare (", by_var, ")"), "Format by")

        # Select by variables one by one
        cross_by <- cross_tab |>
            collapse::fsubset(cross_tab[["BY"]] == by_var)

        # Extract unique values
        if (anyNA(cross_by[["by_vars"]])){
            values <- c(unique(stats::na.omit(cross_by[["by_vars"]])), NA)
        }
        else{
            values <- unique(cross_by[["by_vars"]])
        }

        monitor_df <- monitor_df |> monitor_end()

        # Loop through all unique values to generate frequency tables per expression
        for (value in values){
            # In case NAs are removed
            if (is.na(value) && na.rm){
                next
            }

            monitor_df <- monitor_df |> monitor_start(paste0("Excel (", by_var, "_", value, ")"), "Format by")
            message("   + ", paste0(by_var, " = ", value))

            # Put additional by info together with the information which by variable
            # and which value is currently filtered.
            by_info <- paste0(by_var, " = ", value)

            # Filter table by current by variable and value
            if (!is.na(value)){
                cross_temp <- cross_by |>
                    collapse::fsubset(cross_by[["by_vars"]] == value)
            }
            else{
                cross_temp <- cross_by |>
                    collapse::fsubset(is.na(cross_by[["by_vars"]]))
            }

            # Generate frequency tables as normal but base is filtered data frame
            wb_list <- format_cross_excel(wb,
                                          cross_temp,
                                          rows,
                                          columns,
                                          column_names,
                                          statistics,
                                          formats,
                                          by,
                                          titles,
                                          footnotes,
                                          style,
                                          output,
                                          by_info,
                                          index,
                                          NULL)

            index <- index + 1

            wb <- wb_list[[1]]

            monitor_df <- monitor_df |> monitor_end()
        }

        monitor_df <- monitor_df |> monitor_end()
    }

    # Return workbook
    list(wb, monitor_df)
}
