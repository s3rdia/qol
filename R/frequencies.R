#' Display Frequency Tables of Single Variables
#'
#' @description
#' [frequencies()] produces two kinds of tables for a quick overview of single variables.
#' The first table is for a broader overview and contains mean, sd, min, max, freq and missings.
#' The second table is the actual frequency table which shows the weighted sums, percentages
#' and unweighted frequencies per expression.
#'
#' @param data_frame A data frame in which are the variables to tabulate.
#' @param variables A vector of single variables to create frequency tables for.
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
#' [frequencies()] is based on the 'SAS' procedure Proc Freq, which provides
#' efficient and readable ways to output frequency tables.
#'
#' To create a frequency table you only need to provide a single variable.
#' Nothing special about this. The real power comes into play, when you output your
#' tables as a fully styled 'Excel' workbook. Setting up a custom, reusable style is
#' as easy as setting up options like: provide a color for the table header, set the
#' font size for the row header, should borders be drawn for the table cells yes/no,
#' and so on.
#'
#' You also can provide multiple single variables to generate multiple, evenly designed
#' tables, all at once. For just a quick overview, rather than fully designed
#' tables, you can also just output the tables in ASCII style format.
#'
#' @return
#' Returns a list of two data tables containing the results for the frequency tables.
#'
#' @seealso
#' Creating a custom table style: [excel_output_style()], [modify_output_style()],
#' [number_format_style()], [modify_number_formats()].
#'
#' Creating formats: [discrete_format()] and [interval_format()].
#'
#' Functions that can handle formats and styles: [crosstabs()], [any_table()].
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
#' # Output frequencies tables
#' my_data |> frequencies(sex)
#' my_data |> frequencies(c(age, education),
#'                        weight = weight)
#'
#' # Also works with characters
#' my_data |> frequencies("sex")
#' my_data |> frequencies(c("age", "education"),
#'                        weight = "weight")
#'
#' # Applying formats and titles
#' sex. <- discrete_format(
#'     "Total"  = 1:2,
#'     "Male"   = 1,
#'     "Female" = 2)
#'
#' my_data |> frequencies(sex, formats(sex = sex.),
#'                        titles    = titles,
#'                        footnotes = footnotes)
#'
#' # Split frequencies by expressions of another variable
#' my_data |> frequencies(sex, by = education)
#'
#' # Get a list with two data tables for further usage
#' result_list <- my_data |> frequencies(sex, formats(sex = sex.))
#'
#' # Output in text file
#' my_data |> frequencies(sex, output = "text")
#'
#' # Output to Excel
#' my_data |> frequencies(sex, output = "excel")
#'
#' # With individual styling
#' my_style <- excel_output_style(header_back_color = "0077B6",
#'                                font              = "Times New Roman")
#'
#' my_data |> frequencies(sex, output = "excel", style = my_style)
#'
#' @export
frequencies <- function(data_frame,
                        variables,
                        formats   = c(),
                        by        = c(),
                        weight    = NULL,
                        titles    = c(),
                        footnotes = c(),
                        style     = excel_output_style(),
                        output    = "console",
                        na.rm     = FALSE,
                        print     = TRUE,
                        monitor   = FALSE){

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
    variables_temp <- sub("^list\\(", "c(", gsub("\"", "", deparse(substitute(variables))))

    if (substr(variables_temp, 1, 2) == "c("){
        variables <- as.character(substitute(variables))
    }
    else if (!is_error(variables)){
        # Do nothing. In this case variables already contains the substituted variable names
        # while variables_temp is evaluated to the symbol passed into the function.
    }
    else{
        variables <- variables_temp
    }

    # Remove extra first character created with substitution
    variables <- variables[variables != "c"]

    provided_variables <- variables
    invalid_variables  <- variables[!variables %in% names(data_frame)]
    variables          <- variables[variables %in% names(data_frame)]

    if (length(invalid_variables) > 0){
        message(" ! WARNING: The provided analysis variable '", paste(invalid_variables, collapse = ", "), "' is not part of\n",
                "            the data frame. This variable will be omitted during computation.")
    }

    if (length(variables) == 0){
        message(" X ERROR: No valid analysis variables provided. Frequencies will be aborted.")
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

    invalid_by <- by[by %in% variables]

    if (length(invalid_by) > 0){
        message(" X ERROR: The provided by variable '", paste(invalid_by, collapse = ", "), "' is also part of\n",
                "          the frequency variables which is not allowed. Frequencies will be aborted.")
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

    ###########################################################################
    # Frequency starts
    ###########################################################################

    monitor_df <- monitor_df |> monitor_next("Mean summary", "Summary")
    message("\n > Computing stats.")

    # For mean tabulation only numeric variables are allowed. So here all character
    # variables are excluded temporarily.
    vars_mean <- variables

    for (variable in vars_mean){
        if (is.character(data_frame[[variable]])){
            vars_mean <- vars_mean[vars_mean != variable]
        }
    }

    # In case no by variables are specified
    if (length(by) == 0){
        # Compute stats for mean summary
        mean_tab <- suppressMessages(data_frame |>
             summarise_plus(values     = vars_mean,
                            statistics = c("mean", "sd", "min", "max", "freq", "missing"),
                            weight     = weight_var,
                            notes      = FALSE,
                            na.rm      = na.rm) |>
                 drop_type_vars())
    }
    # In case by variables are specified
    else{
        mean_tab <- suppressMessages(data_frame |>
             summarise_plus(class      = by,
                            values     = vars_mean,
                            statistics = c("mean", "sd", "min", "max", "freq", "missing"),
                            formats    = formats,
                            weight     = weight_var,
                            nesting    = "all",
                            types      = by,
                            notes      = FALSE,
                            na.rm      = na.rm))

        if (!is.null(mean_tab)){
            mean_tab <- mean_tab |>
                fuse_variables("by_vars", by) |>
                collapse::fsubset(DEPTH > 0)
        }
    }

    # Put results for each variable below each other instead of beside
    mean_columns <- c("mean", "sd", "min", "max", "freq", "miss")
    all_rows     <- list()

    for (variable in vars_mean){
        # Extract all stats for a single variable and give them uniform names
        var_row <- suppressMessages(mean_tab |>
            keep("TYPE", "by_vars",
                 grep(variable,
                      names(mean_tab),
                      value = TRUE)))

        if (length(by) == 0){
            names(var_row) <- mean_columns
        }
        else{
            names(var_row) <- c("TYPE", "by_vars", mean_columns)
        }

        # Put variable name in front and put formatted row below the other ones
        # if there are any.
        var_row              <- cbind(variable, var_row)
        all_rows[[variable]] <- var_row
    }

    mean_tab <- data.table::rbindlist(all_rows, use.names = TRUE, fill = TRUE)

    # Summarise results for a frequency output per variable
    monitor_df <- monitor_df |> monitor_next("Freq summary", "Summary")

    data_frame[["var"]] <- 1

    # In case no by variables are specified
    if (length(by) == 0){
        freq_tab <- suppressMessages(data_frame |>
             summarise_plus(class      = variables,
                            values     = "var",
                            statistics = c("sum", "pct_group", "freq"),
                            formats    = formats,
                            weight     = weight_var,
                            nesting    = "single",
                            notes      = FALSE,
                            na.rm      = na.rm))
    }
    # In case by variables are specified
    else{
        group_vars   <- c(by, variables)
        combinations <- as.vector(outer(by, variables, paste, sep = "+"))
        combinations <- c("total", combinations)

        freq_tab <- suppressMessages(data_frame |>
             summarise_plus(class      = group_vars,
                            values     = "var",
                            statistics = c("sum", "pct_group", "freq"),
                            formats    = formats,
                            weight     = weight_var,
                            nesting    = "all",
                            types      = combinations,
                            notes      = FALSE,
                            na.rm      = na.rm)) |>
             fuse_variables("fused_vars", variables) |>
             fuse_variables("by_vars", by)

        freq_tab[1, "by_vars"] <- "total"
        freq_tab[["BY"]]       <- sub("\\+.*", "", freq_tab[["TYPE"]])
        freq_tab[["TYPE"]]     <- sub(".*\\+", "", freq_tab[["TYPE"]])
    }

    if (is.null(mean_tab) || is.null(freq_tab)){
        message(" X ERROR: Frequencies could not be computed.")
        return(invisible(NULL))
    }

    # Prepare table format for output
    message(" > Formatting tables.")

    if (output %in% c("console", "text")){
        monitor_df <- monitor_df |> monitor_next("Format tables", "Format tables")

        # In case no by variables are provided
        if (length(by) == 0){
            complete_mean  <- format_mean_text(mean_tab, vars_mean, mean_columns)
            complete_freq  <- format_freq_text(freq_tab, variables, formats,
                                               by, titles, footnotes)

            complete_table <- c(complete_mean, complete_freq)
        }
        # In case there are  by variables are provided
        else{
            complete_table <- format_by_text(mean_tab, freq_tab, variables, mean_columns,
                                             formats, by, titles, footnotes, na.rm)
        }
    }
    else if (output == "excel" || output == "excel_nostyle"){
        wb <- openxlsx2::wb_workbook() |>
            prepare_styles(style)

        monitor_df <- monitor_df |> monitor_end()

        # In case no by variables are provided
        if (length(by) == 0){
            wb_list <- format_mean_excel(mean_tab, mean_columns, style, output, wb = wb,
                                         monitor_df = monitor_df)
            wb_list <- format_freq_excel(wb_list[[1]], freq_tab, variables, formats,
                                         by, titles, footnotes, style, output, monitor_df = wb_list[[2]])

            wb         <- wb_list[[1]]
            monitor_df <- wb_list[[2]]
        }
        # In case there are  by variables are provided
        else{
            wb_list <- format_by_excel(mean_tab, freq_tab, variables, mean_columns,
                                       formats, by, titles, footnotes, style, output,
                                       na.rm, wb, monitor_df)

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

    monitor_df <- monitor_df |> monitor_end()
    monitor_df |> monitor_plot(draw_plot = monitor)

    end_time <- round(difftime(Sys.time(), start_time, units = "secs"), 3)
    message("\n- - - 'frequencies' execution time: ", end_time, " seconds\n")

    invisible(list("mean" = mean_tab,
                   "freq" = freq_tab))
}

###############################################################################
# Format numbers with decimal points as thousands
###############################################################################
#' Enhance Readability for Large Numbers
#'
#' @description
#' Format numbers with big marks to make them more readable in the text output.
#' Also set up number of decimals.
#'
#' @param number The number to format.
#' @param width Width provided by the largest number in the table column to format
#' all numbers i a column at an even width.
#' @param stat Stat that determines the numbers formatting.
#' @param decimals Number of decimals.
#'
#' @return
#' Returns a formatted number.
#'
#' @noRd
format_number <- function(number,
                          width,
                          stat,
                          decimals = 1){

    # Format numbers to make them more readable
    if (stat %in% c("freq", "miss", "var_freq", "var_cum_freq")){
        output_format <- format(number,
                                format       = "d",
                                decimal.mark = ",",
                                big.mark     = ".",
                                scientific   = FALSE,
                                nsmall       = 0)
    }
    else if (stat %in% c("min", "max")){
        output_format <- format(round(number, decimals),
                                format       = "d",
                                decimal.mark = ",",
                                big.mark     = ".",
                                scientific   = FALSE)
    }
    # Any other stat with decimal numbers
    else{
        output_format <- format(round(number, decimals),
                                format       = "f",
                                decimal.mark = ",",
                                big.mark     = ".",
                                scientific   = FALSE,
                                nsmall       = decimals)
    }

    # Remove NA
    output_format <- sub("NA", "", output_format)

    # Output new format
    format(output_format,
           width   = width,
           justify = "right")
}

###############################################################################
# Get maximum column width for equal formatting
###############################################################################
#' Get Maximum Column Width
#'
#' @description
#' Get maximum column width based on the largest text or number written in the column.
#'
#' @param data_frame The data frame which contains the columns to measure.
#' @param columns_to_format The actual columns from the data frame to be measured for
#' their widest texts or numbers.
#' @param decimals Number of decimals for numbers.
#'
#' @return
#' Returns a vector of column widths.
#'
#' @noRd
get_column_width <- function(data_frame,
                             columns_to_format,
                             decimals = 1){
    all_widths <- c()

    # Get the maximum column width by looking up which is the largest number
    # individually in each column.
    for (column in columns_to_format){
        max_width <- nchar(column)

        if (column %in% c("freq", "miss", "var_freq", "var_cum_freq")){
            max_width <- max(max_width,
                             nchar(format(data_frame[[column]],
                                          format     = "d",
                                          big.mark   = ",",
                                          scientific = FALSE,
                                          nsmall     = 0)))
        }
        # Any other stat with decimal numbers
        else{
            max_width <- max(max_width,
                             nchar(format(round(data_frame[[column]], decimals),
                                          format     = "f",
                                          big.mark   = ",",
                                          scientific = FALSE,
                                          nsmall     = decimals)))
        }

        all_widths <- c(all_widths, max_width)
    }

    all_widths
}

###############################################################################
# Format mean table for console output
###############################################################################
#' Format Mean Table Output (Text Based)
#'
#' @description
#' Format a short summarising table for all numeric variables.
#'
#' @param mean_tab The data frame which contains the information for this summarising
#' table.
#' @param variables The variables that should appear in the table.
#' @param mean_columns The column names from the data frames that should appear
#' in the table.
#'
#' @return
#' Returns a character vector with all formatted rows for the table.
#'
#' @noRd
format_mean_text <- function(mean_tab,
                             variables,
                             mean_columns){
    if (nrow(mean_tab) == 0){
        return(c())
    }

    if ("TYPE" %in% names(mean_tab)){
        mean_tab <- mean_tab |>
            dropp("TYPE", "by_vars")
    }

    # Get the maximum width of the provided variable names to determine the width
    # of the first column.
    first_column_width <- nchar("variable")

    for (variable in variables){
        first_column_width <- max(first_column_width,
                                  nchar(mean_tab[["variable"]]))
    }

    # Set header row formatting. Loop through all header columns and give
    # each column the optimal width.
    column_widths  <- get_column_width(mean_tab, mean_columns)
    header_columns <- c()

    for (i in seq_len(length(mean_columns))){
        header_columns <- c(header_columns,
                            format(mean_columns[i],
                                   width   = column_widths[i],
                                   justify = "right"))
    }

    # Put together the first two rows of the table as complete table header
    header_row <- paste0(format("variable",
                                width   = first_column_width,
                                justify = "left"), "  |   ",
                         paste(header_columns, collapse = "   "))

    complete_header <- c(header_row,
                         strrep("-", nchar(header_row)))

    # Format table column by column. Basically concatenate first column text and
    # corresponding values together while keeping the individual maximum column
    # width in mind.
    first_column <- paste0(format(mean_tab[["variable"]],
                                  width   = first_column_width,
                                  justify = "left"), "  |   ")

    # Loop through stat columns and format each number in a readable way
    formatted_cols <- vector("list", length(mean_columns))

    for (column in seq_len(length(mean_columns))){
        formatted_cols[[column]] <- format_number(mean_tab[column + 1],
                                                  width = column_widths[column],
                                                  stat  = names(mean_tab[column + 1]))
    }

    # Convert list to matrix
    formatted_matrix <- do.call(cbind, formatted_cols)

    # Convert matrix to single formatted rows
    all_rows <- paste0(first_column,
                       apply(formatted_matrix, 1, paste, collapse = "   "))

    # Output formatted result
    c("", complete_header, all_rows)
}

###############################################################################
# Format mean table for excel output
###############################################################################
#' Format Mean Table Output (Excel Based)
#'
#' @description
#' Format a short summarising table for all numeric variables.
#'
#' @param mean_tab The data frame which contains the information for this summarising
#' table.
#' @param mean_columns The column names from the data frames that should appear
#' in the table.
#' @param style A list containing the styling elements.
#' @param output Determines whether to style the output or to just quickly paste
#' the data.
#' @param wb An already created workbook to add more sheets to.
#' @param index Index of the current variable expression. Used for computation with
#' by variables.
#' @param monitor_df Data frame which stores the monitoring values.
#'
#' @return
#' Returns a list containing a formatted Excel workbook as well as the monitoring
#' data frame.
#'
#' @noRd
format_mean_excel <- function(mean_tab,
                              mean_columns,
                              style = excel_output_style(),
                              output,
                              wb    = NULL,
                              index = NULL,
                              monitor_df){
    monitor_df <- monitor_df |> monitor_start("Excel prepare (mean)", "Format mean")

    if (nrow(mean_tab) == 0){
        monitor_df <- monitor_df |> monitor_end()

        return(list(wb, monitor_df))
    }

    # Drop TYPE if present
    if ("TYPE" %in% names(mean_tab)){
        mean_tab <- mean_tab |> dropp("TYPE", "by_vars")
    }

    # Round values according to style options
    mean_tab <- mean_tab |>
        collapse::fmutate(mean = round(mean, style[["number_formats"]][["mean_decimals"]]),
                          sd   = round(sd,   style[["number_formats"]][["sd_decimals"]]),
                          min  = round(min,  style[["number_formats"]][["min_decimals"]]),
                          max  = round(max,  style[["number_formats"]][["max_decimals"]]))

    # Build header row
    header     <- c("variable", mean_columns)
    table_data <- rbind(header, as.matrix(mean_tab[, c("variable", mean_columns)]))

    # Get table ranges
    mean_ranges <- get_mean_tab_ranges(mean_tab, style)

    # If function is called with by variables the sheet names have to be differentiated
    if (!is.null(index)){
        wb$add_worksheet(paste0("means", index),
                         grid_lines = style[["grid_lines"]])
    }
    # If function is called without by variables
    else{
        wb$add_worksheet("means", grid_lines = style[["grid_lines"]])
    }

    # Add table data and format according to style options
    monitor_df <- monitor_df |> monitor_next("Excel data (mean)", "Format mean")

    wb$add_data(x          = mean_tab,
                start_col  = style[["start_column"]],
                start_row  = style[["start_row"]],
                na.strings = style[["na_symbol"]])

    # Only do the formatting when user specified it. With the excel_nostyle
    # option this whole part gets omitted to get a very quick unformatted
    # excel output.
    if (output == "excel"){

        monitor_df <- monitor_df |> monitor_next("Excel cell styles (mean)", "Format mean")
        wb <- wb |> handle_cell_styles(mean_ranges, style)

        # Set up inner table number formats
        col_index <- 1

        for (type in c("mean", "sd", "min", "max", "freq", "missing")){
            wb$add_cell_style(dims                = mean_ranges[[paste0("mean_col_ranges", col_index)]],
                              apply_number_format = TRUE,
                              num_fmt_id          = wb$styles_mgr$get_numfmt_id(paste0(type, "_numfmt")))

            col_index <- col_index + 1
        }

        # If there are width or heights defined
        monitor_df <- monitor_df |> monitor_next("Excel widths/heights (mean)", "Format mean")

        column_width <- style[["column_widths"]]
        row_heights  <- style[["row_heights"]]

        if (length(column_width) == 1 && length(row_heights) == 1){
            if (column_width != "auto" || row_heights != "auto"){
                wb <- wb |> handle_col_row_dimensions(mean_ranges,
                                                      ncol(mean_tab) + (style[["start_column"]] - 1),
                                                      nrow(mean_tab) + (style[["start_row"]] - 1),
                                                      style)
            }
        }
        # If both widths and heights should be determined automatically.
        else{
            wb <- wb |> handle_col_row_dimensions(mean_ranges,
                                                  style[["start_column"]],
                                                  nrow(mean_tab) + (style[["start_row"]] - 1),
                                                  style)
        }

        # If autofit columns is selected, set a manual size after the first table
        # column which is sufficient in most cases. The integrated autofit often
        # gives to narrow columns.
        if (length(column_width) == 1){
            if (column_width == "auto"){
                start_col <- style[["start_column"]] + 1

                wb$set_col_widths(cols   = start_col:mean_ranges[["table.end"]],
                                  widths = 10)
            }
        }

        wb$add_named_region(dims = mean_ranges[["whole_tab_range"]], name = "table", local_sheet = TRUE)
        wb$add_named_region(dims = mean_ranges[["table_range"]],     name = "data",  local_sheet = TRUE)
    }

    # Return workbook
    monitor_df <- monitor_df |> monitor_end()

    list(wb, monitor_df)
}

###############################################################################
# Format freq table for console output
###############################################################################
#' Format Frequency Table Output (Text Based)
#'
#' @description
#' Format a frequency table for each variable which shows the counts, sums and
#' percentages per variable expression.
#'
#' @param freq_tab The data frame which contains the information for this frequency
#' table.
#' @param variables The variables that should appear in the table.
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
format_freq_text <- function(freq_tab,
                             variables,
                             formats,
                             by,
                             titles,
                             footnotes){
    complete_tabs <- c()

    # Generate a separate frequency table for each variable
    for (variable in variables){
        # Compute additional stats
        var_tab <- compute_cumulative(freq_tab, variable, formats)

        # If no weight variable was applied
        if (identical(var_tab[["var_sum"]], var_tab[["var_freq"]])){
            freq_columns <- c("var_freq", "pct_group", "var_cum_freq",
                              "var_cum_pct")

            freq_top_columns    <- c("    ", "       ", "cumulative", "cumulative")
            freq_bottom_columns <- c("freq", "percent",       "freq",    "percent")
        }
        # If weight variable was applied
        else{
            freq_columns <- c("var_sum", "pct_group", "var_cum_sum",
                              "var_cum_pct", "var_freq", "var_cum_freq")

            freq_top_columns    <- c("   ", "       ", "cumulative", "cumulative", "    ", "cumulative")
            freq_bottom_columns <- c("sum", "percent",        "sum",    "percent", "freq",       "freq")
        }

        var_tab <- var_tab |>
            data.table::setcolorder(c("fused_vars", freq_columns))

        # Get the maximum width of the provided variable names to determine the width
        # of the first column.
        first_column_width <- max(nchar(variable), nchar("total"))

        first_column_width <- max(first_column_width,
                                  nchar(stats::na.omit(var_tab[["fused_vars"]])))

        # Set header row formatting. Loop through all header columns and give
        # each column the optimal width.
        column_widths         <- get_column_width(var_tab, freq_columns, decimals = 3)
        header_top_columns    <- c()
        header_bottom_columns <- c()

        var_tab <- var_tab |> collapse::frename("pct_group" = "var_pct_group")

        for (i in seq_len(length(freq_bottom_columns))){
            header_top_columns <- c(header_top_columns,
                                    format(freq_top_columns[i],
                                           width   = column_widths[i],
                                           justify = "right"))

            header_bottom_columns <- c(header_bottom_columns,
                                       format(freq_bottom_columns[i],
                                              width   = column_widths[i],
                                              justify = "right"))
        }

        # Put together the first two rows of the table as complete table header
        header_top_row <- paste0(format("",
                                        width   = first_column_width,
                                        justify = "left"), "  |  ",
                                 paste(header_top_columns,    collapse = "  "), "\n")

        header_row <- paste0(header_top_row,
                             format(variable,
                                    width   = first_column_width,
                                    justify = "left"), "  |  ",
                             paste(header_bottom_columns, collapse = "  "))

        complete_header <- c(header_row,
                             strrep("-", nchar(header_top_row)))

        # Format table column by column. Basically concatenate first column text and
        # corresponding values together while keeping the individual maximum column
        # width in mind.
        first_column <- paste0(format(var_tab[["fused_vars"]],
                                      width   = first_column_width,
                                      justify = "left"), "  |  ")

        # Loop through stat columns and format each number in a readable way
        formatted_cols <- vector("list", length(freq_columns))

        for (column in seq_len(length(freq_columns))){
            # Weighted sums
            if (column %in% c(1, 3)){
                formatted_cols[[column]] <- format_number(var_tab[column + 1],
                                                          width    = column_widths[column],
                                                          stat     = names(var_tab[column + 1]),
                                                          decimals = 3)
            }
            # Percentages and Unweighted freq
            else{
                formatted_cols[[column]] <- format_number(var_tab[column + 1],
                                                          width = column_widths[column],
                                                          stat  = names(var_tab[column + 1]))
            }
        }

        # Convert list to matrix
        formatted_matrix <- do.call(cbind, formatted_cols)

        # Convert matrix to single formatted rows
        all_rows <- paste0(first_column,
                           apply(formatted_matrix, 1, paste, collapse = "  "))

        # If no multilabel formats are applied separate total row at the bottom from the rest
        if (!is_multilabel(formats, variable)){
            rows <- length(all_rows)

            all_rows <- c(all_rows[1:(rows - 1)],
                          strrep("-", nchar(header_top_row)),
                          all_rows[rows])
        }
        # If multilabel formats are applied remove total row
        else{
            all_rows <- all_rows[-length(all_rows)]

            # Only do this step with no by variables here otherwise the separate
            # by function down below will handle this message, so that it appears only
            # once and not for each loop.
            if (length(by) == 0){
                message(" ~ NOTE: The format for variable '", variable, "' is a multilabel.\n",
                        "         In this case cumulative results aren't computed properly.")
            }
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


#' Compute Cumulative Variables
#'
#' @description
#' Compute separate cumulative variables for frequencies, weighted sums and
#' percentages.
#'
#' @param freq_tab The data frame which contains the information for this frequency
#' table.
#' @param variabls The currently computed variable.
#' @param formats A list in which is specified which formats should be applied to
#' which variable.
#'
#' @return
#' Returns a data frame with added cumulative variables.
#'
#' @noRd
compute_cumulative <- function(freq_tab,
                               variable,
                               formats){
    # Sort total to bottom if no by variables are specified
    if (!"BY" %in% names(freq_tab)){
        # Reduce data frame to the necessary rows
        var_tab <- freq_tab |> collapse::fsubset(TYPE %in% c("total", variable))

        # Generate cumulative variables first to have a complete data frame
        var_tab <- var_tab |> data.table::setorder(-"DEPTH")
    }
    # Compute new total if by variables are specified because otherwise the grand total would be
    # used for every single expression.
    else{
        total_temp <- freq_tab |>
            collapse::fsubset(TYPE %in% variable) |>
            collapse::fsummarise(across(c("var_sum", "var_pct_group", "var_freq"), collapse::fsum))

        var_tab <- data.table::rbindlist(list(freq_tab, total_temp), use.names = TRUE, fill = TRUE)
        var_tab <- var_tab |>
            collapse::fsubset(TYPE %in% c(variable, NA))
    }

    var_tab[["fused_vars"]][nrow(var_tab)] <- "total"

    var_tab[["var_cum_sum"]]  <- collapse::fcumsum(var_tab[["var_sum"]])
    var_tab[["var_cum_pct"]]  <- collapse::fcumsum(var_tab[["var_pct_group"]])
    var_tab[["var_cum_freq"]] <- collapse::fcumsum(var_tab[["var_freq"]])

    # Erase total cumulative values
    var_tab[["var_cum_sum"]][nrow(var_tab)]  <- NA
    var_tab[["var_cum_pct"]][nrow(var_tab)]  <- NA
    var_tab[["var_cum_freq"]][nrow(var_tab)] <- NA

    # Order columns in order of freq table
    var_tab |> collapse::frename("var_pct_group" = "pct_group")
}

###############################################################################
# Format freq table for excel output
###############################################################################
#' Format Frequency Table Output (Excel Based)
#'
#' @description
#' Format a frequency table for each variable which shows the counts, sums and
#' percentages per variable expression.
#'
#' @param wb An already created workbook to add more sheets to.
#' @param freq_tab The data frame which contains the information for this frequency
#' table.
#' @param variables The variables that should appear in the table.
#' @param formats A list in which is specified which formats should be applied to
#' which variable.
#' @param by Separate the frequency output by the expressions of the provided variables.
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
format_freq_excel <- function(wb,
                              freq_tab,
                              variables,
                              formats,
                              by,
                              titles,
                              footnotes,
                              style,
                              output,
                              by_info = NULL,
                              index   = NULL,
                              monitor_df){
    # Generate a separate frequency table for each variable
    for (variable in variables){
        monitor_df <- monitor_df |> monitor_start("Excel prepare (freq)", "Format freq")

        # Compute additional stats
        var_tab <- compute_cumulative(freq_tab, variable, formats)

        # Round values according to style options
        var_tab <- var_tab |>
            collapse::fmutate(var_sum     = round(var_sum,     style[["number_formats"]][["sum_decimals"]]),
                              var_cum_sum = round(var_cum_sum, style[["number_formats"]][["sum_decimals"]]),
                              pct_group   = round(pct_group,   style[["number_formats"]][["pct_decimals"]]),
                              var_cum_pct = round(var_cum_pct, style[["number_formats"]][["pct_decimals"]]))

        # If no weight variable was applied
        if (identical(var_tab[["var_sum"]], var_tab[["var_freq"]])){
            freq_columns <- c("var_freq", "pct_group", "var_cum_freq",
                              "var_cum_pct")

            freq_col_labels <- c("freq", "percent", "cumulative freq", "cumulative percent")

            by_info_col <- style[["start_column"]] + 6
        }
        # If weight variable was applied
        else{
            freq_columns <- c("var_sum", "pct_group", "var_cum_sum",
                              "var_cum_pct", "var_freq", "var_cum_freq")

            freq_col_labels <- c("sum", "percent", "cumulative sum",
                                 "cumulative percent", "freq", "cumulative freq")

            by_info_col <- style[["start_column"]] + 8
        }

        # If multilabel, remove automatically computed total observation.
        # Only do this step with no by variables here otherwise the separate
        # by function down below will handle this message, so that it appears only
        # once and not for each loop.
        if (is_multilabel(formats, variable)){
            if (length(by) == 0){
                message(" ~ NOTE: The format for variable '", variable, "' is a multilabel.
         In this case cumulative results aren't computed properly.")

                var_tab <- var_tab |>
                    collapse::fsubset(TYPE != "total")
            }
        }

        var_tab <- var_tab |>
            keep("fused_vars", freq_columns, order_vars = TRUE)

        names(var_tab) <- c(variable, freq_col_labels)

        # Get table ranges
        freq_ranges <- get_freq_tab_ranges(var_tab, titles, footnotes, style)

        # If function is called with by variables the sheet names have to be differentiated
        # and by info has to be written besides the table.
        if (!is.null(by_info)){
            wb$add_worksheet(paste0(variable, index),
                                    grid_lines = style[["grid_lines"]])
            wb$add_data(x         = by_info,
                        start_col = by_info_col,
                        start_row = style[["start_row"]])
        }
        else{
            wb$add_worksheet(variable, grid_lines = style[["grid_lines"]])
        }

        # Format titles and footnotes if there are any
        monitor_df <- monitor_df |> monitor_next("Excel titles/footnotes (freq)", "Format freq")
        wb <- wb |>
            format_titles_foot_excel(titles, footnotes, freq_ranges, style, output)

        # Add table data and format according to style options
        monitor_df <- monitor_df |> monitor_next("Excel data (freq)", "Format freq")

        wb$add_data(x          = var_tab,
                    start_col  = style[["start_column"]],
                    start_row  = freq_ranges[["header.row"]],
                    na.strings = style[["na_symbol"]])

        # Only do the formatting when user specified it. With the excel_nostyle
        # option this whole part gets omitted to get a very quick unformatted
        # excel output.
        if (output == "excel"){
            monitor_df <- monitor_df |> monitor_next("Excel cell styles (freq)", "Format freq")
            wb <- wb |> handle_cell_styles(freq_ranges, style)

            # Set up inner table number formats
            col_index <- 1
            col_types <- c("sum", "pct", "sum", "pct", "freq", "freq")

            if (!"sum" %in% names(var_tab)){
                col_types <- c("freq", "pct", "freq", "pct")
            }

            for (type in col_types){
                wb$add_cell_style(dims                = freq_ranges[[paste0("freq_col_ranges", col_index)]],
                                  apply_number_format = TRUE,
                                  num_fmt_id          = wb$styles_mgr$get_numfmt_id(paste0(type, "_numfmt")))

                col_index <- col_index + 1
            }

            # Adjust table dimensions
            monitor_df <- monitor_df |> monitor_next("Excel widths/heights (freq)", "Format freq")
            wb <- wb |> handle_col_row_dimensions(freq_ranges,
                                                  ncol(var_tab) + (style[["start_column"]] - 1),
                                                  nrow(var_tab) + (style[["start_row"]] - 1),
                                                  style)

            wb <- wb |> handle_auto_dimensions(freq_ranges,
                                               style)

            wb <- wb |> handle_header_table_dim(freq_ranges,
                                                style)

            wb$add_ignore_error(dims = freq_ranges[["cat_col_range"]], number_stored_as_text = TRUE)

            wb$add_named_region(dims = freq_ranges[["whole_tab_range"]], name = "table", local_sheet = TRUE)
            wb$add_named_region(dims = freq_ranges[["table_range"]],     name = "data",  local_sheet = TRUE)
        }

        monitor_df <- monitor_df |> monitor_end()
    }

    # Return workbook
    list(wb, monitor_df)
}

###############################################################################
# Format grouped by tables for console output
###############################################################################
#' Format Frequency Table Output with by Variables (Text Based)
#'
#' @description
#' Format a frequency table for each variable which shows the counts, sums and
#' percentages per variable expression.
#'
#' @param mean_tab The data frame which contains the information for this summarising
#' table.
#' @param freq_tab The data frame which contains the information for this frequency
#' table.
#' @param variables The variables that should appear in the table.
#' @param mean_columns The column names from the data frames that should appear
#' in the table.
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
format_by_text <- function(mean_tab,
                           freq_tab,
                           variables,
                           mean_columns,
                           formats,
                           by,
                           titles,
                           footnotes,
                           na.rm){
    # Print message if multilabels are applied
    for (variable in variables){
        if (is_multilabel(formats, variable)){
            message(" ~ NOTE: The format for variable '", variable, "' is a multilabel.\n",
                    "         In this case cumulative results aren't computed properly.")
        }
    }

    complete_tabs <- c()

    # Loop through all by variables
    for (by_var in by){
        # Select by variables one by one
        mean_by <- mean_tab |>
            collapse::fsubset(mean_tab[["TYPE"]] == by_var)

        freq_by <- freq_tab |>
            collapse::fsubset(freq_tab[["BY"]] == "total" | freq_tab[["BY"]] == by_var)

        # Extract unique values
        if (anyNA(freq_by[["by_vars"]])){
            values <- c(unique(stats::na.omit(freq_by[["by_vars"]]))[-1], NA)
        }
        else{
            values <- unique(freq_by[["by_vars"]])
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
                mean_temp <- mean_by |>
                    collapse::fsubset(mean_by[["by_vars"]] == value)

                freq_temp <- freq_by |>
                    collapse::fsubset(freq_by[["by_vars"]] == "total" | freq_by[["by_vars"]] == value)
            }
            else{
                mean_temp <- mean_by |>
                    collapse::fsubset(is.na(mean_by[["by_vars"]]))

                freq_temp <- freq_by |>
                    collapse::fsubset(freq_by[["by_vars"]] == "total" | is.na(freq_by[["by_vars"]]))
            }

            # Generate mean table as normal but base is filtered data frame
            current_mean <- format_mean_text(mean_temp,
                                             variables,
                                             mean_columns)

            # Generate frequency tables as normal but base is filtered data frame
            current_freq <- format_freq_text(freq_temp,
                                             variables,
                                             formats,
                                             by,
                                             titles,
                                             footnotes)

            # Output formatted result
            complete_tabs <- c(complete_tabs, complete_header, current_mean, current_freq)
        }
    }

    # Output formatted result
    complete_tabs
}

###############################################################################
# Format grouped by tables for excel output
###############################################################################
#' Format Frequency Table Output with by Variables (Excel Based)
#'
#' @description
#' Format a frequency table for each variable which shows the counts, sums and
#' percentages per variable expression.
#'
#' @param mean_tab The data frame which contains the information for this summarising
#' table.
#' @param freq_tab The data frame which contains the information for this frequency
#' table.
#' @param variables The variables that should appear in the table.
#' @param mean_columns The column names from the data frames that should appear
#' in the table.
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
format_by_excel <- function(mean_tab,
                            freq_tab,
                            variables,
                            mean_columns,
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
    for (variable in variables){
        if (is_multilabel(formats, variable)){
            message(" ~ NOTE: The format for variable '", variable, "' is a multilabel.\n",
                    "         In this case cumulative results aren't computed properly.")
        }
    }

    index <- 1

    # Loop through all by variables
    for (by_var in by){
        monitor_df <- monitor_df |> monitor_next(paste0("Excel prepare (", by_var, ")"), "Format by")

        # Select by variables one by one
        mean_by <- mean_tab |>
            collapse::fsubset(mean_tab[["TYPE"]] == by_var)

        freq_by <- freq_tab |>
            collapse::fsubset(freq_tab[["BY"]] == "total" | freq_tab[["BY"]] == by_var)

        # Extract unique values
        if (anyNA(freq_by[["by_vars"]])){
            values <- c(unique(stats::na.omit(freq_by[["by_vars"]]))[-1], NA)
        }
        else{
            values <- unique(freq_by[["by_vars"]])
        }

        # Loop through all unique values to generate frequency tables per expression
        for (value in values){
            # In case NAs are removed
            if (is.na(value) && na.rm){
                next
            }

            message("   + ", paste0(by_var, " = ", value))

            monitor_df <- monitor_df |> monitor_next(paste0("Excel mean (", by_var, "_", value, ")"), "Format by")

            # Put additional by info together with the information which by variable
            # and which value is currently filtered.
            by_info <- paste0(by_var, " = ", value)

            # Filter table by current by variable and value
            if (!is.na(value)){
                mean_temp <- mean_by |>
                    collapse::fsubset(mean_by[["by_vars"]] == value)

                freq_temp <- freq_by |>
                    collapse::fsubset(freq_by[["by_vars"]] == "total" | freq_by[["by_vars"]] == value)
            }
            else{
                mean_temp <- mean_by |>
                    collapse::fsubset(is.na(mean_by[["by_vars"]]))

                freq_temp <- freq_by |>
                    collapse::fsubset(freq_by[["by_vars"]] == "total" | is.na(freq_by[["by_vars"]]))
            }

            # Generate mean table as normal but base is filtered data frame
            if (nrow(mean_temp) > 0){
                wb_list <- format_mean_excel(mean_temp,
                                             mean_columns,
                                             style,
                                             output,
                                             wb,
                                             index,
                                             NULL)

                wb_list[[1]]$add_data(x         = by_info,
                                      start_col = style[["start_column"]] + 8,
                                      start_row = style[["start_row"]])
            }
            else{
                wb_list      <- list()
                wb_list[[1]] <- wb
            }

            monitor_df <- monitor_df |> monitor_next(paste0("Excel freq (", by_var, "_", value, ")"), "Format by")

            # Generate frequency tables as normal but base is filtered data frame
            wb_list <- format_freq_excel(wb_list[[1]],
                                         freq_temp,
                                         variables,
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
        }
    }

    monitor_df <- monitor_df |> monitor_end()

    # Return workbook
    list(wb, monitor_df)
}
