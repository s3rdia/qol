#' Get Detailed Summary About A Data Frame
#'
#' @description
#' Prints a summary of a data frames contents, including details such as variable names,
#' types, unique values, missings and min/max values. It also tells you the number of
#' observations and variables present in the data frame, memory usage and the number
#' of duplicate observations.
#'
#' @param data_frame The data frame to get the content information from.
#' @param output The following output formats are available: console (default) or text.
#' @param monitor FALSE by default. If TRUE, outputs two charts to visualize the
#' functions time consumption.
#'
#' @details
#' [content_report()] is based on the 'SAS' procedure Proc Contents, which provides
#' a summary of global information one one hand like number of observations and variables among
#' many others and on the other hand shows per variable information like type and length.
#'
#' 'R' doesn't store the same information in a data frame like 'SAS', but there are many
#' useful information to get a quick overview of a data frame. With this function you don't
#' need to look at each variable individually. You can simply run it over a data frame and
#' get values for: number of unique values, missing values (absolute and relative), min and
#' max value as well as the top value.
#'
#' @return
#' Returns a list containing the global information as well as a data table containing
#' the per variable information.
#'
#' @examples
#' # Example data frame
#' my_data <- dummy_data(100)
#'
#' content_report(my_data)
#'
#' @export
content_report <- function(data_frame,
                           output  = "console",
                           monitor = FALSE){
    # Measure the time
    start_time <- Sys.time()

    #-------------------------------------------------------------------------#
    monitor_df <- NULL |> monitor_start("Global information", "Global information")
    #-------------------------------------------------------------------------#

    ###########################################################################
    # Get global information
    ###########################################################################

    message(" > Collecting global information")

    # Get the variable types as a vector
    column_types <- collapse::vtypes(data_frame)

    # Collect global information
    n_obs  <- collapse::fnrow(data_frame)
    n_vars <- collapse::fncol(data_frame)

    global_info <- list(
        name           = deparse(substitute(data_frame)),
        type           = class(data_frame)[[1]],
        n_obs          = n_obs,
        n_vars         = n_vars,
        n_cells        = n_obs * n_vars,
        duplicate_rows = collapse::fsum(collapse::fduplicated(data_frame)),
        type_counts    = collapse::fnobs(column_types, g = column_types),
        memory_usage   = format(utils::object.size(data_frame), units = "auto"))

    ###########################################################################
    # Per variable information
    ###########################################################################

    #-------------------------------------------------------------------------#
    monitor_df <- monitor_df |> monitor_next("Type", "Per variable information")
    #-------------------------------------------------------------------------#

    message(" > Collecting per variable information")

    variable_report <- data.table::data.table(
        pos = 1:collapse::fncol(data_frame),
        name = names(data_frame),
        type = column_types
    )

    #-------------------------------------------------------------------------#
    monitor_df <- monitor_df |> monitor_next("Unique values", "Per variable information")
    #-------------------------------------------------------------------------#

    message("   + Number of unique values")

    variable_report[["unique"]] <- sapply(data_frame, data.table::uniqueN)

    #-------------------------------------------------------------------------#
    monitor_df <- monitor_df |> monitor_next("Missing values", "Per variable information")
    #-------------------------------------------------------------------------#

    message("   + Number of missing values")

    variable_report[["na_count"]] <- sapply(data_frame, function(variable) collapse::fsum(is.na(variable)))
    variable_report[["na_pct"]]   <- sapply(data_frame, function(variable) round(collapse::fsum(is.na(variable))/length(variable) * 100, 1))

    #-------------------------------------------------------------------------#
    monitor_df <- monitor_df |> monitor_next("Longest value", "Per variable information")
    #-------------------------------------------------------------------------#

    message("   + Longest value")

    variable_report[["longest"]] <- sapply(data_frame, get_max_length)

    #-------------------------------------------------------------------------#
    monitor_df <- monitor_df |> monitor_next("Min/Max value", "Per variable information")
    #-------------------------------------------------------------------------#

    message("   + Min and max value")

    variable_report[["min_val"]] <- sapply(data_frame, format_value, type_func = collapse::fmin, type = "min")
    variable_report[["max_val"]] <- sapply(data_frame, format_value, type_func = max, type = "max")

    #-------------------------------------------------------------------------#
    monitor_df <- monitor_df |> monitor_next("Top value", "Per variable information")
    #-------------------------------------------------------------------------#

    message("   + Top value")

    variable_report[["top_freq"]] <- sapply(data_frame, get_top_value)

    #-------------------------------------------------------------------------#
    monitor_df <- monitor_df |> monitor_next("Create report", "Create report")
    #-------------------------------------------------------------------------#

    # Concatenate global information into one character
    global_text <- c(strrep("#", 100),
                     "                                     DATA FRAME CONTENT REPORT",
                     strrep("#", 100),
                     "",
                     sprintf("%-15s: %s", "Data frame",     global_info[["name"]]),
                     sprintf("%-15s: %s", "Type",           global_info[["type"]]),
                     sprintf("%-15s: %d", "Observations",   global_info[["n_obs"]]),
                     sprintf("%-15s: %d", "Variables",      global_info[["n_vars"]]),
                     sprintf("%-15s: %d", "Cells",          global_info[["total_cells"]]),
                     sprintf("%-15s: %s", "Memory Usage",   global_info[["memory_usage"]]),
                     sprintf("%-15s: %d", "Duplicate Rows", global_info[["duplicate_rows"]]),

                    "\n--- Variable Type Counts ---\n",

                    utils::capture.output(global_info[["type_counts"]]),

                    "\n--- Variable Details ---\n\n")
    old <- options(max.print = .Machine$integer.max)
    on.exit(options(old), add = TRUE)

    # Print global information and data frame with per variable information
    if (output %in% c("console")){
        cat(paste(global_text, collapse = "\n"))
        print.data.frame(variable_report)
    }
    else if (output == "text"){
        # Capture texts to print in variables
        global_text   <- utils::capture.output(cat(paste(global_text, collapse = "\n")))
        variable_text <- utils::capture.output(print.data.frame(variable_report))

        # Write texts to file and open editor
        temp_file <- tempfile(fileext = ".txt")
        writeLines(c(global_text, variable_text), temp_file, sep = "\n")

        if (interactive()){
            file.show(temp_file)
        }
    }

    end_time <- round(difftime(Sys.time(), start_time, units = "secs"), 3)
    message("\n- - - 'content_report' execution time: ", end_time, " seconds\n")

    #-------------------------------------------------------------------------#
    monitor_df <- monitor_df |> monitor_end()
    monitor_df |> monitor_plot(draw_plot = monitor)
    #-------------------------------------------------------------------------#

    # Return report list
    invisible(list(global    = global_info,
                   variables = variable_report))
}


#' Get Maximum Length Of A Variable
#'
#' @description
#' Get the maximum length of all variable types.
#'
#' @param variable Variable from which to get the length of the longest element.
#'
#' @return
#' Returns a numeric value
#'
#' @noRd
get_max_length <- function(variable){
    if (!any(!is.na(variable))){
        return(0)
    }

    # Just analyze the top 1000 because otherwise this would take to long
    variable <- variable[1:collapse::fmin(1000, collapse::fnrow(variable))]

    # Process doubles first because it should be more common that even integers are
    # saved as doubles.
    if (is.double(variable)){
        # Get unique values to avoid redundant formatting
        unique_values <- round(variable, 5) |>
            collapse::funique(sort = FALSE) |>
            collapse::na_omit()

        # Convert to character to count digits and decimal places
        chars <- format(unique_values, digits = 20, nsmall = 0, scientific = FALSE, trim = TRUE)

        # Remove trailing zeros to only get decimals if there really are any
        chars <- sub("\\.0+$", "", chars)
        chars <- sub("(\\.[0-9]*[1-9])0+$", "\\1", chars)

        return(collapse::fmax(collapse::vlengths(chars)))
    }

    # Process integers
    if (is.integer(variable)){
        # The negative sign will be ignored
        variable <- abs(as.numeric(variable))

        # log10(0) below is not defined, therefore get rid of all zeros
        variable[variable == 0] <- 1

        return(collapse::fmax(floor(log10(variable)) + 1))
    }

    # Process characters
    if (is.character(variable)){
        return(collapse::fmax(collapse::vlengths(variable), na.rm = TRUE))
    }

    # Fallback
    collapse::fmax(collapse::vlengths(as.character(variable)), na.rm = TRUE)
}


#' Get Minimum Or Maximum Value Of A Variable
#'
#' @description
#' Get the minimum or maximum value of a variable. For character variables retrieves
#' the alphabetically first or last entry.
#'
#' @param variable Variable from which to get the minimum or maximum value.
#' @param type_func Enter minimum or maximum function.
#' @param type The function type as character.
#'
#' @return
#' Returns a character.
#'
#' @noRd
format_value <- function(variable, type_func, type){
    if (collapse::allNA(variable)){
        return(NA_character_)
    }

    # For character values sort unique values and get first or last element of
    # the resulting vector.
    if (is.character(variable)){
        variable <- variable |>
            collapse::na_omit() |>
            collapse::funique() |>
            sort()

        if (type == "min"){
            return(variable[1])
        }
        else{
            return(variable[length(variable)])
        }
    }

    # Otherwise for numeric value get min or max value and return as character
    value <- type_func(variable, na.rm = TRUE)

    if (is.numeric(value)){
        return(as.character(round(value, 5)))
    }

    as.character(value)
}


#' Get The Top Value In Terms of Frequency
#'
#' @description
#' Get the value with the most number of observations.
#'
#' @param variable Variable from which to get the value with the top frequency.
#'
#' @return
#' Returns a character.
#'
#' @noRd
get_top_value <- function(variable) {
    if (collapse::allNA(variable)){
        return("All NA")
    }

    # Round doubles, otherwise for doubles with decimals places this would result
    # in 0.0 % afterwards and would take forever
    if (is.double(variable)){
        variable <- round(variable, 3)
    }

    # Calculate frequencies
    counts <- collapse::fnobs(variable, g = variable)

    if (length(counts) == 0){
        return("NA")
    }

    # Check if all values are unique
    if (all(counts == 1) && length(variable) > 1){
        return("All unique")
    }

    # Evaluate top value percentage
    top_value <- names(counts)[which.max(counts)]
    top_freq  <- collapse::fmax(counts)
    pct       <- round((top_freq / length(variable)) * 100, 1)

    # Format percentage so that it can be displayed in a column with even width
    # despite uneven values.
    pct_formatted <- format(pct, width = 5, nsmall = 1, digits = 1)

    paste0(top_value, " (", pct_formatted, "%)")
}
