#' Check If Variable Length Throws an Error
#'
#' @description
#' This try catch check is used for error handling in some functions. It enables a
#' function to react to a none existing variable in a way, that let's the code flow
#' instead of stopping with an error.
#'
#' @param variable The variable whose length should be checked.
#'
#' @return
#' Returns the result of a try catch check.
#'
#' @noRd
is_error <- function(variable) {
    # Check if the variable can be evaluated or if it throws an error
    result <- tryCatch({
        length(variable) == 1
        FALSE
    }, error = function(e){
        TRUE
    })

    result
}


#' Reorder Variable Combination Alphabetically
#'
#' @description
#' [summarise_plus()] can create multiple variable combinations from the grouping
#' variables. The order in which the TYPE variable is generated is always the order
#' which the user provided. In [any_table()] different column variable combinations
#' have to be joined back to the same row variable combinations. The problem is, that
#' the order of row variables inside the loop can differ from the order in the TYPE
#' variable. Therefor this function can order this TYPE alphabetically, to make a
#' join possible.
#'
#' @param combination A combination of variables in the form "var1 + var2 + var3 + ...".
#'
#' @return
#' Returns an alphabetically sorted combination of variables.
#'
#' @noRd
reorder_combination <- function(combination){
    combination <- gsub(" ", "", combination)

    sapply(strsplit(combination, "\\+"), function(variables){
        paste(sort(variables), collapse = "+")
    })
}


#' Check if Format is a Multilabel
#'
#' @description
#' Check for a multilabel, meaning if a format can produce multiple new categories
#' from a single original value.
#'
#' @param format_list The list of variables with corresponding formats.
#' @param variable The variable whose format should be checked.
#'
#' @return
#' Returns TRUE or FALSE depending on the result.
#'
#' @noRd
is_multilabel <- function(format_list, variable){
    if (length(format_list) == 0){
        return(FALSE)
    }

    if (!variable %in% names(format_list)){
        return(FALSE)
    }

    # Select the current format
    format <- format_list[[variable]]

    # In case of discrete format a simple check for duplicated values is sufficient
    if (names(format)[1] == "value" & any(duplicated(format[["value"]]))){
        return(TRUE)
    }

    # In case of an interval format the format has to be joined with itself to
    # see whether the result produces more observations than the format had before.
    if ("from" %in% names(format)){
        format_temp <- format |> collapse::frename("label" = "label_check")

        format_dt      <- data.table::copy(format)
        format_temp_dt <- data.table::copy(format_temp)

        data.table::setkey(format_dt,      from, to)
        data.table::setkey(format_temp_dt, from, to)

        # Merge data frame with format by range
        format_check <- data.table::foverlaps(format_dt, format_temp_dt,
                                              by.x = c("from", "to"),
                                              by.y = c("from", "to"))

        if (nrow(format_check) > nrow(format)){
            return(TRUE)
        }
    }

    # No multilabel detected
    return(FALSE)
}


#' Order Statistics Alternating
#'
#' @description
#' In [any_table()] the table columns can be ordered in different ways. Interleaved
#' means that columns of different statistics appear alternated in the table.
#'
#' @param data_frame The data frame which contains the colums to be ordered.
#' @param patterns The statistics in the order they should be interleaved.
#'
#' @return
#' Returns a data frame with interleaved ordered statistics columns.
#'
#' @noRd
order_interleaved <- function(data_frame, patterns) {
    # Find columns for each pattern
    columns <- lapply(patterns, function(pattern){
        grep(pattern, names(data_frame), value = TRUE)
    })

    # Interleave columns. Pad shorter groups with NA and bind alternating.
    max_length  <- max(lengths(columns))

    interleaved <- as.vector(
        do.call(rbind, lapply(columns, function(column){
            c(column, rep(NA, max_length - length(column)))
        }))
    )
    interleaved <- interleaved[!is.na(interleaved)]

    # Order columns
    data_frame |> data.table::setcolorder(interleaved, after = ncol(data_frame))
}
