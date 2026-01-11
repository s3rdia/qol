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
    if (names(format)[1] == "value" && any(duplicated(format[["value"]]))){
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

        if (collapse::fnrow(format_check) > collapse::fnrow(format)){
            return(TRUE)
        }
    }

    # No multilabel detected
    FALSE
}


#' Order Statistics Alternating
#'
#' @description
#' In [any_table()] the table columns can be ordered in different ways. Interleaved
#' means that columns of different statistics appear alternated in the table.
#'
#' @param data_frame The data frame which contains the columns to be ordered.
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


#' Check If Data Frame Is Already Summarised
#'
#' @description
#' [any_table()] can be used to tabulate individual data but also pre summarised
#' data. This check is used to determine which route to use.
#'
#' @param data_frame The data frame which contains the columns to be checked.
#' @param group_vars The grouping variables which potentially form unique combinations.
#'
#' @return
#' Returns TRUE or FALSE depending on the result.
#'
#' @noRd
is_pre_summed <- function(data_frame, group_vars){
    !collapse::any_duplicated(data_frame[group_vars])
}


#' Unlist Variables
#'
#' @description
#' Unlists variables concatenated by + signs. This is used to get the single variables
#' in e.g. [any_table()], where rows and columns are passed like c("sex", "sex + age").
#'
#' @param var_names A character vector of variable names.
#'
#' @return
#' Character vector.
#'
#' @noRd
unlist_variables <- function(var_names){
    # This errors if a value without quotation marks is passed. In this case return NULL
    # so that the function using this check can abort.
    tryCatch({
        collapse::funique(trimws(unlist(strsplit(var_names, "\\+"))))
    }, error = function(e) {
        NULL
    })
}
