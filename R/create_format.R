#' Create Format Container
#'
#' @name formats
#'
#' @description
#' Create a format container which stores discrete or interval values
#' with corresponding labels that can be applied by using [summarise_plus()].
#'
#' Create a format container independent from any data frame. Define which values
#' should be recoded into which new categories, if the format is applied to
#' a variable in a data frame.
#' It is possible to assign a single value to multiple new categories to create
#' a multilabel.
#' It is recommended to let format names end with a dot to make them stand out.
#'
#' @param ... List all the desired recodings/recoding ranges. Every element contains a text for
#' the new category name and the values/value ranges which should be recoded into this new category.
#'
#' @details
#' The concept of having formats as molds or stencils to put the data through, is inspired by
#' 'SAS' formats. In 'SAS' formats are defined with the procedure Proc Formats, which is adapted
#' with [discrete_format()] and [interval_format()]. Here you can define, which values
#' should be transferred into which result categories. This is completely detached from the data
#' your working with.
#'
#' The great thing about this is, that one can not only label and recode values, but one can also
#' define so called multilabels. Meaning, one original value can be transferred into multiple
#' result categories.
#'
#' A cell in a data frame can only hold one distinct value, which is normally a good thing.
#' But let's say you want to convert single ages into age categories. The age "3" for example
#' could go into the category "under 6", but also in "under 12", "under 18" and "total". Normally
#' you would compute additional variables, which hold the different categorizations, or you could also
#' double up the observations for each category. Both ways would just bloat up the data frame and
#' cost additional memory, particularly if you work with big data sets.
#'
#' With these format containers, you just keep a small reference of original values and result
#' categories. Formats and data find their way together only just before computing the results,
#' meaning the original data frame can be passed into a function capable of handling formats (see below),
#' without any data transformation beforehand. You just tell the function which format should
#' be applied to which variable. That's it. The function handles the rest and outputs all the
#' desired categories.
#'
#' This method is very memory efficient, readable and user friendly for creating larger and more
#' complex outputs at the same time.
#'
#' @return
#' Returns a data table which contains the values/value ranges with the corresponding labels
#'
#' @seealso
#' Functions that can handle formats: [summarise_plus()], [frequencies()], [crosstabs()],
#' [any_table()], [recode_multi()], [transpose_plus()], [sort_plus()].
#'
#' @examples
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
#' education. <- discrete_format(
#'     "Total"            = c("low", "middle", "high"),
#'     "low education"    = "low",
#'     "middle education" = "middle",
#'     "high education"   = "high")
#'
#' income. <- interval_format(
#'     "Total"              = 0:99999,
#'     "below 500"          = 0:499,
#'     "500 to under 1000"  = 500:999,
#'     "1000 to under 2000" = 1000:1999,
#'     "2000 and more"      = 2000:99999)
#'
#' state. <- discrete_format(
#'     "Germany"                       = 1:16,
#'     "Schleswig-Holstein"            = 1,
#'     "Hamburg"                       = 2,
#'     "Lower Saxony"                  = 3,
#'     "Bremen"                        = 4,
#'     "North Rhine-Westphalia"        = 5,
#'     "Hesse"                         = 6,
#'     "Rhineland-Palatinate"          = 7,
#'     "Baden-Württemberg"             = 8,
#'     "Bavaria"                       = 9,
#'     "Saarland"                      = 10,
#'     "West"                          = 1:10,
#'     "Berlin"                        = 11,
#'     "Brandenburg"                   = 12,
#'     "Mecklenburg-Western Pomerania" = 13,
#'     "Saxony"                        = 14,
#'     "Saxony-Anhalt"                 = 15,
#'     "Thuringia"                     = 16,
#'     "East"                          = 11:16)
#'
#' # With discrete formats you can specify the keyword "other" to
#' # catch any other value not covered by the explicitly specified values.
#' age. <- discrete_format(
#'     "under 18"       = 0:17,
#'     "18 to under 25" = 18:24,
#'     "25 to under 55" = "other")
#'
#' # With interval formats you can also use the keywords "low" and "high" to
#' # catch everything from the lowest to the highest values, in case one doesn't
#' # know exactly what the lowest and highest values are.
#' income. <- interval_format(
#'     "Total"              = c("low", "high"),
#'     "below 500"          = c("low", 499),
#'     "500 to under 1000"  = 500:999,
#'     "1000 to under 2000" = 1000:1999,
#'     "2000 and more"      = c(2000, "high"))
#'
#' @rdname formats
#'
#' @keywords internal
NULL


#' @rdname formats
#'
#' @export
discrete_format <- function(...){
    # Measure the time
    start_time <- Sys.time()

    # Translate ... into separately controllable arguments
    list_of_groupings <- tryCatch({
        # Force evaluation to see if it exists
        list(...)
    }, error = function(e){
        # Evaluation failed
        NULL
    })

    if (is.null(list_of_groupings)){
        message(" X ERROR: Formats must be provided in the form [target category] = [original expressions].\n",
                "          Creating format will be aborted.")
        return(invisible(NULL))
    }

    # Check if any element has a missing name. If that is the case the list wasn't provided
    # in the correct way.
    labels <- names(list_of_groupings)

    if (is.null(labels) || any(is.na(labels) | labels == "")){
        message(" X ERROR: Formats must be provided in the form [target category] = [original expressions].\n",
                "          Creating format will be aborted.")
        return(invisible(NULL))
    }

    unwrap_grouping <- function(single_label){
        data.table::data.table(value = list_of_groupings[[single_label]],
                   label = single_label,
                   stringsAsFactors = FALSE)
    }

    unwrap_all_groupings <- lapply(names(list_of_groupings), unwrap_grouping)

    # Flatten to long format
    unwrapped_format <- data.table::rbindlist(unwrap_all_groupings)

    # If label column is all numeric then convert it to numeric
    unwrapped_format <- unwrapped_format |> convert_numeric("label")

    # Convert "other" keyword to integer max. This should be a value no one would pick normally.
    if ("other" %in% tolower(unwrapped_format[["value"]])){
        unwrapped_format[["value"]] <- sub("other", .Machine[["integer.max"]], tolower(unwrapped_format[["value"]]))

        # If value column is all numeric then convert it to numeric
        unwrapped_format <- unwrapped_format |> convert_numeric("value")
    }

    end_time <- round(difftime(Sys.time(), start_time, units = "secs"), 3)
    message("- - - 'discrete_format' execution time: ", end_time, " seconds")

    unwrapped_format
}


#' @rdname formats
#'
#' @export
interval_format <- function(...){
    # Measure the time
    start_time <- Sys.time()

    # Translate ... into separately controllable arguments
    ranges <- tryCatch({
        # Force evaluation to see if it exists
        list(...)
    }, error = function(e){
        # Evaluation failed
        NULL
    })

    if (is.null(ranges)){
        message(" X ERROR: Formats must be provided in the form [target category] = [original expressions].\n",
                "          Creating format will be aborted.")
        return(invisible(NULL))
    }

    # Check if any element has a missing name. If that is the case the list wasn't provided
    # in the correct way.
    labels <- names(ranges)

    if (is.null(labels) || any(is.na(labels) | labels == "")){
        message(" X ERROR: Formats must be provided in the form [target category] = [original expressions].\n",
                "          Creating format will be aborted.")
        return(invisible(NULL))
    }

    # Get from - to value as vectors
    from <- sapply(ranges, function(x) min(x, na.rm = TRUE))
    to   <- sapply(ranges, function(x) max(x, na.rm = TRUE))

    # Insert pseudo low and high numbers for keywords
    if (is.character(to)){
        # First check if there are any other words than low and high in the format. If yes, abort.
        if (!any(c("low", "high") %in% tolower(to))){
            message(" X ERROR: Unknown keyword found. Creating interval format will be aborted.")
            return(invisible(NULL))
        }

        # Low always ends up in "to", because it is a character value and comes alphabetically after high
        if ("low" %in% tolower(to)){
            # Swap the real not "low" value to "to" and insert a pseudo low number in "from"
            to[tolower(to) == "low"]           <- from[tolower(to) == "low"]
            from[tolower(from) == tolower(to)] <- -.Machine[["double.xmax"]]

            from <- as.numeric(from)
        }

        # If there is the "high" keyword, at this point it will always be in "to" either the same way as low,
        # just because it is character or because it is swapped here by the low code above.
        if ("high" %in% tolower(to)){
            # Enter a pseudo high number for "to"
            to[tolower(to) == "high"] <- .Machine[["double.xmax"]]

            from <- as.numeric(from)
        }

        to <- as.numeric(to)
    }

    end_time <- round(difftime(Sys.time(), start_time, units = "secs"), 3)
    message("- - - 'interval_format' execution time: ", end_time, " seconds")

    # Put everything together in a data frame
    data.table::data.table(from  = from,
                           to    = to,
                           label = labels) |> convert_numeric("label")
}


#' Evaluate Formats
#'
#' @description
#' Get the list of formats with their corresponding data frames.
#'
#' @param formats_list A list containing format names.
#'
#' @return
#' Returns a list of format data frames.
#'
#' @noRd
evaluate_formats <- function(formats_list){
    if (length(formats_list) == 0){
        return(c())
    }

    # Fetch all the provided formats and get the original data frames and store them into a list
    formats <- lapply(formats_list, function(format){
        if (length(format) > 1){
            message(" ! WARNING: Formats not passed correctly. Create format object first and then\n",
                    "            pass it to the function parameter like: list(my_variable = my_format)")
            return(c())
        }
        dynGet(as.character(format), ifnotfound = NULL, inherits = TRUE)
    })

    names(formats) <- names(formats_list)

    formats
}


#' Check If Format List Is Provided
#'
#' @description
#' Check if the provided list is a list of data frames.
#'
#' @param formats_list A list containing format data frames.
#'
#' @return
#' Returns TRUE or FALSE.
#'
#' @noRd
is_list_of_dfs <- function(formats_list){
    # This errors if formats_list can't be evaluated because of a missing object.
    # In this case return FALSE so that the function using this check can evaluate to NULL.
    tryCatch({
        (is.list(formats_list)
         && length(formats_list) > 0
         && all(vapply(formats_list, data.table::is.data.table, logical(1))))
    }, error = function(e) {
        FALSE
    })

}


#' Expand A List Of Formats To Generate All Possible Combinations
#'
#' @description
#' Generates a data frame which contains all nested combinations of the provided
#' format labels.
#'
#' @param ... A list containing format data frames.
#' @param names Custom variable names for the newly generated data frame.
#'
#' @return
#' Returns a data frames with all format label combinations.
#'
#' @examples
#' # Example formats
#' sex. <- discrete_format(
#'     "Total"  = 1:2,
#'     "Male"   = 1,
#'     "Female" = 2)
#'
#' age. <- discrete_format(
#'     "Total"          = 0:100,
#'     "under 18"       = 0:17,
#'     "18 to under 25" = 18:24,
#'     "25 to under 55" = 25:54,
#'     "55 to under 65" = 55:64,
#'     "65 and older"   = 65:100)
#'
#' # Expand formats
#' expand_df <- expand_formats(sex., age.,
#'                             names = c("sex", "age"))
#'
#' @export
expand_formats <- function(..., names = NULL){
    # Measure the time
    start_time <- Sys.time()

    # Translate ... into a list if possible
    format_list <- tryCatch({
        # Force evaluation to see if it exists
        formats      <- list(...)
        format_names <- names(formats)

        # If ... is passed as a list instead of single format data frames, it has
        # to be unwrapped first. Otherwise there would be a list inside a list.
        if (length(formats) == 1 && is.list(formats[[1]])){
            if (is.null(format_names) || identical(format_names, "")){
                formats <- formats[[1]]
            }

            # If there is only one format data frame provided there is nothing to expand,
            # so the function just returns the unique values of the label column.
            if (data.table::is.data.table(formats)){
                return(formats |>
                           keep("label") |>
                           collapse::funique())
            }
        }

        formats
    }, error = function(e) {
        # Evaluation failed
        NULL
    })

    if (is.null(format_list)){
        message(" X ERROR: Formats must be provided as data frames or as a list of data frames.\n",
                "          Format expansion will be aborted.")
        return(invisible(NULL))
    }

    # Create a list of format data frames which only contain the "label" column
    format_list <- lapply(format_list, function(format){
            if ("label" %in% names(format)){
                format[["label"]]
            }
            else{
                NULL
            }
        })

    # Abort if there is a missing "label" column in one of the data frames. Normally I could drop this
    # and go on, but if used in other functions this could lead to follow up errors. So abort to be safe.
    if (length(format_list[sapply(format_list, is.null)]) > 0){
        message(" X ERROR: A data frame is missing the 'label' column. This function is especially for expanding formats created\n",
                "          with 'discrete_format' and 'interval_format'. If you want to include another non format data frame, you have\n",
                "          to name the desired expansion column 'label'. Format expansion will be aborted.")

        return(invisible(NULL))
    }

    # Generate the cartesian product of all given labels to get all possible
    # combinations of categories.
    expand_df <- do.call(data.table::CJ, c(format_list,
                                           list(unique = TRUE, sorted = FALSE)))

    # Name the columns after the given names
    if (!is.null(names)){
        names(expand_df) <- names
    }

    # Convert to factor so that they are sorted in format order
    var_names <- names(expand_df)
    expand_df <- expand_df |> convert_factor(var_names)

    end_time <- round(difftime(Sys.time(), start_time, units = "secs"), 3)
    message("- - - 'expand_formats' execution time: ", end_time, " seconds")

    expand_df
}
