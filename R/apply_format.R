#' Apply Formats to a Data Frame
#'
#' @description
#' Used to apply formats to a data frame. Handles single labels with a simple join
#' and also multilabels by computing the cartesian product.
#'
#' @param data_frame The data frame to which formats should be joined.
#' @param formats A list in which is specified which formats should be applied
#' to which variables.
#' @param group_vars A vector containing all grouping variables.
#'
#' @return
#' Returns a data table to which a format data frame was joined.
#'
#' @noRd
apply_format <- function(data_frame, formats, group_vars = NULL){
    if (length(formats) == 0){
        return(data_frame)
    }

    temp_data <- data_frame
    arguments <- formats

    # Loop through all given variables and join each format with the data frame
    # at a time
    for (current_var in names(arguments)){
        format_df <- arguments[[current_var]]

        if (is.null(format_df)){
            message(" ! WARNING: Format for '", current_var, "' not found. Formatting for variable '", current_var, "' will be omitted.")
            next
        }

        # Security checks to ensure types are right
        if (!is.null(group_vars)){
            if (!current_var %in% group_vars){
                # No Warning because this exit is used to omit missing group variables
                next
            }
        }

        if (!current_var %in% names(temp_data)){
            message(paste0(" ! WARNING: Variable '", current_var, "' not found in the input data frame. No Format will be applied."))
            next
        }

        # Convert provided data frames to data table for speed
        if (!data.table::is.data.table(temp_data)){
            temp_data <- data.table::as.data.table(temp_data)
        }

        if (!data.table::is.data.table(format_df)){
            temp_data <- data.table::as.data.table(format_df)
        }

        # Look up variable names in format data frame to check whether it is an
        # interval or discrete format
        interval_variables <- c("from", "to")
        actual_variables <- names(format_df)[1:2]

        # In case of interval format
        if (identical(interval_variables, actual_variables)){
            # Separate NAs from rest of the data frame because the used join
            # can't handle them
            temp_na   <- temp_data[is.na(temp_data[[current_var]]), ]
            temp_data <- temp_data[!is.na(temp_data[[current_var]]), ]

            # Generate pseudo variables for range merging
            temp_data[["qol_from"]] <- temp_data[[current_var]]
            temp_data[["qol_to"]]   <- temp_data[[current_var]]

            data_frame[["qol_ID"]] <- seq_len(nrow(data_frame))

            # Make a copy of format data frame or otherwise the original will be
            # altered by the following key sorting
            format_dt <- data.table::copy(format_df)

            # Set key variables
            data.table::setkey(temp_data, qol_from, qol_to)
            data.table::setkey(format_dt, from, to)

            # Merge data frame with format by range
            temp_data <- data.table::foverlaps(temp_data, format_dt,
                                               by.x = c("qol_from", "qol_to"),
                                               by.y = c("from", "to")) |>
                dropp(current_var, "qol_from", "qol_to", "from", "to") |>
                collapse::frename("label" = current_var, .nse = FALSE)

            # Put NAs back into full data frame
            temp_data <- data.table::rbindlist(list(temp_data, temp_na), fill = TRUE)

            all_levels <- format_df[3] |>
                unlist(use.names = FALSE) |>
                unique() |>
                stats::na.omit()
        }
        # In case of discrete format
        else{
            # Rename label column to be specific to the variable
            format_df <- format_df |> collapse::frename("value" = current_var, .nse = FALSE)

            # Perform a cartesian join which joins all possibilities together, meaning it doubles observations
            temp_data <- collapse::join(temp_data, format_df,
                                        on       = current_var,
                                        how      = "left",
                                        multiple = TRUE,
                                        verbose  = FALSE)

            # If not all values are represented in the format container, check where there are gaps
            # and fill them at the affected positions
            na_positions <- positions <- which(is.na(temp_data[["label"]]) & !is.na(temp_data[[current_var]]))
            if (length(na_positions) > 0){
                temp_data[["label"]][na_positions] <- temp_data[[current_var]][na_positions]
            }

            # Drop current variable and rename newly joined label to current variable name
            temp_data <- temp_data |>
                dropp(current_var) |>
                collapse::frename(stats::setNames("label", current_var))

            # Extract the number of labels from format
            label_levels <- format_df[-1] |>
                unlist(use.names = FALSE) |>
                unique() |>
                stats::na.omit()

            all_levels <- union(label_levels, temp_data[[current_var]])
        }

        # Make sure that the labels will appear in order of the format when
        # applied to a variable in a data frame
        temp_data[[current_var]] <- factor(
            temp_data[[current_var]],
            levels  = all_levels,
            ordered = TRUE)
    }

    temp_data
}
