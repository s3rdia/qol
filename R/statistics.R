################################################################################
# Export functions
################################################################################
#' Perform Row Wise Calculations
#'
#' @description
#' Perform row wise calculations on numeric variables.
#'
#' @param data_frame A data frame in which are the values to be calculated.
#' @param statistics Available functions: "sum", "freq", "mean", "median", "mode",
#' "min", "max".
#' @param ... Variable names of the value variables.
#' @param round_digits The number of decimal places the values should be rounded to.
#'
#' @return
#' Returns a numeric vector.
#'
#' @examples
#' # Example data frame
#' my_data <- data.frame(var1 = 1:5,
#'                       var2 = c(6, 7, 8, NA, 10),
#'                       var3 = 11:15)
#'
#' # Calculate new variables
#' my_data[["sum"]]  <- my_data |> row_calculation("sum",  var1, var2, var3)
#' my_data[["mean"]] <- my_data |> row_calculation("mean", var1:var3)
#' @export
row_calculation <- function(data_frame,
                            statistics,
                            ...,
                            round_digits = NULL){
    # Just keep the selected variables and transpose the entire matrix. Since
    # the rows then become the columns, it is possible to just use column wise
    # operations on the matrix.
    var_matrix <- data_frame |>
        keep(...) |>
        t()

    if (!is.numeric(var_matrix)){
        print_message("ERROR", "Only numeric values allowed. Calculation will be aborted.")

        return(invisible(NA))
    }

    # Check if provided statistics is valid
    statistics <- get_origin_as_char(statistics, substitute(statistics))

    if (length(statistics) > 1){
        print_message("WARNING", "Only one <statistics> allowed at a time. The first element will be used.")

        statistics <- statistics[1]
    }

    if (!tolower(statistics) %in% c("sum", "mean", "median", "min", "max", "mode", "freq")){
        print_message("WARNING", "<Statistics> '[statistics]' [?is/are] invalid. 'sum' will be used.", statistics = tolower(statistics))

        statistics <- "sum"
    }

    # Get the desired operation
    stat_function <- switch(statistics,
                            sum    = collapse::fsum,
                            mean   = collapse::fmean,
                            median = collapse::fmedian,
                            min    = collapse::fmin,
                            max    = collapse::fmax,
                            mode   = collapse::fmode,
                            freq   = collapse::fnobs,
                            NULL)

    # Compute row result and return as vector
    result <- stat_function(var_matrix)

    if (!is.null(round_digits)){
        result <- round_values(result, round_digits)
    }

    result
}


################################################################################
# Internal functions used in summarise_plus
################################################################################
#' Unweighted Frequencies of Values Greater Than Zero
#'
#' @description
#' This stat always computes unweighted sums, completely ignoring provided weights.
#' It also just counts the values which are greater than 0.
#'
#' @param values The values to sum up.
#' @param group Grouping variables.
#'
#' @return
#' Returns uweighted frequencies of values greater than zero.
#'
#' @noRd
freq_g0_qol <- function(values, group){
    # Create a vector of same size as input vector and make every value a 1
    # that is greater than 0.
    # Sum up this new vector to get unweighted frequencies.
    values[values > 0] <- 1

    collapse::fsum(values, g = group)
}


#' Calculate Any Grouped And Weighted Percentiles
#'
#' @description
#' Calculates any percentile by grouping variables. Uses the SAS default way of
#' calculating percentiles, which is Type 2 described in [quantile()].
#'
#' @param values The values for which to compute percentiles.
#' @param weight A weighting variable.
#' @param group Grouping variables.
#' @param probs The percentiles that should be computed.
#'
#' @return
#' Returns da data.table with weighted percentiles.
#'
#' @noRd
percentiles_qol <- function(values, weight, group, probs){
    if (anyNA(values)){
        print_message("NOTE", c("There are missing values present in the variable[?s] '[vars]'. These will",
                                "be removed during the percentile calculation."), vars = names(values),
                      always_print = TRUE)
    }

    if (anyNA(weight)){
        print_message("NOTE", c("There are missing values present in the weight variable. These will",
                                "be removed during the percentile calculation."),
                      always_print = TRUE)
    }

    # If weight only consists of one unique value, then the results are unweighted.
    # In this case set weight to NULL so that a shortcut for the calculation can
    # be used.
    if (length(collapse::funique(weight)) == 1){
        weight <- NULL
    }

    # Capture the percentile names for later and extract the numeric value
    prob_names <- probs
    probs      <- as.numeric(sub("p", "", probs)) / 100

    # Split the grouping object
    split_ids <- collapse::gsplit(seq_len(collapse::fnrow(values)), group)

    # Calculate percentiles individually for each group
    result_list <- lapply(split_ids, function(id){
        # Extract the weights which belong to the current group
        group_weights <- weight[id]

        # Calculate all requested percentiles for every value variable. The result
        # is converted into a single named vector.
        result_vector <- unlist(lapply(names(values), function(variable){
                # Calculate all requested percentiles for the current variable
                # within the current group.
                percentiles <- calculate_single_percentiles(values[[variable]][id],
                                                            group_weights, probs)

                # Rename to final variable names
                stats::setNames(as.list(percentiles), paste0(variable, "_", prob_names))
            }),

            # Preserve names while flattening the nested list structure into
            recursive = FALSE,
            use.names = TRUE)

        # Convert the named vector into a one-row data.table. Each group contributes
        # exactly one row to the final result.
        data.table::as.data.table(result_vector)
    })

    # Stack all result rows into one final data frame
    data.table::rbindlist(result_list)
}


#' @return
#' Returns a weighted percentile vector.
#'
#' @noRd
calculate_single_percentiles <- function(variable, weight, probs){
    # Compute and return unweighted results if now weight ist provided
    if (is.null(weight)){
        return(stats::quantile(variable, probs, na.rm = TRUE))
    }

    # Remove observations where either the value or the weight is missing.
    # This ensures that both vectors keep the same length and remain aligned.
    valid_ids <- !is.na(variable) & !is.na(weight)

    variable <- variable[valid_ids]
    weight   <- weight[valid_ids]

    # If no valid observations remain, return one NA per requested percentile.
    if (length(variable) == 0){
        return(rep(NA_real_, length(probs)))
    }

    # Sort values in ascending order and apply the same ordering to the weights.
    sort_order <- order(variable)

    variable <- variable[sort_order]
    weight   <- weight[sort_order]

    # Calculate total weight.
    sum_of_weights <- collapse::fsum(weight)

    # If all weights sum to zero, weighted percentiles are undefined.
    if (sum_of_weights == 0){
        return(rep(NA_real_, length(probs)))
    }

    # Compute the weighted cumulative distribution function (CDF). Values range
    # from 0 to 1.
    weight_cdf <- collapse::fcumsum(weight) / sum_of_weights

    # Calculate each requested percentile.
    vapply(probs, function(prob){
        # Find the largest index whose cumulative weight is still
        # less than or equal to the requested percentile.
        lower <- findInterval(prob, weight_cdf)

        # If the percentile lies below the first weighted observation, return the
        # minimum value.
        if (lower == 0){
            return(variable[1])
        }

        # If the percentile lies beyond the last weighted observation,
        # return the maximum value.
        upper <- lower + 1

        if (upper > length(variable)){
            return(variable[lower])
        }

        # If the lower observation is still below the requested percentile,
        # use the next observation.
        if (weight_cdf[lower] < prob){
            variable[upper]
        }
        # Otherwise return a weighted average of the two surrounding
        # observations.
        else{
            (weight[lower] * variable[lower] +
             weight[upper] * variable[upper]) /
            (weight[lower] + weight[upper])
        }

    }, numeric(1))
}


#' Get Missings of Combined Grouping Variables
#'
#' @description
#' Calculate the number of missings, which is generated by the combination of all
#' grouping variables.
#'
#' @param group_vars Grouping variables.
#' @param notes Flag if missings should be computed and message should be printed.
#' @param na.rm Flag if NA values are removed or not.
#'
#' @return
#' Returns a message stating the number of missing values based on grouping variables.
#'
#' @noRd
get_group_missings <- function(group_vars, notes, na.rm){
    if (na.rm){
        return(FALSE)
    }

    if (!notes){
        return(FALSE)
    }

    # Count combined missings of group variables. To count as missings only
    # one of the variables has to be NA in a position.
    missings <- collapse::fsum(collapse::missing_cases(group_vars))

    # If there are missings calculate percentage and output a message
    if (missings > 0){
        nobs      <- length(group_vars[[1]])
        percent   <- round(missings * 100 / nobs, 1)
        none_miss <- nobs - missings

        print_message("NOTE", "[missings] missings generated from grouping variables ([percent] %). Number of observations: [none_miss]/[nobs]",
					  missings  = format(missings, big.mark = ".", decimal.mark = ","),
				      percent   = percent,
				      none_miss = format(none_miss, big.mark = ".", decimal.mark = ","),
				      nobs      = format(nobs, big.mark = ".", decimal.mark = ","),
					  always_print = TRUE)
    }
}


#' Compute Percentages by Group
#'
#' @description
#' Calculate the percentages inside a specific group of variables by generating the
#' super group first and joining the super group totals back to the data frame.
#'
#' @param original_df The original unchanged data frame provided by the user.
#' @param summary_df Already summed up result data frame to build up upon.
#' @param statistics User provided statistics.
#' @param group_vars Grouping variables.
#' @param formats Provided list of variables with formats to apply.
#' @param values he values for which to compute percentages.
#' @param weight Weighting variable.
#' @param list_of_statistics A list of all statistics that can be computed.
#' @param monitor_df Data frame which stores the monitoring values.
#' @param fast_pct Flag that states whether the function can take a shortcut for faster
#' calculation.
#'
#' @return
#' Returns a data frame with added percentages, if the statistic was specified.
#'
#' @noRd
compute_group_percentages <- function(original_df,
                                      summary_df,
                                      statistics,
                                      group_vars,
                                      formats,
                                      values,
                                      weight,
                                      list_of_statistics,
                                      monitor_df,
                                      fast_pct){
    if (!"pct_group" %in% statistics){
        return(list(summary_df, monitor_df))
    }

    monitor_df <- monitor_df |> monitor_start("pct_group", paste0("Calc(", paste(group_vars, collapse = " + "), ")"))

    # Create pseudo group variable for totals
    original_df[[".temp_key"]] <- 1
    summary_df[[".temp_key"]]  <- 1

    # Get rid of the last grouping variable to get the grouping above the
    # desired grouping
    if (length(group_vars) > 1){
        super_group <- group_vars[-length(group_vars)]
    }
    else{
        super_group <- ".temp_key"
    }

    cut_off_var <- group_vars[length(group_vars)]

    # If no fast percentage (means original_df is the full data frame)
    if (!fast_pct){
        # It is faster to summarise first without formats
        # and then apply the formats to a much smaller data frame.
        super_df <- original_df |>
            matrix_summarise(values,
                             super_group,
                             original_df[[weight]],
                             c("sum", "sum_wgt"),
                             list_of_statistics,
                             monitor_df)

        super_df <- super_df[[1]]

        super_df <- super_df |>
            apply_format(formats, super_group) |>
            collapse::fselect(-sum_wgt)

        # Catch new variable names
        values <- super_df |> inverse(super_group)
    }
    # If fast percentage (means original_df is already summarised)
    else{
        values <- values[grepl("_sum$", values)]

        # Summarise first
        super_df <- original_df |>
            collapse::fgroup_by(super_group) |>
            collapse::fsummarise(across(values, collapse::fsum))

        super_df <- super_df |>
            apply_format(formats, super_group)
    }

    # Generate new variable names for super_group totals
    new_names <- paste0(values, "_qol")
    old_names <- values

    # Final summarise with formatted data frame
    super_df <- super_df |>
        collapse::fgroup_by(super_group) |>
        collapse::fsummarise(across(values, collapse::fsum)) |>
        collapse::frename(stats::setNames(old_names, new_names)) |>
        collapse::fselect(super_group, new_names)

    # Join super_df to summarized data frame
    if (is.null(super_group)){
        # In case of only totals
        joined_df <- collapse::join(summary_df, super_df,
                                    on       = ".temp_key",
                                    how      = "left",
                                    multiple = TRUE,
                                    verbose  = FALSE,
								    overid   = 2)
    }
    else{
        # In case there is at least one grouping variable
        joined_df <- collapse::join(summary_df, super_df,
                                    on      = super_group,
                                    how     = "left",
                                    verbose = FALSE,
								    overid  = 2)
    }

    # Return finished data frame
    joined_df <- joined_df |>
        calculate_percentages(values, "pct_group", cut_off_var) |>
        collapse::fselect(-.temp_key)

    monitor_df <- monitor_df |> monitor_end()

    list(joined_df, monitor_df)
}


#' Compute Percentages by Group
#'
#' @description
#' Calculate the percentages inside a specific group of variables, which builds upon
#' a data frame which already carries the summarised super groups.
#'
#' @param summary_df Already summed up result data frame to build up upon.
#' @param super_df Already summed up super group results.
#' @param statistics User provided statistics.
#' @param group_vars Grouping variables.
#' @param values he values for which to compute percentages.
#' @param monitor_df Data frame which stores the monitoring values.
#' @param fast_pct Flag that states whether the function can take a shortcut for faster
#' calculation.
#'
#' @return
#' Returns a data frame with added percentages, if the statistic was specified.
#'
#' @noRd
compute_group_percentages_short <- function(summary_df,
                                            super_df,
                                            statistics,
                                            group_vars,
                                            values,
                                            monitor_df,
                                            fast_pct){
    if (!"pct_group" %in% statistics){
        return(list(summary_df, monitor_df))
    }

    monitor_df <- monitor_df |> monitor_start("pct_group", paste0("Calc(", paste(group_vars, collapse = " + "), ")"))

    # Get rid of the last grouping variable to get the grouping above the
    # desired grouping
    super_group <- group_vars[-length(group_vars)]

    # Generate new variable names for super_group totals
    old_names <- values
    new_names <- paste0(values, "_qol")

    super_df <- super_df |>
        collapse::fselect(super_group, old_names) |>
        collapse::frename(stats::setNames(old_names, new_names))

    # Join data frames by super group
    joined_df <- collapse::join(summary_df, super_df,
                                on      = super_group,
                                how     = "left",
                                verbose = FALSE,
								overid  = 2)

    # Return finished data frame
    joined_df <- joined_df |>
        calculate_percentages(values, "pct_group", group_vars[length(group_vars)])

    monitor_df <- monitor_df |> monitor_end()

    list(joined_df, monitor_df)
}


#' Compute Total Percentages
#'
#' @description
#' Calculate the percentages based on the overall totals.
#'
#' @param original_df The original unchanged data frame provided by the user.
#' @param summary_df Already summed up result data frame to build up upon.
#' @param statistics User provided statistics.
#' @param group_vars Grouping variables.
#' @param values he values for which to compute percentages.
#' @param weight Weighting variable.
#' @param list_of_statistics A list of all statistics that can be computed.
#' @param monitor_df Data frame which stores the monitoring values.
#' @param fast_pct Flag that states whether the function can take a shortcut for faster
#' calculation.
#'
#' @return
#' Returns a data frame with added percentages, if the statistic was specified.
#'
#' @noRd
compute_total_percentages <- function(original_df,
                                      summary_df,
                                      statistics,
                                      group_vars,
                                      values,
                                      weight,
                                      list_of_statistics,
                                      monitor_df,
                                      fast_pct){
    if (!"pct_total" %in% statistics){
        return(list(summary_df, monitor_df))
    }

    monitor_df <- monitor_df |> monitor_start("pct_total", paste0("Calc(", paste(group_vars, collapse = " + "), ")"))

    # Generate new variable names for super_group totals
    new_names <- paste0(values, "_qol")

    # Compute the data frame with summaries of the super groups
    super_df <- original_df |> collapse::fungroup()

    # If no fast percentage (means original_df is the full data frame)
    if (!fast_pct){
        super_df <- super_df |>
            matrix_summarise(values,
                             NULL,
                             super_df[[weight]],
                             c("sum", "sum_wgt"),
                             list_of_statistics,
                             monitor_df)

        # Split results and monitor
        super_df <- super_df[[1]]

        # Catch new variable names
        values <- super_df |>
            collapse::fselect(-sum_wgt) |>
            inverse(group_vars)
    }
    # If fast percentage (means original_df is already summarised)
    else{
        values <- values[grepl("_sum$", values)]

        # Simple Summarise
        super_df <- original_df |>
            collapse::fsummarise(across(values, collapse::fsum))
    }

    # Generate new variable names for super_group totals
    new_names <- paste0(values, "_qol")
    old_names <- values

    super_df <- super_df |>
        collapse::frename(stats::setNames(old_names, new_names)) |>
        collapse::fselect(new_names)

    # Join super_df to summarized data frame
    summary_df[[".temp_key"]] <- 1
    super_df[[".temp_key"]]   <- 1

    joined_df <- collapse::join(summary_df, super_df,
                                on       = ".temp_key",
                                how      = "left",
                                multiple = TRUE,
                                verbose  = FALSE,
								overid   = 2)

    # Return finished data frame
    joined_df <- joined_df |>
        calculate_percentages(values, "pct_total", ".temp_key") |>
        collapse::fselect(-.temp_key)

    monitor_df <- monitor_df |> monitor_end()

    list(joined_df, monitor_df)

}


#' Compute Total Percentages
#'
#' @description
#' Calculate the percentages based on the overall totals, which builds upon
#' a data frame which already carries the summarised overall totals.
#'
#' @param summary_df Already summed up result data frame to build up upon.
#' @param total_df Already summed up result data frame to build up upon.
#' @param statistics User provided statistics.
#' @param values he values for which to compute percentages.
#' @param last_group_var The last variable of grouping variables.
#' @param pct_name Name of percentages.
#' @param monitor_df Data frame which stores the monitoring values.
#'
#' @return
#' Returns a data frame with added percentages, if the statistic was specified.
#'
#' @noRd
compute_total_percentages_short <- function(summary_df,
                                            total_df,
                                            statistics,
                                            values,
                                            last_group_var,
                                            pct_name = "pct_total",
                                            monitor_df){
    if (!pct_name %in% statistics){
        return(list(summary_df, monitor_df))
    }

    monitor_df <- monitor_df |> monitor_start(pct_name, paste0("Calc(", paste(last_group_var, collapse = " + "), ")"))

    # Join super_df to summarized data frame
    summary_df[[".temp_key"]] <- 1
    total_df[[".temp_key"]]   <- 1

    joined_df <- collapse::join(summary_df, total_df,
                                on       = ".temp_key",
                                how      = "left",
                                multiple = TRUE,
                                verbose  = FALSE,
								overid   = 2)

    # Return finished data frame
    if (pct_name == "pct_total"){
        joined_df <- joined_df |>
            calculate_percentages(values, pct_name, ".temp_key") |>
            collapse::fselect(-.temp_key)
    }
    else{
        # For group percentages with depth 1
        joined_df <- joined_df |>
            calculate_percentages(values, pct_name, last_group_var) |>
            collapse::fselect(-.temp_key)
    }

    monitor_df <- monitor_df |> monitor_end()

    list(joined_df, monitor_df)

}


#' Compute Percentages
#'
#' @description
#' The main process of calculating percentages.
#'
#' @param joined_df Data frame with joined super group or overall totals.
#' @param values he values for which to compute percentages.
#' @param last_group_var The last variable of grouping variables.
#' @param pct_name Name of percentages.
#'
#' @return
#' Returns a data frame with added percentages.
#'
#' @noRd
calculate_percentages <- function(joined_df, values, pct_name, last_group_var){
    # Specify numerators and denominators
    numerator <- values

    if (!all(values %in% names(joined_df))){
        # In case variable names have the statistic extension at the end
        numerator <- paste0(values, "_sum")
    }

    denominator <- paste0(values, "_qol")
    new_values  <- paste0(gsub("_sum$", "", values), "_", pct_name)

    # Compute percentages for every variable
    for (i in seq_along(numerator)){
        current_num     <- numerator[i]
        current_den     <- denominator[i]
        current_new_var <- new_values[i]

        # Evaluate percentage and set division by 0 to NA
        result <- joined_df[[current_num]] * 100 / joined_df[[current_den]]
        result[is.nan(result)] <- NA

        joined_df[[current_new_var]] <- result
    }

    joined_df |> dropp(denominator)
}
