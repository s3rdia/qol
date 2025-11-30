#' Join Multiple Data Frames In One Go
#'
#' @description
#' Join two or more data frames together in one operation. [multi_join()] can handle
#' multiple different join methods and can join on differently named variables.
#'
#' @param data_frames A list of data frames to join together. The second and all
#' following data frames will be joined on the first one.
#' @param on The key variables on which the data frames should be joined. If a
#' character vector is provided, the function assumes all the variables are in every
#' data frame. To join on different  variable names a list of character vectors has
#' to be provided.
#' @param how A character vector containing the join method names. Available methods are:
#' left, right, inner, full, outer, left_inner and right_inner.
#' @param keep_indicators FALSE by default. If TRUE, a variable for each data frame
#' is created, which indicates whether a data frame provides values.
#' @param monitor FALSE by default. If TRUE, outputs two charts to visualize the
#' functions time consumption.
#'
#' @details
#' [multi_join()] is based on the 'SAS' Data-Step function Merge. Merge is capable of
#' joining multiple data sets together at once, with a very basic syntax.
#'
#' Provide the dataset names, the variables, on which they should be joined and after
#' a full join is complete, the user can decide which parts of the joins should remain
#' in the final dataset.
#'
#' [multi_join()] tries to keep the simplicity, while giving the user the power, to
#' do more joins at the same time. Additionally to what Merge can do, this function
#' also makes use of the Proc SQL possibility to join datasets on different variable
#' names.
#'
#' @return
#' Returns a single data frame with joined variables from all given data frames.
#'
#' @examples
#' # Example data frames
#' df1 <- data.frame(key = c(1, 1, 1, 2, 2, 2),
#'                   a   = c("a", "a", "a", "a", "a", "a"))
#'
#' df2 <- data.frame(key = c(2, 3),
#'                   b   = c("b", "b"))
#'
#' # See all different joins in action
#' join_methods <- c("left", "right", "inner", "full", "outer", "left_inner", "right_inner")
#' joined_data  <- list()
#'
#' for (method in seq_along(join_methods)){
#'     joined_data[[method]] <- multi_join(list(df1, df2),
#'                                         on  = "key",
#'                                         how = join_methods[[method]])
#' }
#'
#' # Left join on more than one key
#' df1b <- data.frame(key1 = c(1, 1, 1, 2, 2, 2),
#'                    key2 = c("a", "a", "a", "a", "a", "a"),
#'                    a    = c("a", "a", "a", "a", "a", "a"))
#'
#' df2b <- data.frame(key1 = c(2, 3),
#'                    key2 = c("a", "a"),
#'                    b    = c("b", "b"))
#'
#' left_joined <- multi_join(list(df1b, df2b), on = c("key1", "key2"))
#'
#' # Join more than two data frames
#' df3 <- data.frame(key = c(1, 2),
#'                   c   = c("c", "c"))
#'
#' multiple_joined <- multi_join(list(df1, df2, df3), on = "key")
#'
#' # You can also use different methods for each join
#' multiple_joined2 <- multi_join(list(df1, df3, df2),
#'                                on  = "key",
#'                                how = c("left", "right"))
#'
#' # Joining on different variable names
#' df1c <- data.frame(key1 = c(1, 1, 1, 2, 2, 2),
#'                    key2 = c("a", "a", "a", "a", "a", "a"),
#'                    a    = c("a", "a", "a", "a", "a", "a"))
#'
#' df2c <- data.frame(var1 = c(2, 3),
#'                    var2 = c("a", "a"),
#'                    b    = c("b", "b"))
#'
#' df3c <- data.frame(any  = c(1, 2),
#'                    name = c("a", "a"),
#'                    c    = c("c", "c"))
#'
#' multiple_joined3 <- multi_join(list(df1c, df2c, df3c),
#'                                on = list(df1c = c("key1", "key2"),
#'                                          df2c = c("var1", "var2"),
#'                                          df3c = c("any", "name")))
#'
#' @export
multi_join <- function(data_frames,
                       on,
                       how             = "left",
                       keep_indicators = FALSE,
                       monitor         = FALSE){
    # Measure the time
    start_time <- Sys.time()

    monitor_df <- NULL |> monitor_start("Error handling", "Preparation")

    # Check if a valid list of data frames is given
    if (!all(vapply(data_frames, is.data.frame, logical(1)))){
        message(" X ERROR: Data frames must be provided as a list. Join will be aborted.")
        return(invisible(NULL))
    }

    if (length(data_frames) < 2L){
        message(" X ERROR: At least two data frames are required. Join will be aborted.")
        return(invisible(NULL))
    }

    # First convert data frame to data table
    for (i in seq_along(data_frames)){
        if (!data.table::is.data.table(data_frames[[i]])) {
            data_frames[[i]] <- data.table::as.data.table(data_frames[[i]])
        }
    }

    ###########################################################################
    # Error handling
    ###########################################################################

    # If a named list is given, a join with unequal variable names will be performed
    unequal_names <- FALSE

    if (is.list(on)){
        # If all list entries have a name
        if (!is.null(names(on)) && all(nzchar(names(on)))){
            # Check if number of list entries matches number of data frames
            if (length(data_frames) != length(on)){
                message(" X ERROR: Length of 'on' doesn't match the number of provided data frames. Join will be aborted.")
                return(invisible(NULL))
            }

            unequal_names <- TRUE
        }
        # If a list entry is missing a name abort
        else{
            message(" X ERROR: If all data frames have the same variable names for the 'on' variables,\n",
                    "          provide them as a vector instead of a list. For unequal names provide a\n",
                    "          named list. Join will be aborted.")
            return(invisible(NULL))
        }
    }

    # Check if all data frames (except the first) one only have unique value combinations for the 'on' variables
    for (i in seq_along(data_frames)){
        # Check if provided variable names are in the base data frame
        if (!unequal_names){
            if (!all(on %in% names(data_frames[[i]]))){
                message(" X ERROR: Not all 'on' variables (", paste(on, collapse = ", "), ") appear in data frame ", i, ".\n",
                        "          Join will be aborted.")
                return(invisible(NULL))
            }
        }
        else{
            if (!all(on[[i]] %in% names(data_frames[[i]]))){
                message(" X ERROR: Not all 'on' variables (", paste(on[[i]], collapse = ", "), ") appear in data frame ", i, ".\n",
                        "          Join will be aborted.")
                return(invisible(NULL))
            }
        }

        # Skip first data frame. This is the only one that is allowed to have duplicate combinations.
        if (i == 1){
            next
        }

        # On equal names there is just one combination which needs to be checked
        if (!unequal_names){
            # Check for duplicate combinations
            if (any(duplicated(data_frames[[i]][on]))){
                message(" X ERROR: The second and all following data frames need to have unique combinations\n",
                        "          in the provided 'on' variables. Join will be aborted.")
                return(invisible(NULL))
            }
        }
        # On unequal names each individual variable combination has to be checked on the corresponding data frame
        else{
            if (any(duplicated(data_frames[[i]][on[[i]]]))){
                message(" X ERROR: The second and all following data frames need to have unique combinations\n",
                        "          in the provided 'on' variables. Join will be aborted.")
                return(invisible(NULL))
            }
        }
    }

    # Only keep valid join methods
    valid_join_methods <- c("left", "right", "inner", "full", "outer", "left_inner", "right_inner")
    invalid_how        <- how[!tolower(how) %in% valid_join_methods]
    how                <- how[tolower(how) %in% valid_join_methods]

    if (length(invalid_how) > 0){
        message(" ! WARNING: The provided join method '", paste(invalid_how, collapse = ", "), "' is not valid.")
    }

    # If length of provided joins is lesser than number of data frames minus one
    if (length(how) == 0){
        how <- "left"

        message(" ! WARNING: No valid join method provided, 'left' will be used.")
    }
    # If length of provided joins is lesser than number of data frames minus one
    else if (length(how) < length(data_frames) - 1){
        # Repeat the last vector element until number of data frames minus one is reached
        how <- c(how, rep(utils::tail(how, 1), length(data_frames) - 1))

        # I am not printing the warning here, so one can be lazy on the function call. Meaning one can just
        # input one join method, if it is the same for all joins.
        # message(" ! WARNING: Not enough join methods in 'how' given. Last join method will be repeated.")
    }
    # If length of provided joins is greater number of data frames minus one
    else if (length(how) > length(data_frames) - 1){
        # Cut elements down to number of data frames minus one
        how <- utils::head(how, length(data_frames) - 1)

        message(" ~ NOTE: Too many join methods given in 'how'. Excess methods will remain unused.")
    }

    ###########################################################################
    # Join starts
    ###########################################################################

    join_keys <- paste0(".", letters)

    message("\n > Begin joining.")

    # First perform joins as a full joins. This way all combinations are in the final
    # data frame. Afterwards the data frame is conditionally filtered.
    for (i in seq_along(data_frames)){
        monitor_df <- monitor_df |> monitor_next(paste0("Join ", i - 1), "Join")

        # Skip first data frame, because it is not joined on itself
        if (i == 1){
            joined_df <- data_frames[[i]]
            joined_df[[join_keys[[i]]]] <- 1

            next
        }

        message("   + ", how[[i - 1]], " joining data_frame ", i, " to base data frame.")

        # Create filter variable which indicates, which data frame provides observations
        data_frames[[i]][[join_keys[[i]]]] <- 1

        # On equal names there is just one combination which can be input directly
        if (!unequal_names){
            joined_df <- collapse::join(joined_df, data_frames[[i]],
                                        on       = on,
                                        how      = "full",
                                        verbose  = FALSE)
        }
        # On unequal names each individual variable combination has to be checked on the corresponding data frame
        else{
            # Check if the same number of 'on' variables are provided
            if (length(on[[1]]) != length(on[[i]])){
                message(" X ERROR: Unequal number of 'on' variables provided. ", paste(on[[1]], collapse = ", "), " vs. " , paste(on[[i]], collapse = ", "), ".\n",
                        "          Join will be aborted.")
                return(invisible(NULL))
            }

            joined_df <- collapse::join(joined_df, data_frames[[i]],
                                        on       = stats::setNames(on[[i]], on[[1]]),
                                        how      = "full",
                                        verbose  = FALSE)
        }

        monitor_df <- monitor_df |> monitor_next(paste0("Subset ", i - 1), "Join")

        # Subset data frame according to provided join methods
        if (tolower(how[[i - 1]]) == "left"){
            joined_df <- joined_df |> collapse::fsubset(joined_df[[join_keys[[1]]]] == 1)
        }
        else if (tolower(how[[i - 1]]) == "right"){
            joined_df <- joined_df |> collapse::fsubset(joined_df[[join_keys[[i]]]] == 1)
        }
        else if (tolower(how[[i - 1]]) == "inner"){
            joined_df <- joined_df |> collapse::fsubset(joined_df[[join_keys[[1]]]] == 1 & joined_df[[join_keys[[i]]]] == 1)
        }
        else if (tolower(how[[i - 1]]) == "outer"){
            joined_df <- joined_df |> collapse::fsubset(is.na(joined_df[[join_keys[[1]]]]) | is.na(joined_df[[join_keys[[i]]]]))
        }
        else if (tolower(how[[i - 1]]) == "left_inner"){
            joined_df <- joined_df |> collapse::fsubset(joined_df[[join_keys[[1]]]] == 1 & is.na(joined_df[[join_keys[[i]]]]))
        }
        else if (tolower(how[[i - 1]]) == "right_inner"){
            joined_df <- joined_df |> collapse::fsubset(is.na(joined_df[[join_keys[[1]]]]) & joined_df[[join_keys[[i]]]] == 1)
        }

        # Drop indicator of joined data frame
        if (!keep_indicators){
            joined_df <- joined_df |> dropp(join_keys[[i]])
        }
    }

    monitor_df <- monitor_df |> monitor_next("Finish join ", "Join")

    # Drop indicator of base data frame
    if (!keep_indicators){
        joined_df <- joined_df |> dropp(join_keys[[1]])
    }
    # If join indicators should stay in the data frame, sort them to the back
    else{
        joined_df <- joined_df |> setcolorder_by_pattern("^\\.")
    }

    end_time <- round(difftime(Sys.time(), start_time, units = "secs"), 3)
    message("\n- - - 'multi_join' execution time: ", end_time, " seconds\n")

    monitor_df <- monitor_df |> monitor_end()
    monitor_df |> monitor_plot(draw_plot = monitor)

    joined_df

}
