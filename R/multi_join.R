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
#' data frame. To join on different variable names a named list of character
#' vectors has to be provided, with one list entry per data frame. If the first
#' data frame should be joined on different variables with each following data
#' frame, its list entry can be a list of character vectors with one vector per
#' join. If fewer combinations are provided than there are remaining data frames,
#' the last combination is repeated.
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
#' All data frames are joined on the first one. The first data frame can therefore
#' be joined on different variables with each following data frame, similar to a
#' SQL statement with multiple JOIN clauses.
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
#' # Joining the first data frame on different variables with each following data frame
#' df1d <- data.frame(key1 = c(1, 1, 2),
#'                    key2 = c("a", "a", "b"),
#'                    key3 = c(10, 20, 20),
#'                    a    = "a")
#'
#' df2d <- data.frame(var1 = c(1, 2),
#'                    var2 = c("a", "b"),
#'                    b    = "b")
#'
#' df3d <- data.frame(any  = c("a", "a", "b"),
#'                    name = c(10, 20, 20),
#'                    c    = "c")
#'
#' multiple_joined4 <- multi_join(list(df1d, df2d, df3d),
#'                                on = list(df1d = list(c("key1", "key2"), c("key3", "key2")),
#'                                          df2d = c("var1", "var2"),
#'                                          df3d = c("name", "any")))
#'
#' @export
multi_join <- function(data_frames,
                       on,
                       how             = "left",
                       keep_indicators = FALSE,
                       monitor         = .qol_options[["monitor"]]){
    # Measure the time
    print_start_message()
    print_step("GREY", "Error handling")

    #-------------------------------------------------------------------------#
    monitor_df <- NULL |> monitor_start("Error handling", "Preparation")
    #-------------------------------------------------------------------------#

    # Check if a valid list of data frames is given
    if (!all(vapply(data_frames, is.data.frame, logical(1)))){
        print_message("ERROR", "Data frames must be provided as a list. Join will be aborted.")
        return(invisible(NULL))
    }

    if (length(data_frames) < 2L){
        print_message("ERROR", "At least two data frames are required. Join will be aborted.")
        return(invisible(NULL))
    }

    # First convert data frame to data table
    for (i in seq_along(data_frames)){
        if (!data.table::is.data.table(data_frames[[i]])){
            data_frames[[i]] <- data.table::as.data.table(data_frames[[i]])
        }
    }

    # If the user passed an unnamed list, get the data frame names back and set
    # them as names.
    if (!is_named_list(data_frames)){
        # Grab the raw call made to the function
        complete_call <- match.call()

        # Extract the passed arguments of the data_frames parameter and set as names
        names(data_frames) <- sapply(as.list(complete_call[["data_frames"]][-1]), deparse)
    }

    ###########################################################################
    # Error handling
    ###########################################################################

    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # If a named list is given, a join with unequal variable names will be performed
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    unequal_names <- FALSE

    # Try evaluating "on" directly. This works, if variable names were passed as
    # characters.
    on <- tryCatch({
        on
    }, error = function(e){
        NULL
    })

    # If variable names were passed without quotation marks "on" will be NULL here
    # and evaluation will be different.
    if (is.null(on)){
        # Capture the complete "on" argument
        on_call <- match.call()
        on_call <- as.list(on_call[["on"]])[-1]

        # Convert list elements to characters
        on <- lapply(on_call, function(element){
            if (is.symbol(element)){
                as.character(element)
            }
            # A nested list means the first data frame is joined on different
            # variables with each following data frame. Each list entry is one
            # join variable combination.
            else if (is.call(element) && identical(element[[1]], quote(list))){
                lapply(as.list(element)[-1], function(inner_element){
                    if (is.symbol(inner_element)){
                        as.character(inner_element)
                    }
                    else if (is.call(inner_element) && identical(inner_element[[1]], quote(c))){
                        as.character(as.list(inner_element)[-1])
                    }
                    else{
                        as.character(inner_element)
                    }
                })
            }
            else{
                sapply(as.list(element)[-1], as.character)
            }
        })
    }

    if (is.list(on)){
        # If all list entries have a name
        if (!is.null(names(on)) && all(nzchar(names(on)))){
            # Check if number of list entries matches number of data frames
            if (length(data_frames) != length(on)){
                print_message("ERROR", "Length of <on> doesn't match the number of provided data frames. Join will be aborted.")
                return(invisible(NULL))
            }

            unequal_names <- TRUE

            # Number of joins equals the number of data frames minus one
            join_count <- length(data_frames) - 1

            # The first data frame can either provide the same join variables
            # for every join (a character vector) or different join variables
            # for each join (a list of character vectors, with one entry per
            # join).
            base_on <- on[[1]]

            if (is.list(base_on)){
                # If not enough variable combinations are provided, the last
                # combination is repeated until it fits the remaining data frames.
                if (length(base_on) < join_count){
                    base_on <- c(base_on, rep(base_on[length(base_on)], join_count - length(base_on)))
                }
                # If too many variable combinations are provided, cut the excess
                else if (length(base_on) > join_count){
                    base_on <- base_on[seq_len(join_count)]
                }
            }
            # The same variable combination is used for every join
            else{
                base_on <- rep(list(base_on), join_count)
            }

            # All other data frames have exactly one join variable combination
            if (!all(vapply(on[-1], is.character, logical(1)))){
                print_message("ERROR", c("The second and all following data frames in <on> must provide their",
                                         "join variables as a character vector. Join will be aborted."))
                return(invisible(NULL))
            }
        }
        # If a list entry is missing a name
        else{
            # If there is no name at all, treat it as a vector and go on
            if (is.null(names(on))){
                on      <- unlist(on)
                base_on <- on
            }
            # If there are some names, but not all entries are named, abort
            else{
                print_message("ERROR", c("If all data frames have the same variable names for the <on> variables,",
                                         "provide them as a vector instead of a list. For unequal names provide a",
                                         "named list. Join will be aborted."))
                return(invisible(NULL))
            }
        }
    }
    else{
        base_on <- get_origin_as_char(on, substitute(on))
    }

    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # Check if all data frames (except the first) one only have unique value
    # combinations for the 'on' variables
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    for (i in seq_along(data_frames)){
        # Check if provided variable names are in the base data frame
        if (!unequal_names){
            if (!all(on %in% names(data_frames[[i]]))){
                print_message("ERROR", c("Not all <on> variables ([on]) appear in data frame [name].",
										 "Join will be aborted."), on = on, name = i)
                return(invisible(NULL))
            }
        }
        else{
            # The first data frame can have different join variables for each
            # join, so all its combinations have to be checked.
            if (i == 1){
                variables_to_check <- unique(unlist(base_on))
            }
            else{
                variables_to_check <- on[[i]]
            }

            if (!all(variables_to_check %in% names(data_frames[[i]]))){
                print_message("ERROR", c("Not all <on> variables ([on]) appear in data frame [name].",
                                         "Join will be aborted."), on = variables_to_check, name = i)
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
            if (collapse::any_duplicated(data_frames[[i]][on])){
                dup_combinations <- get_duplicate_expressions(data_frames[[i]], on)

                print_message("ERROR", c("The second and all following data frames need to have unique combinations",
                                         "in the provided <on> variables. The following duplicated value combinations",
                                         "were found: [duplicates]",
                                         "Join will be aborted."),
                              duplicates = dup_combinations)
                return(invisible(NULL))
            }
        }
        # On unequal names each individual variable combination has to be checked on the corresponding data frame
        else{
            if (collapse::any_duplicated(data_frames[[i]][on[[i]]])){
                dup_combinations <- get_duplicate_expressions(data_frames[[i]], on[[i]])

                print_message("ERROR", c("The second and all following data frames need to have unique combinations",
                                         "in the provided <on> variables. The following duplicated value combinations",
                                         "were found: [duplicates]",
                                         "Join will be aborted."),
                              duplicates = dup_combinations)
                return(invisible(NULL))
            }
        }
    }

    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # Join methods
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    # Only keep valid join methods
    valid_join_methods <- c("left", "right", "inner", "full", "outer", "left_inner", "right_inner")
    valid_methods <- tolower(how) %in% valid_join_methods
    invalid_how   <- how[!valid_methods]
    how           <- how[valid_methods]

    if (length(invalid_how) > 0){
        print_message("WARNING", "The provided join method '[invalid]' is not valid.", invalid = invalid_how)
    }

    # If length of provided joins is lesser than number of data frames minus one
    if (length(how) == 0){
        print_message("WARNING", "No valid join method provided, 'left' will be used.")

        how <- "left"
    }
    # If length of provided joins is lesser than number of data frames minus one
    else if (length(how) < length(data_frames) - 1){
        # Repeat the last vector element until number of data frames minus one is reached
        how <- c(how, rep(utils::tail(how, 1), length(data_frames) - 1))

        # I am not printing the warning here, so one can be lazy on the function call. Meaning one can just
        # input one join method, if it is the same for all joins.
        # print_message("WARNING", "Not enough join methods in 'how' given. Last join method will be repeated.")
    }
    # If length of provided joins is greater number of data frames minus one
    else if (length(how) > length(data_frames) - 1){
        # Cut elements down to number of data frames minus one
        how <- utils::head(how, length(data_frames) - 1)

        print_message("NOTE", "Too many join methods given in <how>. Excess methods will remain unused.")
    }

    ###########################################################################
    # Join starts
    ###########################################################################

    join_keys <- paste0(".", letters)

    print_step("MAJOR", "Begin joining.")

    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # First perform joins as a full joins. This way all combinations are in the
    # final data frame. Afterwards the data frame is conditionally filtered.
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    base_df_name <- names(data_frames)[1]

    # Start the actual joining
    for (i in seq_along(data_frames)){
        #---------------------------------------------------------------------#
        monitor_df <- monitor_df |> monitor_next(paste0("Join ", i - 1), "Join")
        #---------------------------------------------------------------------#

        # Skip first data frame, because it is not joined on itself
        if (i == 1){
            joined_df <- data_frames[[i]]
            joined_df[[join_keys[[i]]]] <- 1

            next
        }

        to_join_df  <- data_frames[[i]]
        join_method <- how[[i - 1]]

        # Depending on the join the printed data frame names have to be swapped
        if (!join_method %in% c("right", "inner_right")){
            print_step("MINOR", "{how} joining [data_frame1] to [data_frame2].",
                       how = join_method, data_frame1 = names(data_frames)[i], data_frame2 = base_df_name)
        }
        else{
            print_step("MINOR", "{how} joining [data_frame1] to [data_frame2].",
                       how = join_method, data_frame1 = base_df_name, data_frame2 = names(data_frames)[i])
        }

        # Create filter variable which indicates, which data frame provides observations
        to_join_df[[join_keys[[i]]]] <- 1

        #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        # Equality crossroads
        #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

        # Get the variable names of the base data frame and the one to be joined
        to_join_df_name     <- names(data_frames)[i]
        joined_df_var_names <- names(joined_df)
        to_join_var_names   <- names(to_join_df)

        # On equal names there is just one combination which can be input directly
        if (!unequal_names){
            join_variables <- base_on
        }
        # On unequal names each individual variable combination has to be checked on the corresponding data frame
        else{
            to_join_on <- on[[i]]

            # The join variables of the first data frame for this specific join
            base_join_vars <- base_on[[i - 1]]

            # Check if the same number of 'on' variables are provided
            if (length(base_join_vars) != length(to_join_on)){
                print_message("ERROR", c("Unequal number of <on> variables provided: [on1] vs [on_i].",
                                         "Join will be aborted."), on1 = base_join_vars, on_i = to_join_on)
                return(invisible(NULL))
            }

            join_variables  <- stats::setNames(to_join_on, base_join_vars)
        }

        # Check for intersecting variable names and clear them before the join,
        # because otherwise it can happen that multiple variables with the exact
        # same name get created through the join below.
        if (!unequal_names){
            joined_exclude  <- on
            to_join_exclude <- on
        }
        else{
            joined_exclude  <- base_on[[i - 1]]
            to_join_exclude <- on[[i]]
        }

        joined_df_var_names <- joined_df_var_names[!joined_df_var_names %in% joined_exclude]
        to_join_var_names   <- to_join_var_names[!to_join_var_names %in% to_join_exclude]

        duplicate_names <- intersect(joined_df_var_names, to_join_var_names)

        if (length(duplicate_names) > 0){
            # On other than right joins clear from to right data frame
            if (!join_method %in% c("right", "inner_right")){
                print_message("WARNING", c("Duplicate variable names found: [duplicates].",
                                           "These variables will be removed from '[data_frame]' before the join."),
                              duplicates = duplicate_names, data_frame = to_join_df_name)
            }
            # On right join clear from the left (base) data frame
            else{
                print_message("WARNING", c("Duplicate variable names found: [duplicates].",
                                           "These variables will be removed from '[data_frame]' before the join."),
                              duplicates = duplicate_names, data_frame = base_df_name)
            }

            to_join_df <- to_join_df |> dropp(duplicate_names)
        }

        # Perform the actual join
        joined_df <- collapse::join(joined_df, to_join_df,
                                    on      = join_variables,
                                    how     = "full",
                                    verbose = FALSE,
									overid  = 2)

        #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        # Subset data frame according to provided join methods
        #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

        #---------------------------------------------------------------------#
        monitor_df <- monitor_df |> monitor_next(paste0("Subset ", i - 1), "Join")
        #---------------------------------------------------------------------#

        if (tolower(join_method) == "left"){
            joined_df <- joined_df |> collapse::fsubset(joined_df[[join_keys[[1]]]] == 1)
        }
        else if (tolower(join_method) == "right"){
            joined_df <- joined_df |> collapse::fsubset(joined_df[[join_keys[[i]]]] == 1)
        }
        else if (tolower(join_method) == "inner"){
            joined_df <- joined_df |> collapse::fsubset(joined_df[[join_keys[[1]]]] == 1 & joined_df[[join_keys[[i]]]] == 1)
        }
        else if (tolower(join_method) == "outer"){
            joined_df <- joined_df |> collapse::fsubset(is.na(joined_df[[join_keys[[1]]]]) | is.na(joined_df[[join_keys[[i]]]]))
        }
        else if (tolower(join_method) == "left_inner"){
            joined_df <- joined_df |> collapse::fsubset(joined_df[[join_keys[[1]]]] == 1 & is.na(joined_df[[join_keys[[i]]]]))
        }
        else if (tolower(join_method) == "right_inner"){
            joined_df <- joined_df |> collapse::fsubset(is.na(joined_df[[join_keys[[1]]]]) & joined_df[[join_keys[[i]]]] == 1)
        }

        # Refresh the base indicator, if more joins are coming. This way it shows
        # how it corresponds to the current running join instead of only the
        # original base data frame. Otherwise rows which were added to the
        # running join by a widening join (e.g. right, full, outer) would be
        # dropped by the next join, because their base indicator is not set.
        if (i < length(data_frames)){
            joined_df[[join_keys[[1]]]] <- rep(1, collapse::fnrow(joined_df))
        }

        # Drop indicator of joined data frame
        if (!keep_indicators){
            key_to_drop <- as.character(join_keys[[i]])
            joined_df   <- joined_df |> dropp(key_to_drop)
        }
    }

    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # Clean up
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    #-------------------------------------------------------------------------#
    monitor_df <- monitor_df |> monitor_next("Finish join ", "Join")
    #-------------------------------------------------------------------------#

    # Drop indicator of base data frame
    if (!keep_indicators){
        key_to_drop <- as.character(join_keys[[1]])
        joined_df <- joined_df |> dropp(key_to_drop)
    }
    # If join indicators should stay in the data frame, sort them to the back
    else{
        joined_df <- joined_df |> setcolorder_by_pattern("^\\.")
    }

    print_closing()

    #-------------------------------------------------------------------------#
    monitor_df <- monitor_df |> monitor_end()
    monitor_df |> monitor_plot(draw_plot = monitor)
    #-------------------------------------------------------------------------#

    joined_df
}


#' Get Duplicated Variable Expressions
#'
#' @description
#' Get a string of the unique duplicated value combinations for the "on"
#' variables of a data frame.
#'
#' @param data_frame The data frame containing the "on" variables.
#' @param on The actual variables on which to join.
#'
#' @return
#' Returns a concatenated string of variable expressions.
#'
#' @noRd
get_duplicate_expressions <- function(data_frame, on){
    # Only keep the unique duplicated combinations
    combos <- data_frame[on]
    combos <- collapse::funique(combos[duplicated(combos), , drop = FALSE])

    # Create a string representation for each combination
    combo_strings <- vapply(seq_len(nrow(combos)), function(row){
        paste(unlist(combos[row, , drop = FALSE], use.names = FALSE), collapse = " - ")
    }, character(1))

    paste(combo_strings, collapse = "; ")
}
