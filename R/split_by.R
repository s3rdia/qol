#' Split Data Frame By Variable Expressions Or Condition
#'
#' @description
#' Split up a data frame based on variable expressions or on conditions to receive
#' multiple smaller data frames. Both possibilities can be used at the same time.
#'
#' @param data_frame A data frame which should be split up into multiple data frames.
#' @param ... Pass in one or multiple variables and/or conditions on which the provided
#' data frame should be splitted.
#' @param formats A list in which is specified which formats should be applied to which
#' variables. Macro variables can be used in the variable names and format names.
#' @param inverse Uses the inverse conditions to split up the data frame.
#' @param monitor FALSE by default. If TRUE, outputs two charts to visualize the
#' functions time consumption.
#'
#' @details
#' [split_by()] is based on the explicit Output from 'SAS'. With the Output function
#' one can - among other things - explicitly tell 'SAS' which observation to output into
#' which data set. Which enables the user to output one observation into one or multiple
#' data sets.
#'
#' Instead of subsetting the same data frame multiple times manually, you can subset it
#' multiple times at once with this function.
#'
#' The function can take in conditions as characters to be able to parse the conditions.
#' The parsing allows to write conditions in a SAS like way. For example the condition
#'
#' "age >= 15 & age < 65" can be written as "15 <= age < 65"
#'
#' Additionally and/or (case insensitive) are recognized keywords which will be translated into &/|.
#'
#' "age >= 15 and age < 65 or sex == 1" becomes "age >= 15 & age < 65 | sex == 1"
#'
#' And [macro()] variables are allowed in any spot, e.g.:
#'
#' YEAR <- 2025\
#' "year == &YEAR" translates to "year == 2025".
#'
#' Single = get translated to ==, the SAS not ^= is translated to !=.
#'
#' In SAS you can write "age in (1 4 7 15 21)" or "age not in (2, 4, 37, 82)" which
#' gets translated into "age %in% (1, 4, 7, 15, 21)" and "!(age %in% (2, 4, 37, 82))".
#'
#' When using == or != operators you can check if character expressions start with
#' (text:), end with (:text) or contain a certain text (:text:). These also get
#' translated.
#'
#' @return Returns a list of data frames split by variable expressions and/or conditions.
#' The lists names are the variable expressions or conditions.
#'
#' @examples
#' # Example data frame
#' my_data <- dummy_data(1000)
#'
#' # Split by variable expressions
#' split_var_df <- my_data |> split_by(sex)
#'
#' # Split by conditions
#' split_cond_df <- my_data |> split_by(sex == 1 & age <  18,
#'                                      sex == 2 & age >= 18)
#'
#' # Split by character conditions with 'SAS' like syntax
#' split_cond_df <- my_data |> split_by(" 15 <= age < 65 ",
#'                                      " education in ('low' 'middle') ")
#'
#' # Split by condition with inverse group
#' split_inv_df <- my_data |> split_by(sex == 1, inverse = TRUE)
#'
#' # Split by variables and conditions
#' split_combi_df <- my_data |> split_by(state, education,
#'                                       sex == 1, age < 18)
#'
#' # Split by variable expressions using formats
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
#' split_format_df <- my_data |> split_by(state,
#'                                        formats = list(state = state.))
#'
#' @export
split_by <- function(data_frame,
                     ...,
                     formats = list(),
                     inverse = FALSE,
                     monitor = .qol_options[["monitor"]]){
    # Measure the time
    print_start_message(suppress = TRUE)

    # Remove internal qol attributes so they don't leak into the result
    data_frame <- strip_qol_attributes(data_frame)

    #-------------------------------------------------------------------------#
    monitor_df <- NULL |> monitor_start("Preparation", "Preparation")
    #-------------------------------------------------------------------------#

    # Arguments which only consist of characters, symbols and vectors are converted
    # by dots_to_char(). If it fails, conditions were passed and the raw expressions
    # are taken instead, of which only the characters are parsed.
    conditions <- tryCatch(dots_to_char(...), error = function(e) NULL)

    if (is.null(conditions)){
        conditions <- unlist(lapply(as.list(substitute(list(...)))[-1], function(entry){
            if (is.character(entry)){
                lapply(entry, resolve_char, data_frame = data_frame)
            }
            else{
                list(entry)
            }
        }), recursive = FALSE)
    }
    else{
        conditions <- lapply(conditions, resolve_char, data_frame = data_frame)
    }

    # Omit variable names which are not part of the data frame
    invalid_names <- local({
        conditions <- vapply(conditions[vapply(conditions, is.name, logical(1))],
                             as.character, character(1))

        conditions[!conditions %in% (data_frame |> part_of_df(conditions))]
    })

    # Split data frame by conditions and store the split data frames into list
    data_list <- list()

    # Apply formats if there are any first
    formats_list <- formats

    if (!is_list_of_dfs(formats)){
        formats_list <- as.list(substitute(formats))[-1]
        formats_list <- evaluate_formats(formats_list, env = parent.frame())
    }

    # Resolve macro variables in the variable names of the provided formats
    names(formats_list) <- apply_macro(names(formats_list))

    if (length(formats_list) > 0){
        #-------------------------------------------------------------------------#
        monitor_df <- NULL |> monitor_start("Applying formats", "Formats")
        #-------------------------------------------------------------------------#

        data_frame <- suppressMessages(do.call(recode_multi, c(list(data_frame), formats_list, list(convert = FALSE))))
    }

    for (condition in conditions){
        #-------------------------------------------------------------------------#
        monitor_df <- monitor_df |> monitor_next(paste(deparse(condition), collapse = " "), "Condition")
        #-------------------------------------------------------------------------#

        # Split by value if a single variable name is provided
        if (is.name(condition) || is.character(condition)){
            condition <- as.character(condition)

            # Omit variables which are not part of the data frame
            if (condition %in% invalid_names){
                next
            }

            # Generate new split list with NA entries
            new_entry <- data_frame |> collapse::rsplit(by = data_frame[[condition]])

            if (NA %in% names(new_entry)){
                names(new_entry)[length(new_entry)] <- paste0("NA_", condition)
            }

            # Check for duplicate list entry names
            duplicate_entry <- intersect(names(data_list), names(new_entry))

            if (length(duplicate_entry) > 0){
                print_message("ERROR", c("Variable '[condition]' caused duplicate list entry names. Try to make them unique",
										 "using formats. Splitting will be aborted."), condition = condition)
                return(data_frame)
            }

            data_list <- c(data_list, new_entry)
        }
        # If a condition is provided split by it
        else if (is.language(condition)){
            # Evaluate the condition first to make it usable below
            real_condition <- eval(substitute(condition), envir = data_frame, enclos = parent.frame())

            # Add the subset or the inverse one as list element. The list element name is the condition iteself.
            if (!inverse){
                data_list <- c(data_list,
                               stats::setNames(list(data_frame |> collapse::fsubset(real_condition)),
                                               gsub('"', "'", paste(deparse(condition), collapse = " "))))
            }
            else{
                data_list <- c(data_list,
                               stats::setNames(list(data_frame |> collapse::fsubset(!real_condition)),
                                               paste0("not (", gsub('"', "'", paste(deparse(condition), collapse = " ")), ")")))
            }
        }
        # If something else is given abort
        else{
            print_message("ERROR", "Only single variables or conditions allowed. Splitting will be aborted.")
            return(data_frame)
        }
    }

    print_closing()

    #---------------------------------------------------------------------#
    monitor_df <- monitor_df |> monitor_end()
    monitor_df |> monitor_plot(draw_plot = monitor)
    #---------------------------------------------------------------------#

    data_list
}



#' Detect Character Conditions
#'
#' @description
#' Detect conditions passed as character and parse them.
#'
#' @param entry Current entry to check for character condition.
#' @param data_frame The data frame to split.
#'
#' @return
#' Returns entry as name or the parsed character condition.
#'
#' @noRd
resolve_char <- function(entry, data_frame){
    if (is.na(entry)){
        return(as.name(entry))
    }

    # If the entry is a column name, split by it directly
    if (entry %in% names(data_frame)){
        return(as.name(entry))
    }

    parsed <- parse_conditions(entry, na.rm = FALSE)

    # A bare identifier stays a variable name so that unknown variables
    # still produce the usual warning.
    if (is.name(parsed)){
        as.name(entry)
    }
    else{
        parsed
    }
}
