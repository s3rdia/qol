#' Resolve Macro Variables In A Text
#'
#' @name macro
#'
#' @description
#' Variables written with a leading "&" inside a text will be resolved into the
#' character or numeric expression the corresponding object is holding.
#'
#' @param text A text containing macro variables to resolve.
#'
#' @return
#' [macro()]: Returns a character.
#'
#' @details
#' Macro variables in 'SAS' can be set up with %Let and can act as global accessible
#' variables. This in itself is nothing special to 'R' because basically every object
#' created outside a function is a global variable.
#'
#' To use these global objects within a text one has to use e.g. paste0("The current year is: ", year).
#' With the macro function one can write it like this: macro("The current year is &year").
#'
#' So where is the benefit? If implemented within a function, a parameter like "title" or
#' "footnote" in the tabulation functions, can resolve these variables without the need
#' of another function. You can just pass the character expression "The current year is &year"
#' and the implementation inside the function resolves the macro variable directly in place.
#'
#' @seealso
#' Functions that can make use of macro variables: [any_table()], [frequencies()],
#' [crosstabs()], [export_with_style()], [summarise_plus()], [transpose_plus()]
#'
#' Within the tabulation functions titles, footnotes, var_labels and stat_labels
#' can resolve macros. Additionally they can be used in the [any_table()] rows and
#' columns parameter, in the [summarise_plus()] types parameter and in the [transpose_plus()]
#' pivot parameter.
#'
#' @examples
#' # Resolving macro variable in single character
#' year <- 2026
#'
#' text <- macro("The current year is &year")
#'
#' # You can also combine multiple macro variables
#' some_variable <- "current"
#' current2026   <- "The current year is"
#'
#' text_combi <- macro("&&some_variable&year &year")
#'
#' @rdname macro
#'
#' @export
macro <- function(text){
    if (!is.character(text)){
        message(" X ERROR: <Text> must be a character. Macro will be aborted.")
        return(invisible(text))
    }

    if (length(text) > 1){
        message(" ! WARNING: <Text> may only be of length one. The first Element will be used.")

        text <- text[[1]]
    }

    # Macro variables start with "&" and consist of alphanumeric characters. There can be a "."
    # at the end of a macro variable to symbolize the end of the variable name.
    regex_pattern <- "&[a-zA-Z0-9_]+(\\.|\\b)"

    # Limit the number of iterations to avoid the case of getting stuck in an endless loop
    for (i in 1:5){
        # Get the starting positions of the macro variables by the pattern above
        macro_vars_position <- gregexpr(regex_pattern, text, perl = TRUE)[[1]]

        # If no matching pattern was found end the loop
        if (length(macro_vars_position) == 1){
            if (macro_vars_position == -1){
                break
            }
        }

        # Get the full macro variable names
        macro_var_names <- regmatches(text, list(macro_vars_position))[[1]]
        var_names       <- gsub("^&|\\.$", "", macro_var_names)

        # Create a map of unique matches of macro variables so that variables that
        # come up multiple times in the same text are only processed once.
        macro_mapping <- unique(data.frame(macro_var = macro_var_names,
                                           var_name  = var_names,
                                           stringsAsFactors = FALSE))

        for (var_number in seq_len(collapse::fnrow(macro_mapping))){
            macro_var <- macro_mapping[var_number, "macro_var"]
            var_name  <- macro_mapping[var_number, "var_name"]

            # Try to get the original value of the macro variable up the environmental ladder
            original_symbol <- tryCatch({
                # Force evaluation to see if it exists
                #get(var_name, envir = parent.frame(), inherits = TRUE)
                dynGet(var_name, ifnotfound = NULL, inherits = TRUE)
            }, error = function(e){
                # Evaluation failed
                NULL
            })

            # If the object was found
            if (!is.null(original_symbol)){
                # Individual error handling
                if (length(original_symbol) > 1){
                    message(" ! WARNING: Macro variable '", macro_var, "' may only be of length one. The first Element will be used.")

                    original_symbol <- original_symbol[[1]]
                }

                if (!is.character(original_symbol)){
                    # Silently convert numerics
                    if (is.numeric(original_symbol)){
                        original_symbol <- as.character(original_symbol)
                    }
                    # All other types will be skipped
                    else{
                        message(" ! WARNING: Macro variable '", macro_var, "' is a complex object. Macro variables may only be character or numeric.\n",
                                "            Resolving will be skipped.")

                        text <- gsub(macro_var, var_name, text, fixed = TRUE)
                        next
                    }
                }

                # Replace the macro variable with the original content of the found object
                text <- gsub(macro_var, original_symbol, text, fixed = TRUE)
            }
            # If the object wasn't found
            else{
                message(" ! WARNING: Macro variable '", macro_var, "' could not be resolved. Object doesn't exist.")

                text <- gsub(macro_var, var_name, text, fixed = TRUE)
            }
        }
    }

    text
}

#' @name macro
#'
#' @param character_vector A vector containing multiple character expressions in which
#' to resolve macro variables.
#'
#' @return
#' [apply_macro()]: Returns a character vector.
#'
#' @examples
#' # Resolving macro variable in character vector
#' char_vector <- c("The current year is &year",
#'                  "The &some_variable year is &year",
#'                  "&&some_variable&year &year")
#'
#' text_vector <- apply_macro(char_vector)
#'
#' @rdname macro
#'
#' @export
apply_macro <- function(character_vector){
    sapply(character_vector, macro, USE.NAMES = FALSE)
}
