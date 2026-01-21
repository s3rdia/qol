#' Concatenate Multiple Variables With Padding
#'
#' @description
#' Concatenate multiple variables inside a data frame into a new variable. An
#' automatic or individual padding can be applied. The padding character can be
#' chosen freely.
#'
#' The function can also be used to give a single variable a padding.
#'
#' @param data_frame A data frame which contains the the variables to concatenate.
#' @param ... The names of the variables to concatenate.
#' @param padding_char A single character which will be used to fill up the empty places.
#' @param padding_length A numeric vector containing the individual padding length per variable.
#' @param padding_right FALSE by default. If TRUE insert padding characters on the right side
#' instead of the left side.
#'
#' @return
#' Returns a character vector.
#'
#' @seealso
#' Other character manipulating functions: Coming soon...
#'
#' @examples
#' # Example data frame
#' my_data <- dummy_data(100)
#'
#' # Concatenate variables as provided
#' my_data[["id1"]] <- my_data |> concat(household_id, state, age)
#'
#' # Concatenate variables with leading zeros. Each variable will
#' # receive an individual padding length according to their
#' # longest value.
#' my_data[["id2"]] <- my_data |> concat(household_id, state, age,
#'                                       padding_char = "0")
#'
#' # Concatenate variables with individual character and lengths.
#' my_data[["id2"]] <- my_data |> concat(household_id, state, age,
#'                                       padding_char   = "_",
#'                                       padding_length = c(5, 3, 4))
#'
#' # Padding a single variable in place
#' my_data[["state"]] <- my_data |> concat(state, padding_char = "0")
#'
#' @export
concat <- function(data_frame,
                   ...,
                   padding_char   = NULL,
                   padding_length = NULL,
                   padding_right  = FALSE){
    variables <- dots_to_char(...)

    # If no padding is defined just concatenate provided variables as they are
    if (is.null(padding_char) && is.null(padding_length)){
        return(do.call(paste0, data_frame[variables]))
    }

    # Padding character may only be of length one
    if (!is.null(padding_char) && collapse::vlengths(padding_char) != 1){
        message(" ! WARNING: <Padding chararacter> must be a single character. Concat will be done without <padding character>.")
        return(do.call(paste0, data_frame[variables]))
    }

    # If no padding character is given use a blank
    if (is.null(padding_char)){
        padding_char <- " "
    }

    # Determine padding lengths. If NULL all given variables will be checked individually
    # for their maximum length.
    if (is.null(padding_length)){
        padding_length <- vapply(data_frame[variables],
                                 function(variable){
                                     collapse::fmax(nchar(variable), na.rm = TRUE)
                                 },
                                 integer(1))
    }
    # With given padding length it has to be checked whether the provided vector
    # is to long or to short in comparison to the number of provided variables.
    # Either way it has to be matched to the number of provided variables. Only if
    # it is of equal length nothing happens here.
    else{
        number_of_columns  <- length(variables)
        number_of_paddings <- length(padding_length)

        # If the padding vector is shorter than the number of provided variables,
        # the remaining variables will receive the individual maximum length as
        # padding length like above.
        if (number_of_paddings < number_of_columns){
            message(" ! WARNING: <Padding length> is shorter than the number of variables.\n",
                    "            Missing lengths will be filled up using maximum individual variable length.")

            missing_length <- vapply(data_frame[variables[(number_of_paddings + 1):number_of_columns]],
                                     function(variable){
                                         collapse::fmax(nchar(variable), na.rm = TRUE)
                                     },
                                     integer(1))

            padding_length <- c(padding_length, missing_length)
        }
        # If the padding vector is longer than the number of provided variables,
        # juts trim it down.
        else if (number_of_paddings > number_of_columns){
            message(" ! WARNING: <Padding length> is longer than the number of variables.\n",
                    "            Extra lengths will be ignored.")

            padding_length <- padding_length[seq_len(number_of_columns)]
        }
    }

    # Apply padding to all variables individually
    padded_variables <- Map(function(variable, padding_length){

            # NA values are just filled up with padding character
            variable[is.na(variable)] <- strrep(padding_char, padding_length)

            # Get the number of places to fill up with padding character per observation
            pad_per_observation <- pmax(padding_length - collapse::vlengths(variable), 0)

            # Concatenate padding and variable values
            if (!padding_right){
                paste0(strrep(padding_char, pad_per_observation), variable)
            }
            else{
                paste0(variable, strrep(padding_char, pad_per_observation))
            }
        },
        data_frame[variables],
        padding_length
    )

    # Concatenate all padded variables together to one single variable
    do.call(paste0, padded_variables)
}
