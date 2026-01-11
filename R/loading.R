#' Check If Path Exists And Retrieve Files
#'
#' @description
#' [libname()] checks if a given path exists and writes a message in the console accordingly.
#' Optional all files from the given path can be retrieved as a named character vector.
#'
#' @param path A folder path.
#' @param get_files FALSE by default. If TRUE returns a named character vector containing file paths.
#'
#' @return
#' Returns the given file path or a named character vector containing file paths.
#'
#' @examples
#' my_path   <- libname("C:/My_Path/")
#' file_list <- libname("C:/My_Path/", get_files = TRUE)
#'
#' @export
libname <- function(path,
                    get_files = FALSE){
    if (!file.exists(path) || dirname(path) == "."){
        message(" X ERROR: Path does not exist: ", path)
        return(invisible(NULL))
    }

    if (get_files){
        # Retrieve all file paths from provided path
        files <- list.files(path, full.names = TRUE)

        # Strip paths and only keep file names with extension
        files <- files[!dir.exists(files)]

        if (length(files) == 0){
            message(" X ERROR: No files found in directory: ", path)
            return(invisible(NULL))
        }

        # Return named character vector
        message(" > Filepaths successfully retrieved: ", path)
        message(paste("   + ", basename(files), collapse = "\n"))
        return(stats::setNames(files, basename(files)))
    }

    message(" > Path successfully assigned: ", path)

    path
}


#' Stack Multiple Data Frames
#'
#' @description
#' Stacks multiple data frames and matches column names.
#'
#' @param ... Put in multiple data frames to stack them in the provided order.
#' @param id Adds an ID column to indicate the different data frames.
#' @param compress No compression by default. If compression receives any value, [set()]
#' will convert character variables to numeric or integer where possible. If set to "factor"
#' all non numeric character variables will be converted to factors.
#' @param guessing_rows 100 by default. [set()] takes a sample of rows to determine of
#' which type variables are.
#'
#' @return
#' Returns a stacked data frame.
#'
#' @examples
#' # Example data frames
#' my_data1 <- dummy_data(100)
#' my_data2 <- dummy_data(100)
#' my_data3 <- dummy_data(100)
#' my_data4 <- dummy_data(100)
#' my_data5 <- dummy_data(100)
#'
#' # Stack data frames
#' stacked_df <- set(my_data1,
#'                   my_data2,
#'                   my_data3,
#'                   my_data4,
#'                   my_data5)
#'
#' @export
set <- function(..., id = FALSE, compress = NULL, guessing_rows = 100){
    # Measure the time
    start_time <- Sys.time()

    # Translate ... into a list if possible
    df_list <- tryCatch({
        # Force evaluation to see if it exists
        list(...)
    }, error = function(e) {
        # Evaluation failed
        NULL
    })

    if (is.null(df_list)){
        message('X ERROR: Unknown object found. Retaining will be aborted.')
        return(invisible(NULL))
    }

    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # Row binding
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    # Generate an ID variable and order it to the last position which indicates the single data frame
    if (id){
        data_frame <- data.table::rbindlist(df_list, use.names = TRUE, fill = TRUE, idcol = "ID")
        data_frame <- data_frame |> data.table::setcolorder("ID", after = ncol(data_frame))
    }
    # Stack without ID variable
    else{
        data_frame <- data.table::rbindlist(df_list, use.names = TRUE, fill = TRUE)
    }

    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # Compression
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    # Compress numeric values to integers if possible and conditionally convert characters to factors
    if (!is.null(compress)){
        columns <- names(data_frame)

        for (column in columns){
            single_column <- data_frame[[column]]

            # Check all character columns
            if (is.character(single_column)){
                # Take a sample of the first x non-NA values to guess type
                sample_column <- single_column |>
                    collapse::na_omit() |>
                    collapse::fsubset(1:min(guessing_rows))

                if (length(sample_column) == 0){
                    next
                }

                # Try to convert to check whether it is numeric
                is_numeric <- suppressWarnings(!any(is.na(as.numeric(sample_column))))

                if (is_numeric){
                    # Check for decimals to decide between integer or double
                    has_decimals <- any(grepl("\\.", sample_column))

                    # Convert
                    if (!has_decimals){
                        data.table::set(data_frame, j = column, value = as.integer(single_column))
                    }
                    else{
                        data.table::set(data_frame, j = column, value = as.numeric(single_column))
                    }
                }
                # Convert character values to factors
                else if (compress == "factor"){
                    data.table::set(data_frame, j = column, value = as.factor(single_column))
                }
            }
            # Check if numeric columns can be converted to integer
            else if (is.numeric(single_column) && !is.integer(single_column) && !is.factor(single_column)){
                # Only convert to integer if it doesn't result in data loss
                if (all(single_column == floor(single_column), na.rm = TRUE)){
                    data.table::set(data_frame, j = column, value = as.integer(single_column))
                }
            }
        }
    }

    end_time <- round(difftime(Sys.time(), start_time, units = "secs"), 3)
    message("- - - 'set' execution time: ", end_time, " seconds")

    data_frame
}
