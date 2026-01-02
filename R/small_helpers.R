#' Drop automatically generated Variables
#'
#' @description
#' If [summarise_plus()] is used with the nested options "all" or "single", three
#' variables are automatically generated: TYPE, TYPE_NR and DEPTH. With this functions
#' these variables are dropped.
#'
#' @param data_frame The data frame with automatically generated variables.
#'
#' @return
#' Returns a data frame without the variables TYPE, TYPE_NR and DEPTH.
#'
#' @examples
#' # Example format
#' sex. <- discrete_format(
#'     "Total"  = 1:2,
#'     "Male"   = 1,
#'     "Female" = 2)
#'
#' # Example data frame
#' my_data <- dummy_data(1000)
#'
#' # Call function
#' all_possible <- my_data |>
#'     summarise_plus(class      = c(year, sex),
#'                    values     = c(income, probability),
#'                    statistics = c("sum", "mean", "freq"),
#'                    formats    = list(sex = "sex."),
#'                    weight     = weight,
#'                    nesting    = "all",
#'                    na.rm      = TRUE) |>
#'     drop_type_vars()
#'
#' @export
drop_type_vars <- function(data_frame){
    data_frame |> dropp("TYPE", "TYPE_NR", "DEPTH")
}

#' Fuse Multiple Variables
#'
#' @description
#' When you have a situation where you have multiple variables with different NA
#' values that happen to be in different places (where one variable has a value the
#' other is NA and vice versa) you can fuse these together to a single variable.
#'
#' @param data_frame A data frame with variables to fuse.
#' @param new_variable_name The name of the new fused variable.
#' @param variables_to_fuse A vector with the variables that should be fused together.
#' @param drop_original_vars Whether to drop or keep the original values. TRUE by default.
#'
#' @return
#' Returns a data frame without the variables TYPE, TYPE_NR and DEPTH.
#'
#' @examples
#' # Example format
#' sex. <- discrete_format(
#'     "Total"  = 1:2,
#'     "Male"   = 1,
#'     "Female" = 2)
#'
#' # Example data frame
#' my_data <- dummy_data(1000)
#'
#' # Call function
#' all_possible <- my_data |>
#'     summarise_plus(class      = c(year, sex),
#'                    values     = c(income, probability),
#'                    statistics = c("sum", "mean", "freq"),
#'                    formats    = list(sex = "sex."),
#'                    weight     = weight,
#'                    nesting    = "all",
#'                    na.rm      = TRUE)
#'
#' all_possible <- all_possible[DEPTH <= 1] |>
#'     fuse_variables("fusion", c("year", "sex"))
#'
#' # NOTE: You can generally use this function to fuse variables. What is done in
#' #       multiple steps above can be achieved by just using nested = "single" in
#' #       summarise_plus.
#' single <- my_data |>
#'     summarise_plus(class      = c(year, sex),
#'                    values     = c(income, probability),
#'                    statistics = c("sum", "mean", "freq"),
#'                    formats    = list(sex = "sex."),
#'                    weight     = weight,
#'                    nesting    = "single",
#'                    na.rm      = TRUE)
#'
#' @export
fuse_variables <- function(data_frame,
                           new_variable_name,
                           variables_to_fuse,
                           drop_original_vars = TRUE){
    # Convert to character
    new_variable_name <- gsub("\"", "", deparse(substitute(new_variable_name)))

    if (length(new_variable_name) > 1){
        message(" X ERROR: No vector allowed. Only one new variable can be generated.")
        return(data_frame)
    }

    # Convert to character vectors
    variables_to_fuse <- get_origin_as_char(variables_to_fuse, substitute(variables_to_fuse))

    # Make sure that the variables provided are part of the data frame.
    variables_to_fuse <- data_frame |> part_of_df(variables_to_fuse)

    # Convert the group columns to character so coalesce works properly
    selected_data   <- data_frame[variables_to_fuse]
    selected_data[] <- lapply(selected_data, as.character)

    # Create a new variable with the first non-NA value from the group columns
    data_frame[[new_variable_name]] <- do.call(data.table::fcoalesce, selected_data)

    if (drop_original_vars){
        data_frame <- data_frame |> dropp(variables_to_fuse)
    }

    data_frame |> data.table::setcolorder(new_variable_name)
}


#' Order Columns by Variable Name Patterns
#'
#' @description
#' Order variables in a data frame based on a pattern rather than whole variable
#' names. E.g. grab every variable that contains "sum" in it's name and order
#' them together so that they appear next to each other.
#'
#' @param data_frame The data frame to be ordered.
#' @param pattern The pattern which is used for ordering the data frame columns.
#'
#' @return
#' Returns a reordered data frame with the ordered variables at the end.
#'
#' @examples
#' # Example data frame
#' my_data <- dummy_data(1000)
#'
#' # Summarise data
#' all_nested <- my_data |>
#'     summarise_plus(class      = c(year, sex),
#'                    values     = c(weight, income),
#'                    statistics = c("sum", "pct_group", "pct_total", "sum_wgt", "freq"),
#'                    weight     = weight,
#'                    nesting    = "deepest",
#'                    na.rm      = TRUE)
#'
#' # Set a different column order
#' new_order <- all_nested |> setcolorder_by_pattern(c("pct", "freq", "sum"))
#'
#' @export
setcolorder_by_pattern <- function(data_frame, pattern){
    # Copy data frame or else the original input data frame will be altered too
    data_frame <- data.table::copy(data_frame)

    # Select all variables in blocks of the requested pattern
    ordered_cols <- unlist(lapply(pattern, function(single_pattern){
        # Returns all variable names that match the provided pattern
        grep(single_pattern, names(data_frame),
             value = TRUE, ignore.case = TRUE)
    }))

    ordered_cols <- collapse::funique(ordered_cols)

    # Put the ordered columns at the end of the data frame
    data_frame |> data.table::setcolorder(ordered_cols, after = ncol(data_frame))
}


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
    if (!file.exists(path)){
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
        message("Filepaths successfully retrieved: ", path)
        return(stats::setNames(files, basename(files)))
    }

    message("Path successfully assigned: ", path)

    path
}


#' Stack Multiple Data Frames
#'
#' @description
#' Stacks multiple data frames and matches column names.
#'
#' @param ... Put in multiple data frames to stack them in the provided order.
#' @param id Adds an ID column to indicate the different data frames.
#' @param compress FALSE by default. If TRUE converts character variables to factors.
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
set <- function(..., id = FALSE, compress = FALSE){
    # Generate an ID variable and order it to the last position which indicates the single data frame
    if (id){
        data_frame <- data.table::rbindlist(list(...), use.names = TRUE, fill = TRUE, idcol = "ID")
        data_frame <- data_frame |> data.table::setcolorder("ID", after = ncol(data_frame))
    }
    # Stack without ID variable
    else{
        data_frame <- data.table::rbindlist(list(...), use.names = TRUE, fill = TRUE)
    }

    # Compress numeric values to integers if possible and conditionally convert characters to factors
    data_frame |> utils::type.convert(as.is = !compress)
}
