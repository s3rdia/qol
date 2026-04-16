#' Check If Path Exists And Retrieve Files
#'
#' @description
#' [libname()] checks if a given path exists and writes a message in the console accordingly.
#' Optional all files from the given path can be retrieved as a named character vector.
#'
#' @param path A folder path.
#' @param get_files FALSE by default. If TRUE returns a named character vector containing
#' file paths.
#' @param extensions Specify file extensions to be kept in the list when retrieving files.
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
                    get_files  = FALSE,
                    extensions = NULL){
    if (!file.exists(path) || dirname(path) == "."){
        print_message("ERROR", "Path does not exist: [path]", path = path)
        return(invisible(NULL))
    }

    print_start_message(suppress = TRUE)

    if (get_files){
		print_step("GREY", "Get files")

        # Retrieve all file paths from provided path
        files <- list.files(path, full.names = TRUE)

        # Strip paths and only keep file names with extension
        files <- files[!dir.exists(files)]

        if (length(files) == 0){
            print_message("ERROR", "No files found in directory: [path]", path = path)
            return(invisible(NULL))
        }

        # Return named character vector
        print_step("MAJOR", "Filepaths successfully retrieved: [path]", path = path)

        for (file in files){
            print_step("MINOR", basename(file))
        }

        # Return all files as list if no extensions where specified
        if (is.null(extensions)){
            print_closing(suppress = TRUE)
            return(invisible(stats::setNames(files, basename(files))))
        }
        # Filter extensions
        else{
            file_list <- stats::setNames(files, basename(files))

            # Create pattern to grab all extensions at once
            pattern <- paste0("(", paste(gsub("\\.", "\\\\.", extensions), collapse = "|"), ")$")

            print_step("MAJOR", "Only keeping files with extension[?s]: [extensions]", extensions = extensions)
            print_closing(suppress = TRUE)

            return(invisible(file_list[grepl(pattern, file_list, ignore.case = TRUE)]))
        }
    }

    print_step("MAJOR", "Path successfully assigned: [path]", path = path)
    print_closing(suppress = TRUE)

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
set <- function(...,
                id            = FALSE,
                compress      = NULL,
                guessing_rows = 100){
    # Measure the time
    print_start_message(suppress = TRUE)

    # Translate ... into a list if possible
    df_list <- tryCatch({
        # Force evaluation to see if it exists
        list(...)
    }, error = function(e){
        # Evaluation failed
        NULL
    })

    if (is.null(df_list)){
        print_message("ERROR", "Unknown object found. Retaining will be aborted.")
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

    print_closing()

    data_frame
}


#' Save And Load FST And RDS Files
#'
#' @name save_load
#'
#' @description
#' [save_file()]: Saves fst and rds files. Offers variable selection and observation
#' subsetting. By default the function has a write protection, which has to be
#' explicitly turned off to be able to overwrite files.
#'
#' @param data_frame The data fame to be saved.
#' @param path The file path to save to/load from.
#' @param file The file name including the file extension.
#' @param keep The variables to keep in the file.
#' @param where A condition on which to subset the observations.
#' @param compress The amount of compression to use from 0 to 100. The lower the value,
#' the larger the file size and faster the saving speed. Uses maximum compression by
#' default.
#' @param protect TRUE by default. Throws an error if a file already exists. If FALSE,
#' overwrites existing files.
#' @param ... Used internally to suppress messages, when using the multi save/load version.
#'
#' @return
#' Returns a data frame. [load_file_multi()] can also return a list of data frame.
#'
#' @examples
#' # Example data frame
#' my_data <- dummy_data(100)
#'
#' # Save files
#' # NOTE: Normally you would pass in the path and file as character. For the
#' #       examples this is handled differently to provide runnable examples.
#' my_data |> save_file(path = tempdir(),
#'                      file = "testfile.fst")
#' my_data |> save_file(path = tempdir(),
#'                      file = "testfile.rds")
#'
#' # Save file and only keep specific variables
#' # NOTE: Since the temporary file already exists now if you run the above code,
#' #       all the following save operations would throw errors because by default
#' #       the function write protects existing files. So for the following examples
#' #       the write protection is turned off.
#' my_data |> save_file(path    = tempdir(),
#'                      file    = "testfile.fst",
#'                      keep    = c(sex, age, state),
#'                      protect = FALSE)
#'
#' # Save file and subset observations
#' my_data |> save_file(path    = tempdir(),
#'                      file    = "testfile.fst",
#'                      where   = sex == 1 & age > 65,
#'                      protect = FALSE)
#'
#' @rdname save_load
#'
#' @export
save_file <- function(data_frame,
                      path,
                      file,
                      keep     = NULL,
                      where    = NULL,
                      compress = 100,
                      protect  = TRUE,
                      ...){
    print_start_message(suppress = TRUE)
    suppress <- ifelse(is.null(unlist(list(...))), FALSE, TRUE)

    ###########################################################################
    # Error handling
    ###########################################################################

    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # File extension
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    extension <- tolower(tools::file_ext(file))

    if (extension == ""){
        print_message("WARNING", "No file extension provided in <file>. 'fst' will be used.")

        file      <- paste0(file, ".fst")
        extension <- "fst"
    }

    if (!extension %in% c("fst", "rds")){
        print_message("WARNING", c("Only 'fst' or 'rds' are allowed as file extensions in <file>.",
								   "'fst' will be used."))

        file      <- paste0(tools::file_path_sans_ext(file), ".fst")
        extension <- "fst"
    }

    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # File path
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    full_path <- file.path(path, file)

    if (file.exists(full_path) && protect){
        print_message("ERROR", c("File already exists: [full_path]",
								 "Set <protect> to FALSE to overwrite files."), full_path = full_path)

        return(invisible(data_frame))
    }

    if (!file.exists(path)){
        print_message("ERROR", "Path does not exist: [full_path]", full_path = full_path)

        return(invisible(data_frame))
    }

    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # Keep
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    # Convert to character vectors
    keep_eval <- substitute(keep)

    if (is.call(keep_eval)){
        keep <- get_origin_as_char(keep, keep_eval)
    }

    # Check if variables exist in the data frame. If not, abort.
    keep <- data_frame |> part_of_df(keep, check_only = TRUE)

    if (is.list(keep)){
        print_message("ERROR", c("The provided variables to <keep> '[keep_vars]' are not part of",
								 "the data frame. Saving will be aborted."), keep_vars = keep[[1]])
        return(invisible(data_frame))
    }

    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # Compress
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    compress <- min(100, max(0, compress))

    ###########################################################################
    # Saving
    ###########################################################################

    # Select and subset data frame
    if (!is.null(keep)){
        data_frame <- suppressMessages(data_frame |>
                                           collapse::fselect(keep) |>
                                           data.table::setcolorder(keep) |>
                                           if.(where))
    }
    else{
        data_frame <- suppressMessages(data_frame |> if.(where))
    }

    if (extension == "fst"){
        suppressMessages(fst::threads_fst(.qol_options[["threads"]]))

        # Save data frame
        suppressWarnings(suppressMessages(
            data_frame |> fst::write_fst(full_path, compress = compress)))
    }
    if (extension == "rds"){
        data_frame |> saveRDS(full_path, compress = compress == 100)
    }

    # Revert used threads
    suppressMessages(fst::threads_fst(NULL))

    if (extension == "fst"){
        print_closing(5)
    }
    else{
        print_closing(15)
    }

    invisible(data_frame)
}


#' @description
#' [save_file_multi()]: Saves multiple files.
#'
#' @param data_frame_list A list of data frames.
#' @param file_list A character vector containing full file paths.
#' @param keep_list Can be a single variable name, a vector or a list of vectors containing
#' the variables to keep per file. If there are fewer list entries than files to load, the
#' last list element will be used repeatedly.
#'
#' @examples
#' # Example lists
#' my_df_list <- list(dummy_data(10),
#'                    dummy_data(10))
#'
#' file1 <- file.path(tempdir(), "first.fst")
#' file2 <- file.path(tempdir(), "second.rds")
#' my_file_list <- list(file1, file2)
#'
#' # Save multiple files at once
#' save_file_multi(data_frame_list = my_df_list,
#'                 file_list       = my_file_list,
#'                 protect         = FALSE)
#'
#' # Save multiple files and only keep specific variables
#' save_file_multi(data_frame_list = my_df_list,
#'                 file_list       = my_file_list,
#'                 keep_list       = c(sex, age, state),
#'                 protect         = FALSE)
#'
#' # Save multiple files and keep different variables per data frame
#' save_file_multi(data_frame_list = my_df_list,
#'                 file_list       = my_file_list,
#'                 keep_list       = list(c(person_id, first_person),
#'                                        c(NUTS3, income, weight)),
#'                 protect         = FALSE)
#'
#' unlink(c(file1, file2,
#'          file.path(tempdir(), "testfile.fst"),
#'          file.path(tempdir(), "testfile.rds")))
#'
#' @rdname save_load
#'
#' @export
save_file_multi <- function(data_frame_list,
                            file_list,
                            keep_list = NULL,
                            compress  = 100,
                            protect   = TRUE){
    print_start_message()

    if (length(data_frame_list) != length(file_list)){
        print_message("ERROR", "Data frame and file list are of unequal lengths. Saving will be aborted.")

        return(invisible(file_list))
    }

    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # Keep
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    # This whole block enables the user to pass different things into keep_list
    keep_eval <- substitute(keep_list)

    # If a single variable name is passed, transform into a list with one character element
    if (is.name(keep_eval)){
        keep_list <- list(get_origin_as_char(keep_list, substitute(keep_list)))
    }
    # If a vector is passed, transform into a list with one character vector
    else if (is.call(keep_eval) && identical(keep_eval[[1]], as.name("c"))){
        keep_list <- list(get_origin_as_char(keep_list, substitute(keep_list)))
    }
    # If a list is passed, make sure it is a list of character vectors
    else if (is.call(keep_eval)){
        # Loop through each list entry and convert vectors to characters
        keep_list <- lapply(as.list(keep_eval)[-1], function(element){
            variables <- as.list(element)[-1]

            # In case of a list of symbols. Variable names are passed without quotation marks.
            if (all(sapply(variables, is.symbol))){
                all.vars(element)
            }
            # In case of a list of character vectors. Variable names are passed with quotation marks.
            else{
                unlist(variables)
            }
        })
    }

    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # Loading
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    # Loop through all files and import them one after another
    length_keep <- length(keep_list)

    print_step("MAJOR", "Saving files")

    # Loop through all files and load them in one after another
    for (i in seq_along(file_list)){
        infile  <- data_frame_list[[i]]
        outfile <- file_list[[i]]

        print_step("MINOR", "{outfile}", outfile = outfile)

        # If there are variables to keep
        if (!is.null(keep_eval)){
            # Iterate through the keep list along with the files
            if (i < length_keep){
                vars_to_keep <- keep_list[[i]]
            }
            # If the iterator is bigger than the number of list elements, just
            # use the last list element for every other iteration.
            else{
                vars_to_keep <- keep_list[[length_keep]]
            }
        }
        else{
            vars_to_keep <- NULL
        }

        # Load file and add to list
        save_file(data_frame = infile,
                  path       = dirname(outfile),
                  file       = basename(outfile),
                  keep       = vars_to_keep,
                  compress   = compress,
                  protect    = protect,
                  suppress   = TRUE)
    }

    # Revert used threads
    suppressMessages(fst::threads_fst(NULL))

    print_closing(15)

    invisible(file_list)
}


#' @description
#' [load_file()]: Loads fst and rds files. Provided variables to keep are read in case
#' insensitive and are returned in provided order. Additionally a subset can be defined
#' directly.
#'
#' @examples
#' # Example files
#' fst_file <- system.file("extdata", "qol_example_data_fst.fst", package = "qol")
#' rds_file <- system.file("extdata", "qol_example_data_rds.rds", package = "qol")
#'
#' # Load file
#' my_fst <- load_file(path = dirname(fst_file),
#'                     file = basename(rds_file))
#' my_rds <- load_file(path = dirname(rds_file),
#'                     file = basename(rds_file))
#'
#' # Load file and only keep specific variables
#' # NOTE: Variable names can be written case insensitive. Meaning if a variable
#' #       is stored as "age" and you write "AGE" in keep, the function will find
#' #       the variable and rename it to "AGE".
#' my_fst_keep<- load_file(path = dirname(fst_file),
#'                         file = basename(rds_file),
#'                         keep = c(AGE, INCOME_class, State, weight))
#'
#' # Load file and subset observations
#' my_fst_where <- load_file(path  = dirname(fst_file),
#'                           file  = basename(rds_file),
#'                           where = sex == 1 & age > 65)
#'
#' @rdname save_load
#'
#' @export
load_file <- function(path,
                      file,
                      keep  = NULL,
                      where = NULL,
                      ...){
    print_start_message(suppress = TRUE)
    suppress <- ifelse(is.null(unlist(list(...))), FALSE, TRUE)

    ###########################################################################
    # Error handling
    ###########################################################################

    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # File path
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    full_path <- file.path(path, file)

    if (!file.exists(full_path)){
        print_message("ERROR", "File does not exist: [full_path]", full_path = full_path)

        return(invisible(NULL))
    }

    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # File extension
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    extension <- tolower(tools::file_ext(file))

    if (extension == ""){
        print_message("ERROR", c("No file extension provided in <file>. 'fst' and 'rds' are allowed.",
								 "Loading file will be aborted."))

        return(invisible(NULL))
    }

    if (!extension %in% c("fst", "rds")){
        print_message("ERROR", c("Only 'fst' or 'rds' are allowed as file extensions in <file>.",
								 "Loading file will be aborted."))

        return(invisible(NULL))
    }

    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # Keep
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    # Convert to character vectors
    keep_eval <- substitute(keep)

    if (is.call(keep_eval)){
        keep <- get_origin_as_char(keep, keep_eval)
    }

    ###########################################################################
    # Loading
    ###########################################################################

    if (extension == "fst"){
        suppressMessages(fst::threads_fst(.qol_options[["threads"]]))

        if (!is.null(keep)){
            # Get metadata first to be able to read in the variable names case insensitive
            meta_data      <- suppressMessages(fst::metadata_fst(full_path))
            variable_names <- meta_data[["columnNames"]]

            # Check if variables exist in the meta data
            invalid_keep <- keep[!tolower(keep) %in% tolower(variable_names)]

            if (length(invalid_keep) > 0){
                print_message("WARNING", "Variables not found: [invalid]", invalid = invalid_keep)
            }

            # Use fast match to identify existing columns
            matching_ids <- collapse::fmatch(tolower(keep), tolower(variable_names))
            valid_ids    <- !is.na(matching_ids)

            # Select existing variables
            vars_to_keep  <- variable_names[matching_ids[valid_ids]]
            keep_names    <- keep[valid_ids]

            # Read in existing variables
            data_frame <- suppressMessages(fst::read_fst(full_path,
                                                         columns       = vars_to_keep,
                                                         as.data.table = TRUE))

            # Rename variables to the names provided in keep
            data_frame |> data.table::setnames(vars_to_keep, keep_names)
        }
        # Without keep read in the whole data set
        else{
            data_frame <- suppressWarnings(suppressMessages(
                fst::read_fst(full_path, as.data.table = TRUE)))
        }
    }
    if (extension == "rds"){
        # RDS format is not able to read in only specific variables. Meaning the
        # whole file has to be read in first.
        data_frame <- data.table::as.data.table(readRDS(full_path))

        if (!is.null(keep)){
            variable_names <- names(data_frame)

            # Check if variables exist in the meta data
            invalid_keep <- variable_names[!tolower(keep) %in% tolower(variable_names)]

            if (length(invalid_keep) > 0){
                print_message("WARNING", "Variables not found: [invalid]", invalid = invalid_keep)
            }

            # Use fast match to identify existing columns
            matching_ids <- collapse::fmatch(tolower(keep), tolower(variable_names))
            valid_ids    <- !is.na(matching_ids)

            # Read in existing variables
            vars_to_keep  <- variable_names[matching_ids[valid_ids]]
            keep_names    <- keep[valid_ids]
            invalid_names <- keep[is.na(vars_to_keep)]

            if (length(invalid_names) > 0){
                print_message("WARNING", "Variables not found: [invalid]", invalid = missing)
            }

            # Only keep the desired variables and rename variables to the names provided in keep
            data_frame <- data_frame |>
                collapse::fselect(vars_to_keep) |>
                data.table::setcolorder(vars_to_keep)

            data_frame |> data.table::setnames(vars_to_keep, keep_names)
        }
    }

    data_frame <- suppressMessages(data_frame |> if.(where))

    # Revert used threads
    suppressMessages(fst::threads_fst(NULL))

    if (extension == "fst"){
        print_closing(5, suppress = suppress)
    }
    else{
        print_closing(15, suppress = suppress)
    }

    invisible(data_frame)
}


#' @description
#' [load_file_multi()]: Loads multiple files and stores them into a list or directly
#' stacks the files into one data frame.
#'
#' @param stack_files TRUE by default. Stacks data frames after loading them. If FALSE,
#' returns all data frames in a list.
#'
#' @examples
#' # Load multiple files and stack them
#' stack_files <- load_file_multi(c(fst_file, rds_file))
#'
#' # Load multiple files and output them in a list
#' list_files <- load_file_multi(file_list   = c(fst_file, rds_file),
#'                               stack_files = FALSE)
#'
#' # Load multiple files and only keep specific variables
#' all_files_keep <- load_file_multi(file_list = c(fst_file, rds_file),
#'                                   keep_list = c(Sex, AGE, stAte))
#'
#' # Load multiple files and keep different variables per data frame
#' all_files_diff <- load_file_multi(file_list = c(fst_file, rds_file),
#'                                   keep_list = list(c(Person_ID, First_Person),
#'                                                    c(nuts3, Income, WEIGHT)))
#'
#' @rdname save_load
#'
#' @export
load_file_multi <- function(file_list,
                            keep_list   = NULL,
                            stack_files = TRUE){
    print_start_message()

    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # Keep
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    # This whole block enables the user to pass different things into keep_list
    keep_eval <- substitute(keep_list)

    # If a single variable name is passed, transform into a list with one character element
    if (is.name(keep_eval)){
        keep_list <- list(get_origin_as_char(keep_list, substitute(keep_list)))
    }
    # If a vector is passed, transform into a list with one character vector
    else if (is.call(keep_eval) && identical(keep_eval[[1]], as.name("c"))){
        keep_list <- list(get_origin_as_char(keep_list, substitute(keep_list)))
    }
    # If a list is passed, make sure it is a list of character vectors
    else if (is.call(keep_eval)){
        # Loop through each list entry and convert vectors to characters
        keep_list <- lapply(as.list(keep_eval)[-1], function(element){
            variables <- as.list(element)[-1]

            # In case of a list of symbols. Variable names are passed without quotation marks.
            if (all(sapply(variables, is.symbol))){
                all.vars(element)
            }
            # In case of a list of character vectors. Variable names are passed with quotation marks.
            else{
                unlist(variables)
            }
        })
    }

    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # Loading
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    # Loop through all files and import them one after another
    length_keep <- length(keep_list)
    result_list <- list()

    print_step("MAJOR", "Loading files")

    # Loop through all files and load them in one after another
    for (i in seq_along(file_list)){
        infile <- file_list[[i]]

        print_step("MINOR", "{infile}", infile = infile)

        # If there are variables to keep
        if (!is.null(keep_eval)){
            # Iterate through the keep list along with the files
            if (i < length_keep){
                vars_to_keep <- keep_list[[i]]
            }
            # If the iterator is bigger than the number of list elements, just
            # use the last list element for every other iteration.
            else{
                vars_to_keep <- keep_list[[length_keep]]
            }
        }
        else{
            vars_to_keep <- NULL
        }

        # Load file and add to list
        result_list[[basename(infile)]] <- load_file(path = dirname(infile),
                                                     file = basename(infile),
                                                     keep = vars_to_keep,
                                                     suppress = TRUE)
    }

    # Stack data frames
    if (stack_files){
        result_list <- suppressMessages(do.call(set, result_list))
    }

    # Revert used threads
    suppressMessages(fst::threads_fst(NULL))

    print_closing(15)

    invisible(result_list)
}
