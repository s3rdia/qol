#' High Level Import From And Export To CSV And XLSX
#'
#' @name import_export
#'
#' @description
#' [import_data()]: A wrapper for [data.table::fread()] and [openxlsx2::wb_to_df()],
#' providing basic import functionality with minimal code.
#'
#' @param infile Full file path with extension to a csv or xlsx file to be imported.
#' @param sheet Only used in xlsx import. Which sheet of the workbook to import.
#' @param region Only used in xlsx import. Can either be an 'Excel' range like 'A1:BY27'
#' or the name of a named region.
#' @param data_frame A data frame to export.
#' @param outfile Full file path with extension. Allowed extensions are ".csv" and ".xlsx".
#' @param separator Only used in CSV-export. Defines the single character value separator.
#' @param decimal Only used in CSV-export. Defines the single character decimal character.
#' @param var_names TRUE by default. Whether to export variable names or not.
#'
#' @details
#' [import_data()] and [export_data()] are based on the 'SAS' procedures Proc Import and Proc Export,
#' which provide a very straight forward syntax. While 'SAS' can import many different formats with
#' these procedures, these 'R' versions concentrate on importing CSV and XLSX files.
#'
#' The main goal here is to just provide as few as possible parameters to tackle most of the imports
#' and exports. These error handling also tries to let an import and export happen, even though
#' a parameter wasn't provided in the correct way.
#'
#' @return
#' Returns a data frame.
#'
#' @seealso
#' Functions that can export with style: [frequencies()], [crosstabs()], [any_table()],
#' [export_with_style()].
#'
#' Creating a custom table style: [excel_output_style()], [modify_output_style()],
#' [number_format_style()], [modify_number_formats()].
#'
#' Global style options: [set_style_options()], [set_variable_labels()], [set_stat_labels()].
#'
#' @examples
#' # Example files
#' csv_file  <- system.file("extdata", "qol_example_data.csv",  package = "qol")
#' xlsx_file <- system.file("extdata", "qol_example_data.xlsx", package = "qol")
#'
#' # Import: Provide full file path
#' my_csv  <- import_data(csv_file)
#' my_xlsx <- import_data(xlsx_file)
#'
#' # Import specific regions
#' range_import <- import_data(xlsx_file, region = "B4:H32")
#' name_import  <- import_data(xlsx_file, region = "test_region")
#'
#' # Import from another sheet
#' sheet_import <- import_data(xlsx_file, sheet = "Sheet 2")
#'
#' @rdname import_export
#'
#' @export
import_data <- function(infile,
                        sheet     = 1,
                        region    = NULL,
                        separator = "auto",
                        decimal   = "auto",
                        var_names = TRUE){
    start_time <- Sys.time()

    ###########################################################################
    # Error handling
    ###########################################################################

    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # Path
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    # Abort on vector provided as path
    if (!is.character(infile) || length(infile) != 1){
        message(" X ERROR: <Infile> must be a single character. Import will be aborted.")

        return(invisible(NULL))
    }

    # Abort on invalid path
    if (!dir.exists(dirname(infile)) || dirname(infile) == "."){
        message(" X ERROR: Path does not exist: ", infile, "\n",
                "          Import will be aborted.")

        return(invisible(NULL))
    }

    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # File extension
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    extension <- tolower(tools::file_ext(infile))

    if (extension == ""){
        message(" X ERROR: No file extension provided in <infile>. 'csv' and 'xlsx' are allowed.\n",
                "          Import will be aborted.")

        return(invisible(NULL))
    }

    if (!extension %in% c("csv", "xlsx")){
        message(" X ERROR: Only 'csv' or 'xlsx' are allowed as file extensions in the <infile>.\n",
                "          Import will be aborted.")

        return(invisible(NULL))
    }

    ###########################################################################
    # Import
    ###########################################################################

    if (extension == "csv"){
        #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        # Separator
        #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

        # Separator may only consist of one character
        if (!is.character(separator)){
            message(" ! WARNING: <Separator> must be provided as character. Automatic detection will be used.")

            separator <- "auto"
        }

        if (length(separator) != 1L || (separator != "auto" && nchar(separator) != 1L)){
            message(" ! WARNING: <Separator> may only be one character. Automatic detection will be used.")

            separator <- "auto"
        }

        #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        # Decimal
        #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

        # Decimal may only consist of one character
        if (!is.character(decimal)){
            message(" ! WARNING: <Decimal> must be provided as character. Automatic detection will be used.")

            decimal <- "auto"
        }

        if (length(decimal) != 1L || (decimal != "auto" && nchar(decimal) != 1L)){
            message(" ! WARNING: <Decimal> may only be one character. Automatic detection will be used.")

            decimal <- "auto"
        }

        # Decimal may not be equal to separator
        if (separator != "auto" && decimal != "auto" && separator == decimal){
            message(" ! WARNING: <Decimal> may not be the same character as the <separator>. Automatic detection will be used.")

            decimal <- "auto"
        }

        #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        # Actual import
        #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

        data_frame <- data.table::fread(file   = infile,
                                        sep    = separator,
                                        dec    = decimal,
                                        header = var_names)
    }
    # xlsx
    else if (extension == "xlsx"){
        if (!is.null(region)){
            # Region may only consist of one character
            if (!is.character(region)){
                message(" ! WARNING: Region must be provided as character. Allowed are specific ranges like 'A1:BY27' or\n",
                        "            the names of named regions. The whole file will be read.")

                region <- NULL
            }

            if (length(region) != 1L){
                message(" ! WARNING: Only one character element allowed for region. The whole file will be read.")

                region <- NULL
            }
        }

        #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        # Actual import
        #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

        # If valid region is provided
        if (!is.null(region)){
            # If it's a specific region
            if (grepl(":", region, fixed = TRUE)){
                data_frame <- openxlsx2::wb_to_df(file            = infile,
                                                  sheet           = sheet,
                                                  dims            = region,
                                                  col_names       = var_names,
                                                  skip_empty_rows = TRUE,
                                                  skip_empty_cols = TRUE,
                                                  na.strings      = "")
            }
            # If it's a named region
            else{
                # Try to grab named region if it exists
                data_frame <- tryCatch({
                    openxlsx2::wb_to_df(file            = infile,
                                        sheet           = sheet,
                                        named_region    = region,
                                        col_names       = var_names,
                                        skip_empty_rows = TRUE,
                                        skip_empty_cols = TRUE,
                                        na.strings      = "")
                }, error = function(e){
                    # Read whole file on error. In this case the named region didn't exist in the file
                    message(" ! WARNING: Region '", region, "' doesn't exist in sheet '", sheet, "'.\n",
                            "            The whole file will be read.")

                    openxlsx2::wb_to_df(file            = infile,
                                        sheet           = sheet,
                                        col_names       = var_names,
                                        skip_empty_rows = TRUE,
                                        skip_empty_cols = TRUE,
                                        na.strings      = "")
                })
            }
        }
        # Otherwise read whole file
        else{
            data_frame <- openxlsx2::wb_to_df(file            = infile,
                                              sheet           = sheet,
                                              col_names       = var_names,
                                              skip_empty_rows = TRUE,
                                              skip_empty_cols = TRUE,
                                              na.strings      = "")
        }
    }

    # Convert xlsx files to data.table
    if (!data.table::is.data.table(data_frame)){
        data_frame <- data.table::as.data.table(data_frame)
    }

    end_time <- round(difftime(Sys.time(), start_time, units = "secs"), 3)
    message("\n- - - 'import_data' execution time: ", end_time, " seconds\n")

    invisible(data_frame)
}


#' @description
#' [import_multi()]: Runs multiple imports on all provided files. Is able to import
#' all sheets from xlsx files.
#'
#' @param file_list [import_multi()]: A character vector containing full file paths.
#'
#' [export_multi()]: A list of data frames.
#'
#' @return
#' Multi functions: Returns a list of data frames.
#'
#' @examples
#' # Import multiple files at once
#' all_files <- import_multi(c(csv_file, xlsx_file))
#'
#' @rdname import_export
#'
#' @export
import_multi <- function(file_list,
                         sheet     = "all",
                         region    = NULL,
                         separator = "auto",
                         decimal   = "auto",
                         var_names = TRUE){
    start_time <- Sys.time()

    # Loop through all files and import them one after another
    result_list <- list()

    for (i in seq_along(file_list)){
        infile <- file_list[[i]]

        filename  <- tools::file_path_sans_ext(basename(infile))
        extension <- tolower(tools::file_ext(infile))

        # For CSV files just do a simple import
        if (extension == "csv"){
            result_list[[filename]] <- suppressMessages(
                import_data(infile    = infile,
                            separator = separator,
                            decimal   = decimal,
                            var_names = var_names))
        }
        # For XLSX files there are two possible path to go
        else if (extension == "xlsx"){
            # If all sheets should be imported from a single file
            if (sheet == "all"){
                # Load file as a workbook first to be able to extract the sheet names
                wb <- openxlsx2::wb_load(infile)
                sheet_names <- openxlsx2::wb_get_sheet_names(wb)

                # Import all sheets one after another by name
                for (sheet_name in sheet_names){
                    result_list[[paste0(filename, "_", sheet_name)]] <-
                        suppressMessages(
                            import_data(infile    = infile,
                                        sheet     = sheet_name,
                                        region    = region,
                                        var_names = var_names))
                }
            }
            # If only a single sheet from each file should be imported, just do a
            # simple import
            else{
                result_list[[filename]] <- suppressMessages(
                    import_data(infile    = infile,
                                sheet     = sheet,
                                region    = region,
                                var_names = var_names))
            }
        }
    }

    end_time <- round(difftime(Sys.time(), start_time, units = "secs"), 3)
    message("\n- - - 'import_multi' execution time: ", end_time, " seconds\n")

    invisible(result_list)
}


#' @description
#' [export_data()]: A wrapper for [data.table::fwrite()] and [openxlsx2::wb_save()],
#' providing basic export functionality with minimal code.
#'
#' @examples
#' # Example data frame
#' my_data <- dummy_data(100)
#'
#' # Example export file paths
#' export_csv  <- tempfile(fileext = ".csv")
#' export_xlsx <- tempfile(fileext = ".xlsx")
#'
#' # Export: Provide full file path
#' my_data |> export_data(export_csv)
#' my_data |> export_data(export_xlsx)
#'
#' @rdname import_export
#'
#' @export
export_data <- function(data_frame,
                        outfile,
                        separator = ";",
                        decimal   = ",",
                        var_names = TRUE){
    start_time <- Sys.time()

    ###########################################################################
    # Error handling
    ###########################################################################

    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # Path
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    # Abort on vector provided as path
    if (!is.character(outfile) || length(outfile) != 1){
        message(" X ERROR: <Outfile> must be a single character. Export will be aborted.")

        return(invisible(data_frame))
    }

    # Abort on invalid path
    if (!dir.exists(dirname(outfile)) || dirname(outfile) == "."){
        message(" X ERROR: Path does not exist: ", outfile, "\n",
                "          Export will be aborted.")

        return(invisible(data_frame))
    }

    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # File extension
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    extension <- tolower(tools::file_ext(outfile))

    if (extension == ""){
        message(" ! WARNING: No file extension provided in <outfile>. 'csv' will be used.")

        outfile   <- paste0(outfile, ".csv")
        extension <- "csv"
    }

    if (!extension %in% c("csv", "xlsx")){
        message(" ! WARNING: Only 'csv' or 'xlsx' are allowed as file extensions in the <outfile>. 'csv' will be used.")

        outfile <- sub(extension, "csv", outfile, ignore.case = TRUE)
        extension <- "csv"
    }

    if (extension == "csv"){
        #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        # Separator
        #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

        # Separator may only consist of one character
        if (!is.character(separator)){
            message(" ! WARNING: <Separator> must be provided as character. ';' will be used.")

            separator <- ";"
        }

        if (length(separator) != 1L || nchar(separator) != 1L){
            message(" ! WARNING: <Separator> may only be one character. ';' will be used.")

            separator <- ";"
        }

        #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        # Decimal
        #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

        # Decimal may only consist of one character
        if (!is.character(decimal)){
            message(" ! WARNING: <Decimal> must be provided as character. ',' will be used.")

            decimal <- ","
        }

        if (length(decimal) != 1L || nchar(decimal) != 1L){
            message(" ! WARNING: <Decimal> may only be one character. ',' will be used.")

            decimal <- ","
        }

        # Decimal may not be equal to separator
        if (separator == decimal){
            message(" ! WARNING: <Decimal> may not be the same character as the <separator>. ',' will be used.")

            decimal <- ","
        }

    ###########################################################################
    # Export
    ###########################################################################

        data_frame |> data.table::fwrite(file = outfile,
                                         sep  = separator,
                                         dec  = decimal,
                                         col.names = var_names)
    }
    # xlsx
    else if (extension == "xlsx"){
        wb <- openxlsx2::wb_workbook()
        wb$add_worksheet()
        wb$add_data(x          = data_frame,
                    col_names  = var_names,
                    na.strings = "")

        # Add named region so there is no need to specify the exact region when importing back
        wb$add_named_region(dims = openxlsx2::wb_dims(rows = seq.int(1, collapse::fnrow(data_frame) + var_names),
                                                      cols = seq.int(1, collapse::fncol(data_frame))),
                            name = "data", local_sheet = TRUE)

        wb$save(file = outfile, overwrite = TRUE)
    }

    end_time <- round(difftime(Sys.time(), start_time, units = "secs"), 3)
    message("\n- - - 'export_data' execution time: ", end_time, " seconds\n")

    invisible(data_frame)
}


#' @description
#' [export_multi()]: Runs multiple exports on a list of data frames. Is able to export
#' to a single xlsx file with multiple sheets.
#'
#' @param out_path The file path where to save the exported files.
#' @param into_sheets TRUE by default. Whether to export all data frames into multiple
#' sheets of a single xlsx file or into separate xlsx files.
#'
#' @examples
#' # Example data frame list
#' my_list <- list(first  = dummy_data(10),
#'                 second = dummy_data(10))
#'
#' # Export multiple data frames into one xlsx file
#' # with multiple sheets
#' export_multi(my_list, tempdir())
#'
#' # Export multiple data frames into multiple xlsx files
#' export_multi(my_list, tempdir(), into_sheets = FALSE)
#'
#' # Export multiple data frames into multiple csv files
#' export_multi(my_list, tempdir(), separator = ";")
#'
#' # Manual cleanup for example
#' file1 <- file.path(tempdir(), "my_list.xlsx")
#' file2 <- file.path(tempdir(), "first.xlsx")
#' file3 <- file.path(tempdir(), "second.xlsx")
#' file4 <- file.path(tempdir(), "first.csv")
#' file5 <- file.path(tempdir(), "second.csv")
#'
#' unlink(c(export_csv, export_xlsx,
#'          file1, file2, file3, file4, file5))
#'
#' @rdname import_export
#'
#' @export
export_multi <- function(file_list,
                         out_path,
                         into_sheets = TRUE,
                         separator   = NULL,
                         decimal     = ",",
                         var_names   = TRUE){
    start_time <- Sys.time()

    if (into_sheets && is.null(separator)){
        wb <- openxlsx2::wb_workbook()
    }

    # Loop through all files and export them one after another
    for (i in seq_along(file_list)){
        data_frame <- file_list[[i]]
        filename   <- names(file_list)[[i]]

        # For CSV files just do a simple export
        if (!is.null(separator)){
            suppressMessages(export_data(data_frame,
                                         outfile   = paste0(out_path, "/", filename, ".csv"),
                                         separator = separator,
                                         decimal   = decimal,
                                         var_names = var_names))
        }
        # For XLSX files there are two possible path to go
        else{
            # If all data frames should be exported into the same file with multiple sheets
            if (into_sheets){
                # Fill all sheets one after another with data
                wb$add_worksheet(filename)
                wb$add_data(x          = data_frame,
                            col_names  = var_names,
                            na.strings = "")

                # Add named region so there is no need to specify the exact region when importing back
                wb$add_named_region(dims = openxlsx2::wb_dims(rows = seq.int(1, collapse::fnrow(data_frame) + var_names),
                                                              cols = seq.int(1, collapse::fncol(data_frame))),
                                    name = "data", local_sheet = TRUE)
            }
            # If only a single sheet from each file should be imported, just do a
            # simple import
            else{
                suppressMessages(export_data(data_frame,
                                             outfile   = paste0(out_path, "/", filename, ".xlsx"),
                                             separator = separator,
                                             decimal   = decimal,
                                             var_names = var_names))
            }
        }
    }

    if (into_sheets && is.null(separator)){
        filename <- get_origin_as_char(file_list, substitute(file_list))
        wb$save(file = paste0(out_path, "/", filename, ".xlsx"), overwrite = TRUE)
    }

    end_time <- round(difftime(Sys.time(), start_time, units = "secs"), 3)
    message("\n- - - 'export_multi' execution time: ", end_time, " seconds\n")

    invisible(file_list)
}
