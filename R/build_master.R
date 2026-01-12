#' Build a Master Script From Folder
#'
#' @description
#' [build_master()] reads a given folder structure, which contains scripts, and builds
#' a master script as a markdown file.
#'
#' @param dir The folder structure which contains the scripts to build upon.
#' @param master_name The file name which should be written.
#' @param author Authors name to be put in the header.
#' @param with_structure Whether the folder structure as tree should be written
#' to the master script.
#' @param with_run_all Whether a section, which let's the user run all scripts,
#' should be written to the master script.
#' @param with_run_folder Whether a section, which let's the user run all scripts from a
#' specific folder, should be written to the master script.
#'
#' @details
#' The function works with folder structures that look like this:
#'
#' root/
#'
#'      subfolder1/
#'
#'          script1.R
#'
#'          script2.R
#'
#'          ....R
#'
#'      subfolder2/
#'
#'          script3.R
#'
#'          script4.R
#'
#'          ....R
#'
#'      .../
#'
#'          ....R
#'
#' @return
#' Returns the script as character vector and saves it as markdown file.
#'
#' @examples
#' # Example export file paths
#' # NOTE: These tempfiles are only for the examples. In reality you just call the
#' # main function and put in your desired path and name directly.
#' temp_file <- tempfile(fileext = ".rstheme")
#' file_name <- basename(tools::file_path_sans_ext(temp_file))
#'
#' # Example master
#' build_master(dir         = dirname(temp_file),
#'              master_name = file_name)
#'
#' # Manual cleanup for example
#' unlink(temp_file)
#'
#' @export
build_master <- function(dir,
                         master_name     = "Master",
                         author          = "",
                         with_structure  = TRUE,
                         with_run_all    = TRUE,
                         with_run_folder = TRUE){
    # Measure the time
    start_time <- Sys.time()

    # Check if folder exists
    if (!dir.exists(dir) || dirname(dir) == "."){
        message(" X ERROR: Directory '", dir, "' does not exist.")
        return(invisible(NULL))
    }

    path <- ifelse(grepl("/$", dir), dir, paste0(dir, "/"))

    # Get folders in provided directory
    folders <- list.dirs(dir, recursive = TRUE, full.names = TRUE)

    # Get all .R scripts inside the folders
    scripts <- lapply(folders, function(folder){
        libname(folder, get_files = TRUE)
    })
    names(scripts) <- folders
    scripts        <- Filter(Negate(is.null), scripts)

    # Setup header
    lines <- c(
        "################################################################################",
        paste0("# ", master_name),
        "#",
        paste0("# Author: ", author),
        "#",
        paste0("# Date: ", format(Sys.Date(), "%d.%m.%Y")),
        "################################################################################",
        "")

    # Get the function call itself with all parameters and convert to character
    # so that it can be inserted in the file as rebuilt option.
    call_expr <- match.call()
    call_text <- paste(deparse(call_expr, width.cutoff = 500), collapse = "\n")

    # Rebuilt master section
    lines <- c(lines,
        "################################################################################",
        "# Rebuilt Master",
        "################################################################################",
        "",
        "```{r rebuilt_master, echo = TRUE}",
        paste0('master_file <- "', path, master_name, '.Rmd"'),
        "",
        "if (file.exists(master_file)){",
        "    file.remove(master_file)",
        "}",
        "",
        call_text,
        "```",
        "")

    # Generate tree view folder structure
    if (with_structure){
        message(" > Write folder structure")

        lines <- c(lines, print_folder_structure(scripts), "")
    }

    # Run all scripts in all folders
    if (with_run_all){
        message(" > Write all scripts execution")

        run_all_folders <- c(
            "",
            "################################################################################",
            "# Run All Scripts in All Folders",
            "################################################################################",
            "```{r run_all_scripts, echo = TRUE}",
            paste0('scripts <- c(', paste(paste0('libname("', names(scripts), '", get_files = TRUE)'), collapse = ',\n             '), ')'),
            "",
            "for (file in scripts){",
            "    source(file, local = FALSE)",
            "}",
            "",
            "rm(scripts)",
            "```")

        lines <- c(lines, run_all_folders)
    }

    message(" > Write script execution")

    # Run folders and files separate
    for (folder in names(scripts)){
        # Run all scripts in current folder
        folder_name <- gsub("[^a-zA-Z0-9_]", "_", paste0("run_", basename(folder)))
        all_scripts_in_folder <- unlist(scripts[[folder]])

        if (with_run_folder){
            message("   + folder: ", folder)

            lines <- c(lines, c("\n#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++",
                                paste0("#     Run All Scripts in Folder: ", basename(folder)),
                                "#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"),
                              paste0("```{r ", folder_name, ", echo = TRUE}"),
                              paste0('      scripts <- libname("', folder, '", get_files = TRUE)'),
                                "",
                                "      for (file in scripts){",
                                "          source(file, local = FALSE)",
                                "      }",
                               "",
                               "      rm(scripts)",
                                "```")
        }

        # Add each script as its own chunk
        files <- scripts[[folder]]

        for (file in files){
            message("     + file: ", file)

            file_name <- gsub("[^a-zA-Z0-9_]", "_", paste0("run_", basename(file)))

            lines <- c(lines, c("#-------------------------------------------------------------------------------#",
                                paste0("#         Run Script: ", basename(file)),
                                "#-------------------------------------------------------------------------------#"),
                              paste0("```{r ", file_name, ", echo = TRUE}"),
                              paste0('          source("', file, '", local = FALSE)'),
                              "```")
        }
    }

    message(" > Putting together master file")

    # Write master file
    writeLines(lines, con = paste0(path, master_name, ".Rmd"))

    end_time <- round(difftime(Sys.time(), start_time, units = "secs"), 3)
    message("\n- - - 'build_master' execution time: ", end_time, " seconds\n")

    invisible(lines)
}


#' Convert List of Files Into a Hierarchical Text Representation
#'
#' @description
#' Writes a text based folder structure in a tree view.
#'
#' @param file_list A list, which contains the folder structure and filenames.
#'
#' @return
#' A formatted character vector.
#'
#' @noRd
print_folder_structure <- function(file_list) {
    # Extract root folder
    root <- paste0(dirname(names(file_list)[1]), "/")

    # Root is always on top
    output <- c("################################################################################",
                "# Folder structure",
                "################################################################################",
                "",
                "# Root",
                "```{r open_root, echo = TRUE}",
                paste0('utils::browseURL("', root, '")'),
                "```")

    # Loop trough all list entries to build folder tree
    for (folder in names(file_list)){
        # Get folder and corresponding files first
        folder_path <- normalizePath(folder, winslash = "/")
        subfolder   <- basename(folder)
        files       <- file_list[[folder]]

        # Open folder section
        output <- c(output,
                    paste0("##......", subfolder),
                    paste0('```{r open_', subfolder, ', echo = TRUE}'),
                    paste0('        utils::browseURL("', folder_path, '")'),
                    "```")

        # Open files block
        for (file in files){
            file_path <- normalizePath(file, winslash = "/")

            output <- c(output,
                        paste0("###.............", basename(file)),
                        paste0('```{r open_', basename(file), ', echo = TRUE}'),
                        paste0('                file.edit("', file_path,'")'),
                        "```")
        }
    }

    output
}
