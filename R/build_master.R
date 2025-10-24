#' Build a Master Script From Folder
#'
#' @description
#' [build_master()] reads a given folder structure, which contains scripts, and builds
#' a master script as a markdown file.
#'
#' @param dir The folder structure which contains the scripts to build upon.
#' @param master_name The file name which should be written.
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
#' build_master(dir = "C:/My Projects/Code", master_name = "Master Script")
#'
#' @export
build_master <- function(dir,
                         master_name     = "Master",
                         with_structure  = TRUE,
                         with_run_all    = TRUE,
                         with_run_folder = TRUE){
    # Measure the time
    start_time <- Sys.time()

    # Check if folder exists
    if (dir != "..."){
        if (!dir.exists(dir)){
            message(" X ERROR: Directory '", dir, "' does not exist.")
            return(invisible(NULL))
        }

        # Get folders in provided directory
        folders <- list.dirs(dir, recursive = TRUE, full.names = TRUE)

        # Get all .R scripts inside the folders
        scripts <- lapply(folders, function(folder){
            files <- list.files(folder, pattern = "\\.R$", full.names = TRUE)

            if (length(files) == 0){
                return(invisible(NULL))
            }
            else{
                files
            }
        })
        names(scripts) <- folders
        scripts        <- Filter(Negate(is.null), scripts)
    }
    # ... is for testing
    else{
        scripts <- list("root:/folder/" = "root:/folder/script.R")
    }

    all_scripts <- unlist(scripts)

    # Setup header
    lines <- c(
        "################################################################################",
        paste0("# ", master_name),
        "#",
        "# Author: ",
        "#",
        paste0("# Date: ", format(Sys.Date(), "%d.%m.%Y")),
        "################################################################################",
        "")

    # Generate tree view folder structure
    if (with_structure){
        lines <- c(lines, print_folder_structure(scripts), "")
    }

    # Run all scripts in all folders
    if (with_run_all){
        run_all_folders <- c(
            "################################################################################",
            "# Run All Scripts in All Folders",
            "################################################################################",
            "",
            "```{r run_all_scripts, echo = TRUE}",
            paste0('all_scripts <- c("', paste(all_scripts, collapse = '",\n                 "'), '")'),
            "",
            "for (file in all_scripts){",
            "    source(file, local = FALSE)",
            "}",
            "```",
            "")

        lines <- c(lines, run_all_folders)
    }

    # Run folders and files separate
    for (folder in names(scripts)){
        # Run all scripts in current folder
        folder_name <- gsub("[^a-zA-Z0-9_]", "_", paste0("run_", basename(folder)))
        all_scripts_in_folder <- unlist(scripts[[folder]])

        if (with_run_folder){
            lines <- c(lines, c("#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++",
                                paste0("# Run All Scripts in Folder: ", basename(folder)),
                                "#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++",
                                ""),
                              paste0("```{r ", folder_name, ", echo = TRUE}\n"),
                              c(paste0('all_scripts <- c("', paste(all_scripts_in_folder, collapse = '",\n                 "'), '")'),
                                "",
                                "for (file in all_scripts){",
                                "    source(file, local = FALSE)",
                                "}",
                                "```",
                                "\n"))
        }

        # Add each script as its own chunk
        files <- scripts[[folder]]

        for (file in files){
            file_name <- gsub("[^a-zA-Z0-9_]", "_", paste0("run_", basename(file)))

            lines <- c(lines, c("#--------------------------------------------------------------------------------",
                                paste0("# Run Script: ", basename(file)),
                                "#--------------------------------------------------------------------------------"),
                              paste0("```{r ", file_name, ", echo = TRUE}"),
                              paste0('source("', file, '", local = FALSE)'),
                              "```",
                              "\n")
        }
    }

    # Write master file
    path <- ifelse(grepl("/$", dir), dir, paste0(dir, "/"))

    if (dir != "..."){
        writeLines(lines, con = paste0(path, master_name, ".Rmd"))
    }

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

    # Loop trough all list entries to build folder tree
    output <- c("################################################################################",
                "# Folder structure",
                "################################################################################",
                "",
                root)

    for (folder in names(file_list)) {
        subfolder <- basename(folder)
        files     <- basename(file_list[[folder]])

        # Write folder structure into character vector
        output <- c(output,
                    paste0("  ", subfolder, "/"),
                    paste0("    |_ ", files))
    }

    output
}
