#' Build a Master Script From Folder
#'
#' @description
#' [build_master()] reads a given folder structure, which contains scripts, and builds
#' a master file. The file which gets written depends on the selected layout.
#'
#' @param dir The folder structure which contains the scripts to build upon.
#' @param master_name The file name which should be written.
#' @param author Authors name to be put in the header.
#' @param layout The layout which should be written to the master file. Available
#' are:
#'
#' * "compact": The header and rebuilt master section followed by one script block
#'              which runs all scripts. (default)
#' * "full":    The whole folder structure with an overview, a section which runs
#'              all scripts and single blocks for every folder and script.
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
#' Returns the script as character vector and saves it as master file.
#'
#' @seealso
#' RStudio helper functions: [run_script()], [run_folder()], [run_project()],
#' [run_project_parallel()]
#'
#' @examples
#' # Example export file paths
#' # NOTE: These tempfiles are only for the examples. In reality you just call the
#' # main function and put in your desired path and name directly.
#' temp_file <- tempfile(fileext = ".Rmd")
#'
#' # Example master
#' build_master(dir         = dirname(temp_file),
#'              master_name = basename(temp_file))
#'
#' # Example compact master
#' build_master(dir         = dirname(temp_file),
#'              master_name = basename(temp_file),
#'              layout      = "full")
#'
#' # Manual cleanup for example
#' unlink(temp_file)
#'
#' @export
build_master <- function(dir,
                         master_name = "Master",
                         author      = "",
                         layout      = "compact"){
    # Measure the time
    print_start_message()

    # Apply macros to dir
    dir <- macro(dir)

    # Check if folder exists
    if (!dir.exists(dir) || dirname(dir) == "."){
        print_message("ERROR", "Directory '[dir]' does not exist.", dir = dir)
        return(invisible(NULL))
    }

    # Determine which layout to build
    layout <- tolower(layout)

    if (!layout %in% c("full", "compact")){
        print_message("WARNING", c("Layout '[layout]' is not a valid option. Available are: 'full' and 'compact'",
                                   "'compact' will be used."), layout = layout)
        layout <- "compact"
    }

    # Get the function call itself with all parameters and convert to character
    # so that it can be inserted in the file as rebuilt option.
    call_text <- paste(deparse(match.call(), width.cutoff = 500), collapse = "\n")

    # Build the layout which writes the master file
    if (layout == "compact"){
        lines <- build_compact_layout(dir         = dir,
                                      master_name = master_name,
                                      author      = author,
                                      call_text   = call_text)
    }
    else{
        lines <- build_full_layout(dir         = dir,
                                   master_name = master_name,
                                   author      = author,
                                   call_text   = call_text)
    }

    print_closing(5)

    invisible(lines)
}


#' Build a Full Master Layout From Folder
#'
#' @description
#' Is the former layout used by [build_master()]. It reads a given folder structure,
#' which contains scripts, and builds a master script as a markdown file.
#'
#' @param dir The folder structure which contains the scripts to build upon.
#' @param master_name The file name which should be written.
#' @param author Authors name to be put in the header.
#' @param call_text The function call which should be inserted in the rebuilt master
#' section.
#'
#' @return
#' Returns the script as character vector and saves it as markdown file.
#'
#' @noRd
build_full_layout <- function(dir,
                              master_name = "Master",
                              author      = "",
                              call_text   = NULL){
    # Measure the time
    print_start_message(suppress = TRUE)

    print_step("MAJOR", "Scanning folder structure")

    # Get all .R scripts inside the folders
    scripts <- get_all_scripts(dir)

    path <- ifelse(grepl("/$", dir), dir, paste0(dir, "/"))

    # Setup header
    lines <- c("################################################################################",
               paste0("# ", master_name),
               "#",
               paste0("# Author: ", author),
               "#",
               paste0("# Date: ", format(Sys.Date(), "%d.%m.%Y")),
               "################################################################################",
               "",
               "```{r load_package, echo = TRUE}",
               "library(qol)",
               "",
               "run_scripts <- function(scripts){",
               "    print_start_message()",
               '    monitor_df <- NULL |> monitor_start("Script start", "Script start")',
               "",
               "    for (file in scripts){",
               '        monitor_df <- monitor_df |> monitor_next(basename(file), basename(file))',
               '        print_step("MAJOR", file)',
               "",
               "        source(file, local = FALSE)",
               "",
               "        monitor_df <- monitor_df |> monitor_end()",
               "    }",
               "",
               "    print_closing()",
               "",
               paste0("    monitor_df |> monitor_plot(draw_plot = TRUE)"),
               "}",
               "```",
               "")

    # Rebuilt master section
    lines <- c(lines, build_rebuilt_section(path, master_name, call_text))

    # Generate tree view folder structure
    print_step("MAJOR", "Write folder structure")

    lines <- c(lines, print_folder_structure(scripts), "")

    # Run all scripts in all folders
    print_step("MAJOR", "Write all scripts execution")

    # Get blanks for an even padding of the code
    sub_dirs        <- collapse::funique(sub(dir, "", names(scripts)))
    max_path_length <- collapse::fmax(nchar(sub_dirs))
    padding         <- substr(paste0(sub_dirs, strrep(" ", max_path_length)), nchar(sub_dirs) + 1, max_path_length)

    # Put together script block
    run_all_folders <- c(
        "",
        "################################################################################",
        "# Run All Scripts in All Folders",
        "################################################################################",
        "```{r run_all_scripts, echo = TRUE}",
        paste0('base_folder <- "', dir, '"'),
        paste0('scripts     <- c(', paste(paste0('libname(paste0(base_folder,"', sub_dirs, '"), ', padding, 'get_files = TRUE, extensions = "R")'), collapse = ',\n                 '), ')'),
        "",
        "run_scripts(scripts)",
        "",
        "rm(base_folder, scripts)",
        "```")

    lines <- c(lines, run_all_folders)

    print_step("MAJOR", "Write script execution")

    # Run folders and files separate
    for (folder in names(scripts)){
        # Run all scripts in current folder
        folder_name <- gsub("[^a-zA-Z0-9_]", "_", paste0("run_", basename(folder)))
        all_scripts_in_folder <- unlist(scripts[[folder]])

        print_step("MINOR", "folder: [folder]", folder = folder)

        # Put together script block
        lines <- c(lines, c("\n#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++",
                            paste0("#     Run All Scripts in Folder: ", basename(folder)),
                            "#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"),
                          paste0("```{r ", folder_name, ", echo = TRUE}"),
                          paste0('      scripts <- libname("', folder, '", get_files = TRUE, extensions = "R")'),
                            "",
                            "      run_scripts(scripts)",
                            "",
                            "      rm(scripts)",
                            "```")

        # Add each script as its own chunk
        files <- scripts[[folder]]

        for (file in files){
            print_step("MINOR", "    file: [file]", file = file)

            file_name <- gsub("[^a-zA-Z0-9_]", "_", paste0("run_", basename(file)))

            # Put together script block
            lines <- c(lines, c("#-------------------------------------------------------------------------------#",
                                paste0("#         Run Script: ", basename(file)),
                                "#-------------------------------------------------------------------------------#"),
                                paste0("```{r ", file_name, ", echo = TRUE}"),
                                paste0('          run_scripts("', file, '")'),
                                "```")
        }
    }

    print_step("MAJOR", "Putting together master file")

    # Write master file
    writeLines(lines, con = paste0(path, master_name, ".Rmd"))

    print_closing(5, suppress = TRUE)

    invisible(lines)
}


#' Build a Compact Master Layout
#'
#' @description
#' The "compact" layout used by [build_master()]. It writes the header and rebuilt
#' master section and afterwards one script block which sources all the script files
#' as separate lines.
#'
#' @param dir The folder structure which contains the scripts to build upon.
#' @param master_name The file name which should be written.
#' @param author Authors name to be put in the header.
#' @param call_text The function call which should be inserted in the rebuilt master
#' section. Used internally by [build_master()] to keep the original call.
#'
#' @return
#' Returns the script as character vector and saves it as markdown file.
#'
#' @noRd
build_compact_layout <- function(dir,
                                 master_name = "Master",
                                 author      = "",
                                 call_text   = NULL){
    # Measure the time
    print_start_message(suppress = TRUE)

    print_step("MAJOR", "Scanning folder structure")

    # Get all .R scripts inside the folders
    scripts <- get_all_scripts(dir)

    path <- ifelse(grepl("/$", dir), dir, paste0(dir, "/"))

    # Setup header
    lines <- c("################################################################################",
               paste0("# ", master_name),
               "#",
               paste0("# Author: ", author),
               "#",
               paste0("# Date: ", format(Sys.Date(), "%d.%m.%Y")),
               "################################################################################",
               "")

    # Rebuilt master section
    lines <- c(lines, build_rebuilt_section(path, master_name, call_text))

    print_step("MAJOR", "Write all scripts execution")

    # Put together script block
    rstudio_helpers <- c(
        "################################################################################",
        "# RStudio helpers",
        "################################################################################",
        "",
        "```{r rstudio_helpers, echo = TRUE}",
        "qol::run_script()",
        "qol::run_folder()",
        "qol::run_project()",
        "qol::run_project_parallel()",
        "```",
        "")

    # The base folder which all scripts have in common
    base_folder <- gsub("\\\\", "/", sub("/$", "", dir))

    # Put together script block
    run_all_scripts <- c(rstudio_helpers,
        "################################################################################",
        "# Run project",
        "################################################################################",
        "",
        "```{r run_all_scripts, echo = TRUE}",
        paste0('base_folder <- "', base_folder, '"'),
        "")

    # Loop through folders and set folder names as comment for visual separation
    for (folder in names(scripts)){
        print_step("MINOR", "folder: [folder]", folder = folder)

        run_all_scripts <- c(run_all_scripts, paste0("# Scripts from folder: ", basename(folder)))

        # Loop through files and add source lines
        for (file in scripts[[folder]]){
            print_step("MINOR", "    file: [file]", file = file)

            # Build the script path relative to the common base folder
            file_normalized <- gsub("\\\\", "/", file)
            relative_path   <- sub("^/+", "", sub(base_folder, "", file_normalized, fixed = TRUE))

            run_all_scripts <- c(run_all_scripts, paste0('source(paste0(base_folder, "/', relative_path, '"))'))
        }

        run_all_scripts <- c(run_all_scripts, "")
    }

    run_all_scripts <- c(run_all_scripts,
        "rm(base_folder)",
        "```")

    lines <- c(lines, run_all_scripts)

    print_step("MAJOR", "Putting together master file")

    # Write master file
    writeLines(lines, con = paste0(path, master_name, ".Rmd"))

    print_closing(5, suppress = TRUE)

    invisible(lines)
}


#' Get All R Scripts From A Folder Structure
#'
#' @description
#' Scans a folder structure for .R scripts and returns them as a named list,
#' where the names are the folders and the entries the contained scripts.
#'
#' @param dir The folder structure which contains the scripts.
#'
#' @return
#' Returns a named list of scripts.
#'
#' @noRd
get_all_scripts <- function(dir){
    # Get folders in provided directory
    folders <- list.dirs(dir, recursive = TRUE, full.names = TRUE)

    # Remove root folder. Otherwise when rebuilding the master script, the root
    # folder would be captured as folder with files in it.
    if (length(folders) > 1){
        folders <- folders[-1]
    }

    # Get all .R scripts inside the folders
    scripts <- lapply(folders, function(folder){
        suppressMessages(libname(folder, get_files = TRUE, extensions = "R"))
    })
    names(scripts) <- folders
    scripts        <- Filter(Negate(is.null), scripts)

    scripts
}


#' Build The Rebuilt Master Section
#'
#' @description
#' Builds the rebuilt master section of the master file, which lets the user
#' rebuild the master file with the same function call which created it.
#'
#' @param path The folder path to which the master file is written.
#' @param master_name The file name which should be written.
#' @param call_text The function call which created the master file.
#'
#' @return
#' Returns the rebuilt master section as character vector.
#'
#' @noRd
build_rebuilt_section <- function(path,
                                  master_name,
                                  call_text){
    c("################################################################################",
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
      paste0("qol::", call_text),
      "",
      "rm(master_file)",
      "```",
      "")
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
print_folder_structure <- function(file_list){
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


###############################################################################
# RSudio helpers
###############################################################################
#' Run 'R' Scripts Via File Dialog
#'
#' @name run_scripts
#'
#' @description
#' Opens an RStudio file selection dialog to pick a single script/folder or a
#' whole folder structure and runs it after confirmation. The dialog starts in the
#' folder of the currently opened script in RStudio.
#'
#' [run_script()]: Runs the selected script.
#'
#' @param path The folder in which the file selection dialog should start. If
#' left out, the folder of the currently opened script in RStudio is used.
#'
#' @details
#' Example structure for a larger project
#'
#' Project folder
#'   - Subfolder1
#'     + Script1.R
#'     + Script2.R
#'     + ...
#'  - Subfolder2
#'    + ...
#'  - ...
#'  - Master.Rmd
#'
#' If you open "Master.Rmd" in such a structure (the file can be completely empty)
#' and simply run one of the commands in the console, a selection window opens
#' which directly shows the path where the "Master.Rmd" file is located.
#' Depending on the command, you can then select and run either a single script,
#' all scripts in a folder, or all scripts in all folders of the project at once.
#'
#' Note: If the master file has the extension "*.R", it is also picked up when
#'       running the project folder with [run_project()]. If it also contains
#'       code which runs files in the subfolders, things will be executed twice.
#'       Therefore it is better to use a different file type or to not add any
#'       code in it.
#'
#' @return
#' Returns TRUE or FALSE.
#'
#' @seealso
#' Build master file: [build_master()]
#'
#' @rdname run_scripts
#'
#' @export
run_script <- function(path = NULL){
    if (!check_required_package("rstudioapi")){
        return(invisible(FALSE))
    }

    if (is.null(path)){
        path <- dirname(rstudioapi::getSourceEditorContext()[["path"]])
    }

    # Call the RStudio dialog to open a script. When the command is executed, the
    # path of the currently opened script is detected directly and used as the
    # starting point.
    selected_script <- rstudioapi::selectFile(
        caption  = "Run Script",
        label    = "Run Script",
        filter   = "R Files (*.R)",
        path     = path,
        existing = TRUE)

    if (is.null(selected_script)){
        print_message("ERROR", "No script selected.")
        return(invisible(FALSE))
    }

    # Open dialog to confirm the execution
    confirm <- rstudioapi::showQuestion(
        title   = "Confirm Running Script",
        message = paste0("Are you sure that you want to run the following script:\n\n",
                         "Folder: ", dirname(selected_script), "\n",
                         "Script: ", basename(selected_script), "?"),
        ok      = "Yes, run",
        cancel  = "Cancel")

    if (!confirm){
        print_message("NOTE", "Action cancelled.")
        return(invisible(FALSE))
    }

    source(selected_script, local = FALSE)

    invisible(TRUE)
}


#' @description
#' [run_folder()]: Runs all scripts in the selected folder.
#'
#' @rdname run_scripts
#'
#' @export
run_folder <- function(path = NULL){
    if (!check_required_package("rstudioapi")){
        return(invisible(FALSE))
    }

    if (is.null(path)){
        path <- dirname(rstudioapi::getSourceEditorContext()[["path"]])
    }

    # Call the RStudio dialog to open a folder. When the command is executed, the
    # path of the currently opened script is detected directly and used as the
    # starting point.
    selected_folder <- rstudioapi::selectDirectory(
        caption  = "Run Scripts in Folder",
        label    = "Run Folder",
        path     = path)

    if (is.null(selected_folder)){
        print_message("ERROR", "No folder selected.")
        return(invisible(FALSE))
    }

    # Get all scripts in the selected folder
    scripts <- suppressMessages(libname(selected_folder, get_files = TRUE, extensions = "R"))

    if (length(scripts) == 0){
        print_message("ERROR", "No scripts found in: [folder]", folder = selected_folder)
        return(invisible(FALSE))
    }

    # Show confirmation dialog
    confirm <- rstudioapi::showQuestion(
        title   = "Confirm Running Scripts In Folder",
        message = paste0("Are you sure that you want to run all ", length(scripts), " scripts in the following folder:\n\n",
                         "Folder: ", selected_folder, "?"),
        ok      = "Yes, run",
        cancel  = "Cancel")

    if (!confirm){
        print_message("NOTE", "Action cancelled.")
        return(invisible(FALSE))
    }

    # Loop through all scripts and run them
    print_start_message()

    for (script in scripts){
        print_headline("[b]Executing file: [script][/b]", script = basename(script), line_char = ".")

        source(script, local = FALSE)
    }

    print_message("NOTE", "Running project was successful.")
    print_closing(60)

    invisible(TRUE)
}


#' @description
#' [run_project()]: Runs all scripts found in a folder structure, including all
#' scripts within subfolders, in sequential order.
#'
#' @rdname run_scripts
#'
#' @export
run_project <- function(path = NULL){
    if (!check_required_package("rstudioapi")){
        return(invisible(FALSE))
    }

    if (is.null(path)){
        path <- dirname(dirname(rstudioapi::getSourceEditorContext()[["path"]]))
    }

    # Call the RStudio dialog to open a folder. When the command is executed, the
    # path of the currently opened script is detected directly and used as the
    # starting point.
    selected_project <- rstudioapi::selectDirectory(
        caption  = "Run All Scripts In Project Folder",
        label    = "Run Project",
        path     = path)

    if (is.null(selected_project)){
        print_message("ERROR", "No project selected.")
        return(invisible(FALSE))
    }

    # Get all scripts in the selected project folder, including subfolders
    scripts <- suppressMessages(libname(selected_project,
                                             get_files  = TRUE,
                                             extensions = "R",
                                             recursive  = TRUE))

    if (length(scripts) == 0){
        print_message("ERROR", "No scripts found in: [folder]", folder = selected_project)
        return(invisible(FALSE))
    }

    # Show confirmation dialog
    confirm <- rstudioapi::showQuestion(
        title   = "Confirm Running Scripts In Project",
        message = paste0("Are you sure that you want to run all ", length(scripts), " scripts in the following project folder (including subfolders):\n\n",
                         "Project: ", selected_project, "\n\n",
                         paste(basename(unique(dirname(scripts))), collapse = "\n"), "?"),
        ok      = "Yes, run",
        cancel  = "Cancel")

    if (!confirm){
        print_message("NOTE", "Action cancelled.")
        return(invisible(FALSE))
    }

    print_start_message()

    # Loop through all scripts and run them
    for (script in scripts){
        print_headline("[b]Executing file: [script][/b]", script = basename(script), line_char = ".")

        source(script, local = FALSE)
    }

    print_message("NOTE", "Running project was successful.")
    print_closing(60)

    invisible(TRUE)
}


#' @description
#' [run_project()]: Runs all scripts found in a folder structure, including all
#' scripts within subfolders. Folders, which are marked with an "!" at the end
#' of their folder name, are run as background jobs in parallel, all other folders
#' are run sequentially.
#'
#' @rdname run_scripts
#'
#' @export
run_project_parallel <- function(path = NULL){
    if (!check_required_package("rstudioapi")){
        return(invisible(FALSE))
    }

    if (is.null(path)){
        path <- dirname(dirname(rstudioapi::getSourceEditorContext()[["path"]]))
    }

    # Call the RStudio dialog to open a folder. When the command is executed, the
    # path of the currently opened script is detected directly and used as the
    # starting point.
    selected_project <- rstudioapi::selectDirectory(
        caption  = "Run All Scripts In Project Folder",
        label    = "Run Project",
        path     = path)

    if (is.null(selected_project)){
        print_message("ERROR", "No project selected.")
        return(invisible(FALSE))
    }

    # Get all scripts in the selected project folder, including subfolders
    scripts <- suppressMessages(libname(selected_project,
                                        get_files  = TRUE,
                                        extensions = "R",
                                        recursive  = TRUE))

    if (length(scripts) == 0){
        print_message("ERROR", "No scripts found in: [folder]", folder = selected_project)
        return(invisible(FALSE))
    }

    # Show confirmation dialog
    confirm <- rstudioapi::showQuestion(
        title   = "Confirm Running Scripts In Project",
        message = paste0("Are you sure that you want to run all ", length(scripts), " scripts in the following project folder (including subfolders):\n\n",
                         "Project: ", selected_project, "\n\n",
                         paste(basename(unique(dirname(scripts))), collapse = "\n"), "?"),
        ok      = "Yes, run",
        cancel  = "Cancel")

    if (!confirm){
        print_message("NOTE", "Action cancelled.")
        return(invisible(FALSE))
    }

    sub_folders <- list.dirs(selected_project, recursive = FALSE, full.names = TRUE)

    print_start_message()

    # Loop through all subdirectories
    status      <- "success"
    active_jobs <- c()
    job_files   <- list()

    for (folder in sub_folders){
        # Identify if the currently processed folder is a job folder. Job folders
        # are marked by an "!" at the end of the folders name. These folders are
        # run in parallel, all other folders are run sequentially.
        folder_name   <- basename(folder)
        is_job_folder <- grepl("!$", folder_name)

        # If the current folder needs to run sequentially and there are still
        # background jobs running, the main process has to wait for all jobs to
        # finish. This is to make sure that the background jobs are guaranteed
        # to have finished their execution.
        if (!is_job_folder && length(active_jobs) > 0){
            status <- wait_for_jobs(active_jobs, job_files)

            # If any background job fails, stop execution
            if (status != "success"){
                print_message("ERROR", "Background job '[job]' failed. Project execution will be aborted.", job = status)
                return(invisible(NULL))
            }

            # Reset active job pool after waiting
            active_jobs <- c()
        }

        # Job folders can run in parallel in the background
        if (is_job_folder){
            print_message("NOTE", "Running background job: [folder]", folder = folder_name)

            # Dynamically write temporary job launcher script
            temp_job_script <- tempfile(fileext = ".R")

            writeLines(c(sprintf('folder_path  <- "%s"', gsub("\\\\", "/", folder)),
                         'scripts    <- list.files(folder_path, pattern = "\\\\.R$", full.names = TRUE)',
                         'folder_env <- new.env(parent = globalenv())',
                         'for (script in scripts){',
                         '    source(script, local = folder_env)',
                         '}'), temp_job_script)

            # Launch Job
            job_id <- rstudioapi::jobRunScript(path = temp_job_script,
                                               name = paste("Job:", folder_name),
                                               importEnv = FALSE)

            # Add job id to list of active jobs, which is used to check whether
            # jobs are still running.
            active_jobs         <- c(active_jobs, job_id)
            job_files[[job_id]] <- temp_job_script
        }
        # Run sequential jobs
        else{
            print_headline("[b]Running folder: [folder][/b]", folder = folder_name, line_char = "_")

            script_files <- list.files(folder, pattern = "\\.R$", full.names = TRUE)

            for (script in script_files){
                print_headline("[b]Executing file: [script][/b]", script = basename(script), line_char = ".")

                source(script, local = FALSE)
            }
        }
    }

    # If the last folder is a job folder, wait till it is finished
    if (length(active_jobs) > 0){
        status <- wait_for_jobs(active_jobs, job_files)

        # If any background job fails, stop execution
        if (status != "success"){
            print_message("ERROR", "Background job '[job]' failed. Project execution will be aborted.", job = status)
            return(invisible(NULL))
        }
    }

    print_message("NOTE", "Running project was successful.")
    print_closing(60)

    invisible(TRUE)
}


#' @description
#' Blocks the main process until all submitted RStudio background jobs have
#' finished. Temporary job scripts are removed and finished jobs are removed
#' from the job list once they have completed.
#'
#' @param job_ids The ids of the submitted background jobs.
#' @param job_files A list which maps the job ids to the temporary job scripts.
#'
#' @return
#' Returns "success" if all jobs finished successfully, otherwise the name of
#' the failed job.
#'
#' @rdname run_scripts
#'
#' @noRd
wait_for_jobs <- function(job_ids, job_files){
    job_status <- "success"

    # No jobs, no blocking
    if (length(job_ids) == 0){
        return(invisible(job_status))
    }

    print_message("NOTE", "Pausing main process until background jobs finish.")

    # Loops until jobs finish
    while (TRUE){
        # Fetch all current jobs from RStudio
        all_jobs <- rstudioapi::jobList()

        # Check which jobs are completed and which are not. Mark the jobs which
        # are still active.
        current_job_ids <- sapply(all_jobs, function(job){
            if (job[["completed"]] > 0){
                # Remove temporary script files if job is finished
                unlink(job_files[[job[["id"]]]])

                # If a job fails, then set the flag to failed and return the failed
                # job.
                if (job[["state_description"]] == "failed"){
                    job_status <- job[["name"]]
                }

                NULL
            }
            else{
                job[["id"]]
            }
        })

        # Get the jobs which are still active
        still_running <- intersect(job_ids, current_job_ids)

        # If all jobs are finished, the sequential loop can go on
        if (length(still_running) == 0){
            # Remove all the jobs from the job list, otherwise they would still
            # be there on other runs until session ends.
            for (job in job_ids){
                rstudioapi::jobRemove(job)
            }

            print_message("NOTE", "All background jobs have finished. Resuming sequential execution.")
            break
        }

        # Don't check the state too often, therefor wait a second
        Sys.sleep(1)
    }

    invisible(job_status)
}
