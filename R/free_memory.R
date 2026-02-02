#' Remove Objects From Memory By Name And Type
#'
#' @description
#' Remove objects by name or type to free up memory. Uses the base [rm()] function
#' but provides more flexible ways to remove objects.
#'
#' @param names Object names to be removed.
#' @param types Object types to be removed.
#' @param envir The environment to remove the objects from.
#'
#' @details
#' [free_memory()] is based on the 'SAS' function Proc Datasets. Among other things
#' this procedure is able to remove datasets from memory. But not only by writing
#' out the full file name, it is capable of looking for datasets starting with, ending
#' with or containing a certain text. Additionally certain object types can be removed.
#'
#' @return Returns removed objects.
#'
#' @examples
#' # Example data frames
#' my_data1 <- dummy_data(10)
#' my_data2 <- dummy_data(10)
#' data     <- dummy_data(10)
#' file     <- dummy_data(10)
#'
#' # Free memory by name
#' free_memory("my_:")
#' free_memory(c("data", "file"))
#'
#' # Free by type
#' my_data1 <- dummy_data(10)
#' my_data2 <- dummy_data(10)
#' data     <- dummy_data(10)
#' file     <- dummy_data(10)
#'
#' free_memory(type = "data.frame")
#'
#' @export
free_memory <- function(names = NULL,
                        types = NULL,
                        envir = .GlobalEnv){

    # If neither names nor types are given, ALL objects will be removed
    objects <- ls(envir = envir)

    # Get objects based on provided names
    if (!is.null(names)){
        if (!is.character(names)){
            message(" X ERROR: <Names> have to be provided as character. Objects will remain untouched.")
            return(invisible(NULL))
        }

        # If more than one object name is provided, treat them as separate names
        object_vector <- character(0)

        for (name in names){
            # Count the colons, there may not be more than two, otherwise the function
            # should abort
            colon_count <- nchar(gsub("[^:]", "", name))

            # In case of a single name just get the object with the provided name
            if (colon_count == 0){
                object_vector <- c(object_vector, intersect(objects, name))
            }
            # In case of a range
            else if (colon_count == 1){
                # When at the end ("text:"), select all objects which start with the characters
                # coming before the colon. ":" acts as a placeholder for those who come after.
                if (endsWith(name, ":")){
                    search_term   <- substring(name, 1, nchar(name) - 1)
                    object_vector <- c(object_vector, objects[startsWith(objects, search_term)])
                }
                # When at the start (":name"), select all objects which end with the characters
                # following the colon. ":" acts as a placeholder for everything that comes before
                else if (startsWith(name, ":")){
                    search_term   <- substring(name, 2)
                    object_vector <- c(object_vector, objects[endsWith(objects, gsub(":", "", name))])
                }
            }
            # In case there are two colons, one at the start and one at the end (":name:"),
            # look for the text inbetween.
            else if (colon_count == 2 && startsWith(name, ":") && endsWith(name, ":")){
                search_term   <- gsub(":", "", name)
                object_vector <- c(object_vector, objects[grepl(search_term, objects, fixed = TRUE)])

            }
            # If there are more than two colons abort
            else{
                message(" X ERROR: <Name> has more than two colons which is not allowed. Objects will remain untouched.")
                return(invisible(NULL))
            }
        }

        objects <- object_vector |> collapse::funique()
    }

    # Get objects based on provided types
    if (!is.null(types)) {
        if (!is.character(types)) {
            message(" X ERROR: <Types> have to be provided as character. Objects will remain untouched.")
            return(invisible(NULL))
        }

        # Get a logical vector with the length of the number of objects
        objects_to_keep <- logical(length(objects))

        # Look up original symbols and get their type. If the object type is within the
        # provided types it will be kept otherwise dropped.
        for (i in seq_along(objects_to_keep)){
            object <- get(objects[i], envir = envir)
            objects_to_keep[i] <- inherits(object, types)
        }

        objects <- objects[objects_to_keep]
    }

    # Abort if no object found
    if (length(objects) == 0) {
        message(" X ERROR: No object found. Objects will remain untouched.")
        return(invisible(NULL))
    }

    # Compute memory usage of the objects to be removed
    object_sizes <- vapply(objects,
                           function(object){
                               utils::object.size(get(object, envir = envir))
                           },
                           numeric(1))

    total_bytes <- collapse::fsum(object_sizes)

    message("\n- - - 'free_memory' removed ", length(objects), " object(s) and freed approximately ",
        format(structure(total_bytes, class = "object_size"), units = "auto"), ".")

    # Remove objects from memory
    rm(list = objects, envir = envir)
    gc(verbose = FALSE)

    invisible(objects)
}
