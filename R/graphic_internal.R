###############################################################################
# Get dimensions
###############################################################################
#' Get The Diagram Start In Cm
#'
#' @description
#' Get the actual diagram start in cm.
#'
#' @param dimensions The list of dimensions parameters.
#' @param fine_tuning The list of fine tuning parameters.
#' @param title_height Heights of the graphic titles.
#'
#' @return
#' Returns a numeric width in cm.
#'
#' @noRd
get_diagram_start_cm <- function(dimensions   = .qol_options[["graphic_dimensions"]],
                                 fine_tuning  = .qol_options[["graphic_fine_tuning"]],
                                 title_height = 0){
    start        <- dimensions[["diagram_start"]]
    start_adjust <- fine_tuning[["diagram_start_adjust"]]

    # Measure diagram start automatically and set it right under the title or
    # take the manually set start.
    if (start == "auto"){
        # Calculate new diagram viewport y starting position
        # HACK: Using one additional margin here because somehow the height measuring
        #       of multiline text doesn't output the correct values.
        valid_heights <- sum(title_height > 0)
        start         <- (dimensions[["graphic_height"]]
                          - (title_height
                          + (dimensions[["margins"]] * (start_adjust + valid_heights))))
    }
    else{
        start <- dimensions[["graphic_height"]] - start
    }

    invisible(start)
}


#' Get The Diagram Width In Cm
#'
#' @description
#' Get the actual diagram width in cm.
#'
#' @param dimensions The list of dimensions parameters.
#'
#' @return
#' Returns a numeric width in cm.
#'
#' @noRd
get_diagram_width_cm <- function(dimensions = .qol_options[["graphic_dimensions"]]){
    width <- dimensions[["diagram_width"]]

    # Measure diagram width automatically and set it to span from side to side or
    # take the manually set height.
    if (width == "auto"){
        width <- (dimensions[["graphic_width"]] - (dimensions[["margins"]] * 2))
    }

    invisible(width)
}


#' Get The Diagram Height In Cm
#'
#' @description
#' Get the actual diagram height in cm.
#'
#' @param dimensions The list of dimensions parameters.
#'
#' @return
#' Returns a numeric width in cm.
#'
#' @noRd
get_diagram_height_cm <- function(dimensions      = .qol_options[["graphic_dimensions"]],
                                  fine_tuning     = .qol_options[["graphic_fine_tuning"]],
                                  title_height    = 0,
                                  footnote_height = 0){
    height        <- dimensions[["diagram_height"]]
    height_adjust <- fine_tuning[["diagram_height_adjust"]]

    # Measure diagram height automatically and set it to span between title and
    # footnotes or take the manually set height.
    if (height == "auto"){
        # Calculate new diagram viewport height
        valid_heights <- sum(title_height    > 0,
                             footnote_height > 0)

        # HACK: Using two additional margins here because somehow the height measuring
        #       of multiline text doesn't output the correct values.
        height <- (dimensions[["graphic_height"]]
                   - title_height
                   - footnote_height
                   - (dimensions[["margins"]] * (height_adjust + valid_heights)))
    }

    invisible(height)
}


###############################################################################
# Adjust labels
###############################################################################
#' Decollision Segment labels
#'
#' @description
#' Tries to decollide segment labels, which are drawn with segment lines on the
#' same height.
#'
#' @param dimensions The list of dimensions parameters.
#'
#' @return
#' Returns a numeric width in cm.
#'
#' @noRd
decollide_labels <- function(segment_labels,
                             x_label_positions,
                             dimensions,
                             fine_tuning){
    # Maximum shift is half of the maximum textbox width. Meaning label can only
    # be shifted of half its size from a centered point. So the textbox edge ends
    # up at the segment line at max.
    max_shift <- (dimensions[["textbox_width"]] / dimensions[["diagram_width"]]) * fine_tuning[["max_segment_label_shift"]]

    # If there are no labels or just one, return original positions
    number_of_labels <- length(segment_labels)
    if (number_of_labels <= 1){
        return(x_label_positions)
    }

    # Get the actual text widths of the segment labels
    widths <- grid::convertWidth(grid::stringWidth(segment_labels), "native", valueOnly = TRUE)

    half_widths <- widths / 2

    # Labels are separated in left and right side. The labels on the left side
    # get shifted to the left, beginning from the middle to reduce needed decollision
    # iterations.
    shifted_x_positions <- x_label_positions
    middle_id           <- (number_of_labels + 1) / 2

    start_segment <- floor(middle_id) - (middle_id %% 1 == 0)

    # Calculate overlap with right neighbors
    for (label in start_segment:1){
        # Far right point of the current label is checked against far left
        # side of the previous label.
        overlap <- ((shifted_x_positions[label] + half_widths[label])
                  - (shifted_x_positions[label + 1] - half_widths[label + 1]))

        # If there is an overlap, decollide
        if (overlap > 0){
            # The label shifting is minimized, depending on what is smaller:
            # either by the actual overlap or the maximum shift. Maximum
            # shift would most of the time mean that there is still an overlap,
            # but shifting further would make no sense, because then the label
            # and segment would be decoupled.
            shifted_x_positions[label] <- max(shifted_x_positions[label] - overlap,
                                              shifted_x_positions[label] - max_shift)
        }
    }

    # The labels on the right side get shifted to the right
    start_segment <- ceiling(middle_id) + (middle_id %% 1 == 0)

    # Calculate overlap with left neighbors
    for (label in start_segment:number_of_labels){
        # Far right point of the previous label is checked against far left
        # side of the current label.
        overlap <- ((shifted_x_positions[label - 1] + half_widths[label - 1])
                  - (shifted_x_positions[label] - half_widths[label]))

        # If there is an overlap, decollide
        if (overlap > 0){
            # The label shifting is minimized, depending on what is smaller:
            # either by the actual overlap or the maximum shift. Maximum
            # shift would most of the time mean that there is still an overlap,
            # but shifting further would make no sense, because then the label
            # and segment would be decoupled.
            shifted_x_positions[label] <- min(shifted_x_positions[label] + overlap,
                                              shifted_x_positions[label] + max_shift)
        }
    }

    # Return shifted positions
    shifted_x_positions
}


###############################################################################
# Graphic output
###############################################################################
#' Output Graphic To Device
#'
#' @description
#' Draw or output a graphic to a desired destination.
#'
#' @param graphic_object The complete graphic to be drawn or exported.
#' @param dimensions qol package dimensions options.
#' @param fine_tuning The list of fine tuning parameters.
#' @param output qol package output options.
#'
#' @return
#' Returns the input graphic object.
#'
#' @noRd
output_graphic <- function(graphic_object,
                           visuals,
                           dimensions,
                           fine_tuning,
                           output,
                           diagram_info,
                           by_info = NULL){
    # Early exit if all graphics should be exported as one file containing the
    # whole grid.
    if (output[["by_as_grid"]]){
        if (interactive()){
            grid::grid.draw(graphic_object)
        }

        return(invisible(graphic_object))
    }

    # Check if only save path or file name is specified. If only one is specified
    # print a note. Otherwise the file would not be saved and opened without a
    # hint to why the file wasn't saved.
    if (is.null(output[["save_path"]]) + is.null(output[["file"]]) == 1){
        if (is.null(output[["save_path"]])){
            print_message("NOTE", c("No save path specified. Both save path and file name with extension",
                                    "need to be specified in the global options or style parameter for",
                                    "the file to be saved. File won't be saved."))
        }
        else{
            print_message("NOTE", c("No file name specified. Both save path and file name with extension",
                                    "need to be specified in the global options or style parameter for",
                                    "the file to be saved. File won't be saved."))
        }
    }

    # First check if full file path is provided. If not only draw graphic in plot window.
    if (is.null(output[["save_path"]]) || is.null(output[["file"]])){
        if (interactive()){
            grid::grid.draw(graphic_object)
        }
    }
    else{
        # If save path doesn't exist, just draw graphic in plot window.
        if (!file.exists(output[["save_path"]])){
            print_message("WARNING", "Path does not exist: ", output[["save_path"]])

            if (interactive()){
                grid::grid.draw(graphic_object)
            }
        }
        # Save file
        else{
            # Get file extension to determine output format
            extension <- tolower(tools::file_ext(output[["file"]]))
            filename  <- tools::file_path_sans_ext(output[["file"]])

            if (!extension %in% c("png", "svg", "jpeg", "jpg", "bmp", "tiff")){
                print_message("WARNING", c("Filetype '[extension]' not supported, 'png' will be used. Valid filetypes are:",
                                           "png, svg, jpeg, jpg, bmp, tiff"), extension = extension)

                extension <- "png"
            }

            # If there is no by info, the file name is already provided through the option
            if (is.null(by_info)){
                filename <- paste0(filename, ".", extension)
            }
            # If this function is called via the by variable loop, then add the expression
            # information to the file name.
            else{
                filename <- paste0(filename, "_", by_info, ".", extension)
            }

            filename <- paste0(output[["save_path"]], "/", filename)

            # On interactive output call the respective function
            if (output[["interactive"]]){
                output_interactive_svg(graphic_object,
                                       visuals,
                                       dimensions,
                                       fine_tuning,
                                       output,
                                       diagram_info,
                                       filename,
                                       by_info)
            }
            # Static output
            else{
                # Put all output functions into a list to call them dynamically
                devices <- list(png  = grDevices::png,
                                svg  = grDevices::svg,
                                jpeg = grDevices::jpeg,
                                jpg  = grDevices::jpeg,
                                bmp  = grDevices::bmp,
                                tiff = grDevices::tiff)

                device_function <- devices[[extension]]

                # Store the base arguments for the output functions
                arguments <- list(filename = filename,
                                  width    = dimensions[["graphic_width"]]  / fine_tuning[["cm_to_inch_factor"]],
                                  height   = dimensions[["graphic_height"]] / fine_tuning[["cm_to_inch_factor"]])

                # Add format specific arguments based on file extension
                if (extension %in% c("png", "jpeg", "jpg", "bmp", "tiff")){
                    arguments[["units"]] <- "in"
                    arguments[["res"]]   <- output[["resolution"]]
                }

                # Export image and draw to plot view
                do.call(device_function, arguments)

                # Draw for physical output
                grid::grid.draw(graphic_object)

                grDevices::dev.off()
            }

            # Draw visible in plot view
            grid::grid.draw(graphic_object)
        }
    }

    invisible(graphic_object)
}

#' @noRd
output_grid <- function(dimensions,
                        fine_tuning,
                        output){
    # Early exit if all graphics should be exported as single files
    if (!output[["by_as_grid"]]){
        return(invisible(NULL))
    }

    # Check if only save path or file name is specified. If only one is specified
    # print a note. Otherwise the file would not be saved and opened without a
    # hint to why the file wasn't saved.
    if (is.null(output[["save_path"]]) + is.null(output[["file"]]) == 1){
        if (is.null(output[["save_path"]])){
            print_message("NOTE", c("No save path specified. Both save path and file name with extension",
                                    "need to be specified in the global options or style parameter for",
                                    "the file to be saved. File won't be saved."))
        }
        else{
            print_message("NOTE", c("No file name specified. Both save path and file name with extension",
                                    "need to be specified in the global options or style parameter for",
                                    "the file to be saved. File won't be saved."))
        }
    }

    # First check if full file path is provided. If not return.
    if (is.null(output[["save_path"]]) || is.null(output[["file"]])){
        return(invisible(NULL))
    }
    else{
        # If save path doesn't exist, abort.
        if (!file.exists(output[["save_path"]])){
            print_message("WARNING", "Path does not exist: ", output[["save_path"]])

            return(invisible(NULL))
        }
        # Save file
        else{
            print_step("MAJOR", "Saving to file")

            # Get file extension to determine output format
            extension <- tolower(tools::file_ext(output[["file"]]))
            filename  <- tools::file_path_sans_ext(output[["file"]])

            if (!extension %in% c("png", "svg", "jpeg", "jpg", "bmp", "tiff")){
                print_message("WARNING", c("Filetype '[extension]' not supported, 'png' will be used. Valid filetypes are:",
                                           "png, svg, jpeg, jpg, bmp, tiff"), extension = extension)

                extension <- "png"
            }

            # Reconstruct file name
            filename <- paste0(filename, ".", extension)
            filename <- paste0(output[["save_path"]], "/", filename)

            # Put all output functions into a list to call them dynamically
            devices <- list(png  = grDevices::png,
                            svg  = grDevices::svg,
                            jpeg = grDevices::jpeg,
                            jpg  = grDevices::jpeg,
                            bmp  = grDevices::bmp,
                            tiff = grDevices::tiff)

            device_function <- devices[[extension]]

            # Store the base arguments for the output functions
            arguments <- list(filename = filename,
                              width    = dimensions[["graphic_width"]]  / fine_tuning[["cm_to_inch_factor"]],
                              height   = dimensions[["graphic_height"]] / fine_tuning[["cm_to_inch_factor"]])

            # Add format specific arguments based on file extension
            if (extension %in% c("png", "jpeg", "jpg", "bmp", "tiff")){
                arguments[["units"]] <- "in"
                arguments[["res"]]   <- output[["resolution"]]
            }

            # Grab the entire grid which is drawn in the plot view as gTree
            grid_object <- grid::grid.grab(wrap.grobs = TRUE)

            # Export image and draw to plot view
            do.call(device_function, arguments)

            # Draw for physical output
            grid::grid.draw(grid_object)

            grDevices::dev.off()

            # Draw visible in plot view
            grid::grid.draw(grid_object)
        }
    }

    invisible(grid_object)
}


###############################################################################
# Interactive output
###############################################################################
#' Output Interactive SVG Graphic
#'
#' @description
#' Output an interactive SVG graphic to a desired destination. The graphic design
#' is like the original static image but carries java script based tooltips for
#' the segments and hover over effects.
#'
#' @param graphic_object The complete graphic to be drawn or exported.
#' @param dimensions Dimension parameters set with [graphic_dimensions()].
#' @param fine_tuning Fine tuning parameters set with [graphic_fine_tuning()].
#' @param output Output parameters set with [graphic_output()].
#' @param diagram_info The list of measurements generated by [get_diagram_dimensions()].
#' @param filename Complete file path with name and extension where the interactive
#' chart should be saved.
#' @param by_info Text which contains the information which variable with which
#' expression is computed at the moment. Used for computation with by variables.
#'
#' @return
#' Returns the input graphic object.
#'
#' @noRd
output_interactive_svg <- function(graphic_object,
                                   visuals,
                                   dimensions,
                                   fine_tuning,
                                   output,
                                   diagram_info,
                                   filename,
                                   by_info = NULL){
    # Execution of this code is only possible if gridSVG package is installed
    if (!check_required_package()){
        return(invisible(NULL))
    }

    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # Export temporary SVG file to grab it back with gridSVG to gain a structure
    # which can be manipulated to make it interactive.
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    if (is.null(by_info)){ print_step("MINOR", "Grabbing static SVG file") }

    # Convert cm to pixels
    dpi <- output[["resolution"]]

    graphic_width  <- (dimensions[["graphic_width"]]  / 2.54) * dpi
    graphic_height <- (dimensions[["graphic_height"]] / 2.54) * dpi

    # Generate a temporary image on device to capture the actual graphics dimensions
    temp_image <- tempfile(fileext = ".png")
    grDevices::png(filename = temp_image,
                   width    = graphic_width,
                   height   = graphic_height,
                   res      = dpi)

    # First draw the static gTree and then capture it with gridSVG to build the
    # base for the interactive structure.
    grid::grid.draw(graphic_object)
    gridSVG::grid.export(temp_image)

    # Read in the SVG file as a string so that it can be manipulated. Delete temporary
    # file afterwards since it is no longer needed and close the device
    svg_string <- paste(readLines(temp_image, warn = FALSE), collapse = "\n")

    grDevices::dev.off()
    file.remove(temp_image)

    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # Adjust line heights because they are to big
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    if (is.null(by_info)){ print_step("MINOR", "Adjust line heights") }

    # There is a problem with the line heights in the generated SVG file. Multi line
    # texts have a fixed but too large space. The definitions can be captured and
    # manipulated. For that all lines of the file are checked for this pattern.
    svg_lines <- unlist(strsplit(svg_string, "\n"))

    current_context     <- "none"
    is_first_line       <- TRUE

    is_group_label_on_top <- all(diagram_info[["values"]] <= 0)
    number_of_label_lines <- 1

    for (current_line in seq_along(svg_lines)){
        # Look for the opening of specific grob containers and detect the context
        if (grepl("<g id=", svg_lines[current_line])){
            # Titles and custom textboxes only need reduced line heights
            if (grepl("title", svg_lines[current_line]) ||grepl("custom_textbox", svg_lines[current_line])){
                current_context <- "title"
            }
            # Footnotes additionally need to be lowered down, because the upper line
            # is anchored too high because of the bigger line height.
            else if (grepl("footnote", svg_lines[current_line])){
                current_context <- "footnote"
            }
            # Group labels need to be treated a bit different as well, because they
            # may not be shifted up, or otherwise they end up inside the x axes.
            else if (grepl("group_labels", svg_lines[current_line])){
                current_context <- "group_labels"
                is_first_line   <- TRUE

                number_of_label_lines <- 1
            }
            # Segment labels need to be shifted down like group labels when they
            # are drawn on top of the diagram.
            else if (grepl("segment_labels", svg_lines[current_line])){
                current_context <- "segment_labels"
                is_first_line   <- TRUE

                number_of_label_lines <- 1
            }
            # Everything else is skipped
            # TODO: CHECK FOR CUSTOM TEXTS
            else{
                current_context <- "other"
            }
        }

        # Adjust title and footnote line heights first
        if (current_context %in% c("title", "footnote") && grepl('dy="-[0-9.]+"', svg_lines[current_line])){
            current_negative_dy <- as.numeric(sub('.*dy="(-[0-9.]+)".*', '\\1', svg_lines[current_line]))
            new_negative_dy     <- current_negative_dy * fine_tuning[["svg_anchor_adjust"]]

            # Manipulate entry
            svg_lines[current_line] <- sub(paste0('dy="', current_negative_dy, '"'),
                                paste0('dy="', new_negative_dy, '"'),
                                svg_lines[current_line])
        }

        # Adjust the line heights depending on the current context
        if (grepl('dy="[0-9.]+"', svg_lines[current_line]) && !grepl('dy="-[0-9.]+"', svg_lines[current_line])){
            if (current_context %in% c("title", "footnote", "group_labels", "segment_labels")){
                current_dy <- as.numeric(sub('.*dy="([0-9.]+)".*', '\\1', svg_lines[current_line]))

                # Group labels must keep their anchor point which needs to be skipped
                # and not adjusted.
                if (current_context == "group_labels"){
                    if (is_first_line && !is_group_label_on_top){
                        line_height_factor <- 1.0
                        is_first_line      <- FALSE
                    }
                    # Set the exact same compression ratio as for titles and footnotes
                    else{
                        line_height_factor <- fine_tuning[["svg_line_height_adjust"]]
                    }
                }
                # Set line height compression ratio
                else{
                    line_height_factor <- fine_tuning[["svg_line_height_adjust"]]
                }

                # Manipulate entry
                new_dy                  <- current_dy * line_height_factor
                svg_lines[current_line] <- sub(paste0('dy="', current_dy, '"'),
                                               paste0('dy="', new_dy, '"'),
                                               svg_lines[current_line])
            }
        }
        # In case all values are negative and group labels are drawn on top of the diagram
        # the first text line has to be shifted down to anchor everything lower.
        else if (grepl('dy="-[0-9.]+"', svg_lines[current_line]) && current_context %in% c("group_labels","segment_labels")){
            if (is_group_label_on_top || current_context == "segment_labels"){
                # Count upcoming text lines belonging to this group label
                number_of_upcoming_lines <- 0
                upcoming_dy              <- 0

                for (upcoming_line in seq(current_line + 1, length(svg_lines))){
                    # Stop at end of current text block
                    if (grepl("</text>", svg_lines[upcoming_line], fixed = TRUE)){
                        break
                    }

                    # If a multi text line is found add it to the number of lines
                    if (grepl('dy="[0-9.]+"', svg_lines[upcoming_line]) &&
                        !grepl('dy="-[0-9.]+"', svg_lines[upcoming_line])){

                        number_of_upcoming_lines <- number_of_upcoming_lines + 1

                        # Get the line spacing on the first multi line only because
                        # for all following lines it is the same value.
                        if (upcoming_dy == 0){
                            upcoming_dy <- as.numeric(sub('.*dy="([0-9.]+)".*', '\\1',
                                                          svg_lines[upcoming_line]))
                        }
                    }
                }

                # If there are multiple text lines, they need to be adjusted
                if (number_of_upcoming_lines >= 0){
                    # Current anchor value
                    current_dy <- as.numeric(sub('.*dy="(-[0-9.]+)".*', '\\1',
                                                 svg_lines[current_line]))

                    # Measure saved space, when compressing upcoming lines
                    saved_space <- number_of_upcoming_lines * upcoming_dy * (1 - fine_tuning[["svg_line_height_adjust"]])

                    # Just add the saved space to the anchor point to bring it down
                    new_dy <- current_dy + saved_space

                    # Manipulate entry
                    svg_lines[current_line] <- sub(paste0('dy="', current_dy, '"'),
                                                   paste0('dy="', new_dy, '"'),
                                                   svg_lines[current_line])
                }
            }
        }
    }

    # Put the manipulated lines back together
    svg_string <- paste(svg_lines, collapse = "\n")

    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # Inject Java Script
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    if (is.null(by_info)){ print_step("MINOR", "Inject Java Script") }

    # Put together the whole java script and inject the custom tooltip design
    full_java_script <- sprintf(java_script,
                                visuals[["tooltip_font_color"]],
                                visuals[["tooltip_background_color"]],
                                visuals[["tooltip_border_color"]],
                                visuals[["tooltip_background_opacity"]],
                                visuals[["tooltip_border_width"]],
                                visuals[["font"]],
                                dimensions[["tooltip_font_size"]],
                                visuals[["tooltip_x_padding"]],
                                visuals[["tooltip_y_padding"]],
                                visuals[["tooltip_corner_radius"]])

    # Set up the tooltip texts and inject them into the SVG string as well as the
    # code to trigger the mouse over tooltips.
    values   <- diagram_info[["formatted_values"]]
    category <- c(diagram_info[["individual_groups"]], list(diagram_info[["unique_segments"]]))
    category <- do.call(expand.grid, c(rev(category), stringsAsFactors = FALSE))
    category <- apply(category, 1, function(level){
            paste(rev(level), collapse = "!!!{b}> ")
        })
    value_labels <- rep(diagram_info[["value_labels"]], length(values))

    for(i in seq_along(values)){
        # Tooltip text to display on mouse over
        tooltip <- paste0("{b}", category[i], "!!!––––––––––!!!", value_labels[i], ":{b}", " ", values[i], "{/b}")

        # Capture only the segments which should receive tooltips.
        # All the ids in the SVG files receive a .1 at the end. Don't know why.
        current_segment_part <- paste0('id="tooltip_segment', i, '.1"')

        # Put together the new part
        new_segment_part <- sprintf(
            "%s onmousemove=\"showTooltip(evt, '%s'); this.setAttribute('opacity','0.7');\" onmouseout=\"hideTooltip(); this.setAttribute('opacity','1');\"",
            current_segment_part, tooltip)

        # Swap out the static for the dynamic part
        svg_string <- sub(current_segment_part, new_segment_part, svg_string, fixed = TRUE)
    }

    # This needs to be added to make the tooltips visible
    tooltip_layer <- '<g id="tooltip" visibility="hidden">
                      <rect id="tooltip-bg"/>
                      <text id="tooltip-text"/></g>'

    # At last inject all the java script and tooltips into the SVG file
    svg_string <- sub("</svg>", paste0(full_java_script, "\n",
                                       tooltip_layer, "\n</svg>"),
                      svg_string, fixed = TRUE)

    # Finally save the dynamic SVG file
    writeLines(svg_string, filename)
}


#' Check If gridSVG Is Installed
#'
#' @description
#' gridSVG package is required to get a better structured base SVG file.
#'
#' @return
#' Returns TRUE or FALSE
#'
#' @noRd
check_required_package <- function(){
    # Check if gridSVG is available without loading it into the global namespace
    if (!requireNamespace("gridSVG", quietly = TRUE)){
        # Set up menu message
        menu_message <- paste("\n\nThe <gridSVG> package is required for interactive charts but is not currently installed.",
                              "Would you like to install it now?", sep = "\n")

        # Ask the user whether to install gridSVG or abort execution
        user_choice <- utils::menu(title   = menu_message,
                                   choices = c("Yes", "No (abort execution)"))

        # If the user chooses to install the package do so automatically
        if (user_choice == 1){
            print_message("NOTE", "Installing <gridSVG> package.")

            utils::install.packages("gridSVG")

            # Double check if installation was successful
            if (!requireNamespace("gridSVG", quietly = TRUE)){
                print_message("NOTE", "Installation failed. Try  to install <gridSVG> manually via install.packages('gridSVG').")

                return(invisible(FALSE))
            }

            print_message("NOTE", "Installing successful.")
        }
        # If the user doesn't want to install the package, abort
        else{
            print_message("ERROR", c("<gridSVG> is required to generate an interactive SVG graphic.",
                                     "Graphic generation will be aborted."))

            return(invisible(FALSE))
        }
    }

    TRUE
}

#' Check If gridSVG Is Installed
#'
#' @description
#' This is the main java script code to make tooltips pop up and hide again. This
#' code also makes all the dynamic styling work. The whole coloring and formatting
#' takes place here.
#'
#' @noRd
java_script <- '<script type="application/ecmascript"><![CDATA[

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Grab the styling values from the R code and inject them into global variables
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

var tooltipOptions = {
    fontColor:   "%s",
    fillColor:   "%s",
    borderColor: "%s",
    fillOpacity:  %s,
    borderWidth:  %s,
    fontFamily:  "%s",
    fontSize:     %s,
    paddingX:     %s,
    paddingY:     %s,
    cornerRadius: %s
};

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Set up the function that makes the toolstip pop up
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

function showTooltip(evt, text){
    // Dom element selection
    var svg     = evt.target.ownerSVGElement;
    var tooltip = document.getElementById("tooltip");
    var label   = document.getElementById("tooltip-text");
    var bg      = document.getElementById("tooltip-bg");

    // Map coordinates according to the canvas itself
    var pt     = svg.createSVGPoint();
    pt.x       = evt.clientX;
    pt.y       = evt.clientY;
    var cursor = pt.matrixTransform(svg.getScreenCTM().inverse());

    // Font must be scaled according to the current displayed size of the graphic.
    // The zoom has to be countered basically.
    var scaleFactor     = svg.viewBox.baseVal.width / svg.getBoundingClientRect().width;
    var adaptedFontSize = parseFloat(tooltipOptions.fontSize) * scaleFactor;
    var adaptedPaddingX = parseFloat(tooltipOptions.paddingX) * scaleFactor;
    var adaptedPaddingY = parseFloat(tooltipOptions.paddingY) * scaleFactor;

    // Set up font
    label.setAttribute("font-family", tooltipOptions.fontFamily);
    label.setAttribute("font-size", adaptedFontSize + "px");

    // Clear previous multi line text element
    while(label.firstChild) { label.removeChild(label.firstChild); }

    // Put together and format multi line tooltips
    var lines = text.split("!!!");

    for(var lineIndex = 0; lineIndex < lines.length; lineIndex++){
        var line = lines[lineIndex];
        var bold = false;
        var xPos = adaptedPaddingX;

        // Current line is cut up into pieces which determine whether parts of
        // the text are drawn in bold or in normal font weight.
        var pieces = line.split(/(\\{b\\}|\\{\\/b\\})/);

        for(var pieceIndex = 0; pieceIndex < pieces.length; pieceIndex++){
            var piece = pieces[pieceIndex];

            // Set bold entry and exit points
            if(piece === "{b}")  { bold = true;  continue; }
            if(piece === "{/b}") { bold = false; continue; }
            if(piece === "")     {               continue; }

            // Create and append inline sub-text tag (tspan)
            var tspan = document.createElementNS("http://www.w3.org/2000/svg", "tspan");

            tspan.textContent = piece;
            tspan.setAttribute("font-weight", bold ? "bold" : "normal");
            tspan.setAttribute("x", xPos);
            tspan.setAttribute("y", adaptedPaddingY + adaptedFontSize + (lineIndex * adaptedFontSize * 1.3));

            label.appendChild(tspan);

            // Advance cursor by the pixel width of rendered piece
            xPos += tspan.getComputedTextLength();
        }
    }

    // Calculate the tooltip box dimensions
    var bbox = label.getBBox();

    var tooltipWidth  = bbox.width + (2 * adaptedPaddingX);
    var tooltipHeight = bbox.height + (2 * adaptedPaddingY);

    //-------------------------------------------------------------------------
    // Function which sets the text color dynamically to black or white depending
    // on the tooltip background color
    //-------------------------------------------------------------------------

    function getContrastColor(color){
        var rgb = color.match(/\\d+/g);
        if(!rgb || rgb.length < 3) { return "black"; }

        // Calculate YIQ relative luminance formula to dynamically check light vs. dark
        var brightness = (parseInt(rgb[0]) * 299 + parseInt(rgb[1]) * 587 + parseInt(rgb[2]) * 114) / 1000;
        return brightness > 128 ? "black" : "white";
    }

    //-------------------------------------------------------------------------
    // Formatting tooltips
    //-------------------------------------------------------------------------

    var tooltipTextColor, tooltipFillColor, tooltipBorderColor;

    // On auto grab the color of the respective segment
    if(tooltipOptions.fillColor === "auto"){
        tooltipFillColor = window.getComputedStyle(evt.target).fill;
    }
    // Otherwise get the user defined color
    else{
        tooltipFillColor = tooltipOptions.fillColor;
    }

    // On auto dynamically set the color to black or white
    if(tooltipOptions.fontColor === "auto"){
        tooltipTextColor = getContrastColor(tooltipFillColor);
    }
    // Otherwise get the user defined color
    else{
        tooltipTextColor = tooltipOptions.fontColor;
    }

    // On auto grab the color of the respective segment
    if(tooltipOptions.borderColor === "auto"){
        tooltipBorderColor = window.getComputedStyle(evt.target).fill;
    }
    // Otherwise get the user defined color
    else{
        tooltipBorderColor = tooltipOptions.borderColor;
    }

    // Apply user defined style options
    bg.setAttribute("fill", tooltipFillColor);
    bg.setAttribute("fill-opacity", tooltipOptions.fillOpacity);
    bg.setAttribute("stroke", tooltipBorderColor);
    bg.setAttribute("stroke-width", parseFloat(tooltipOptions.borderWidth) * scaleFactor);
    bg.setAttribute("rx", parseFloat(tooltipOptions.cornerRadius) * scaleFactor);
    bg.setAttribute("ry", parseFloat(tooltipOptions.cornerRadius) * scaleFactor);
    label.setAttribute("fill", tooltipTextColor);

    // Align background
    bg.setAttribute("x", bbox.x - adaptedPaddingX);
    bg.setAttribute("y", bbox.y - adaptedPaddingY);
    bg.setAttribute("width", tooltipWidth);
    bg.setAttribute("height", tooltipHeight);

    //-------------------------------------------------------------------------
    // Tooltips are to be drawn inside the graphic. Which means, they are right
    // or left adjusted depending on which side of the graphic the tooltip pops
    // up. Same with top or bottom part, while this isnt as relevant.
    //-------------------------------------------------------------------------

    var svgWidth  = svg.viewBox.baseVal.width;
    var svgHeight = svg.viewBox.baseVal.height;

    // Evaluate layout space position quadrant rules
    var leftHalf   = cursor.x < svgWidth  / 2;
    var bottomPart = cursor.y < svgHeight / 4;
    var offset     = 10 * scaleFactor;

    // Compute Flip Horizontal Axis Boundary Collision
    var tooltipX = leftHalf ? (cursor.x + offset) : (cursor.x - tooltipWidth - offset);

    // Compute Flip Vertical Axis Boundary Collision
    var tooltipY = bottomPart ? (cursor.y + offset) : (cursor.y - tooltipHeight - offset);

    // Transform coordinate group placement matrix and release display
    tooltip.setAttribute("transform", "translate(" + tooltipX + "," + tooltipY + ")");
    tooltip.setAttribute("visibility", "visible");
}

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Hide tooltip
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

function hideTooltip(){
    document.getElementById("tooltip").setAttribute("visibility", "hidden");
}

]]></script>'
