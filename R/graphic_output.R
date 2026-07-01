#' Output Graphic To Device
#'
#' @description
#' Draw or output a graphic to a desired destination.
#'
#' @param graphic_object The complete graphic to be drawn or exported.
#' @param dimensions Dimension parameters set with [graphic_dimensions()].
#' @param fine_tuning Fine tuning parameters set with [graphic_fine_tuning()].
#' @param output Output parameters set with [graphic_output()].
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
            print_message("WARNING", "Path does not exist: [path]", path = output[["save_path"]])

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

            # Set extensions to svg on interactive outputs
            if (output[["interactive"]] && extension != "svg"){
                extension <- "svg"
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


#' @description
#' Output a grid of graphics.
#'
#' @return
#' Returns NULL or the grid graphic object.
#'
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
#' Returns NULL or the input graphic object.
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
    full_java_script <- paste(readLines(system.file("extdata", "qol_js_tooltip.txt", package = "qol"), encoding = "UTF-8", warn = FALSE), collapse = "\n")
    full_java_script <- gsub("%FONT_COLOR%",    visuals[["tooltip_font_color"]],         full_java_script, fixed = TRUE)
    full_java_script <- gsub("%FILL_COLOR%",    visuals[["tooltip_background_color"]],   full_java_script, fixed = TRUE)
    full_java_script <- gsub("%BORDER_COLOR%",  visuals[["tooltip_border_color"]],       full_java_script, fixed = TRUE)
    full_java_script <- gsub("%FILL_OPACITY%",  visuals[["tooltip_background_opacity"]], full_java_script, fixed = TRUE)
    full_java_script <- gsub("%BORDER_WIDTH%",  visuals[["tooltip_border_width"]],       full_java_script, fixed = TRUE)
    full_java_script <- gsub("%FONT_FAMILY%",   visuals[["font"]],                       full_java_script, fixed = TRUE)
    full_java_script <- gsub("%FONT_SIZE%",     dimensions[["tooltip_font_size"]],       full_java_script, fixed = TRUE)
    full_java_script <- gsub("%PADDING_X%",     visuals[["tooltip_x_padding"]],          full_java_script, fixed = TRUE)
    full_java_script <- gsub("%PADDING_Y%",     visuals[["tooltip_y_padding"]],          full_java_script, fixed = TRUE)
    full_java_script <- gsub("%CORNER_RADIUS%", visuals[["tooltip_corner_radius"]],      full_java_script, fixed = TRUE)
    full_java_script <- gsub("%GROUP_COLOR%",   visuals[["group_hover_color"]],          full_java_script, fixed = TRUE)

    #-------------------------------------------------------------------------#
    # Set up segment tooltips
    #-------------------------------------------------------------------------#

    # Set up the tooltip texts and inject them into the SVG string as well as the
    # code to trigger the mouse over tooltips.
    values         <- diagram_info[["formatted_values"]]
    tooltip_header <- build_tooltip_header(c(diagram_info[["individual_groups"]], list(diagram_info[["unique_segments"]])))
    value_labels   <- rep(diagram_info[["value_labels"]], length(values))

    # Gather the segment tooltips
    for(i in seq_along(values)){
        # Tooltip text to display on mouse over
        tooltip <- paste0(tooltip_header[i], "!!!\u2013\u2013\u2013\u2013\u2013\u2013\u2013\u2013\u2013\u2013!!!", value_labels[i], "!!!{b}", " ", values[i], "{/b}")

        # Capture only the segments which should receive tooltips.
        # All the ids in the SVG files receive a .1 at the end. Don't know why.
        current_segment_part <- paste0('id="tooltip_segment', i, '.1"')

        # Put together the new part
        new_segment_part <- sprintf(
            "%s onmousemove=\"showTooltip(evt, '%s'); this.setAttribute('opacity','%s');\" onmouseout=\"hideTooltip(); this.setAttribute('opacity','1');\"",
            current_segment_part, tooltip, visuals[["segment_hover_opacity"]])

        # Swap out the static for the dynamic part
        svg_string <- sub(current_segment_part, new_segment_part, svg_string, fixed = TRUE)
    }

    #-------------------------------------------------------------------------#
    # Set up group tooltips
    #-------------------------------------------------------------------------#

    number_of_groups   <- diagram_info[["number_of_groups"]]
    number_of_segments <- diagram_info[["number_of_segments"]]

    values         <- matrix(diagram_info[["formatted_values"]], nrow = number_of_groups, byrow = TRUE)
    tooltip_header <- build_tooltip_header(c(rev(diagram_info[["individual_groups"]])))

    segment_labels <- diagram_info[["unique_segments"]]
    value_label    <- diagram_info[["value_labels"]]

    # Gather the group tooltips
    for(i in seq_len(number_of_groups)){
        segment_lines <- paste0("{legend:", diagram_info[["colors_to_use"]][1:diagram_info[["number_of_segments"]]],"}",
                                segment_labels, "\u00A0:\u00A0{b}", values[i, ], "{/b}")

        # Tooltip text to display on mouse over
        tooltip <- paste0(tooltip_header[i], "{/b}", "!!!\u2013\u2013\u2013\u2013\u2013\u2013\u2013\u2013\u2013\u2013", "!!!", value_label, "!!!",
                          paste(segment_lines, collapse = "!!!"))

        # Capture only the segments which should receive tooltips.
        # All the ids in the SVG files receive a .1 at the end. Don't know why.
        current_group_part <- paste0('id="tooltip_group', i, '.1"')

        # Put together the new part
        new_group_part <- sprintf(
            "%s opacity=\"0\" onmousemove=\"showTooltip(evt, '%s'); this.setAttribute('opacity','%s');\" onmouseout=\"hideTooltip(); this.setAttribute('opacity','0');\"",
            current_group_part, tooltip, visuals[["group_hover_opacity"]])

        # Swap out the static for the dynamic part
        svg_string <- sub(current_group_part, new_group_part, svg_string, fixed = TRUE)
    }

    #-------------------------------------------------------------------------#
    # Finalize java script code
    #-------------------------------------------------------------------------#

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


#' Build The Tooltip Header From Axes Group Labels
#'
#' @description
#' Stacks the group labels as a header text for the pop up tooltips, when hovering
#' over segments and groups with the mouse on interactive graphics.
#'
#' @param group_labels All group labels that should be put together as header text.
#'
#' @return
#' Returns a character containing the tooltip header text.
#'
#' @noRd
build_tooltip_header <- function(group_labels){
    # Combine list entries as cartesian product into data frame
    group_labels <- do.call(expand.grid, c(rev(group_labels), stringsAsFactors = FALSE))

    # If there is only one column in the data frame then there is only one header
    # category and therefore no need for the subheader building.
    if (collapse::fncol(group_labels) == 1){
        apply(group_labels, 1, function(level){
            paste0("{b}[ ", toupper(level), " ]")
        })
    }
    # With more than one column put them together as individual nested levels.
    # The header categories will be stacked.
    else{
        apply(group_labels, 1, function(level){
            level <- rev(level)

            paste0("{b}\u00A0\u00A0",
                   paste(level[-length(level)], collapse = "!!!{b}: "),
                   "!!!{b}: [ ", toupper(level[length(level)]), " ]")
        })
    }
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
