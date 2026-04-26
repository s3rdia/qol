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
