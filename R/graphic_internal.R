###############################################################################
# Get dimensions
###############################################################################
#' Get The Diagram Start In Cm
#'
#' @description
#' Get the actual diagram start from the top in cm. If a title is set then the
#' titles height is subtracted with additional margins from the top.
#'
#' @param dimensions Dimension parameters set with [graphic_dimensions()].
#' @param title_height Height of the graphic titles.
#'
#' @return
#' Returns a numeric width in cm.
#'
#' @noRd
get_diagram_start_cm <- function(dimensions   = .qol_options[["graphic_dimensions"]],
                                 title_height = 0){
    start <- dimensions[["diagram_start_top"]]

    # Measure diagram start automatically and set it right under the title or
    # take the manually set start.
    if (start == "auto"){
        # Calculate new diagram viewport y starting position
        valid_heights <- sum(title_height > 0)
        start         <- (dimensions[["graphic_height"]]
                          - (title_height
                          + (dimensions[["margins"]] * 2)))
    }
    else{
        start <- dimensions[["graphic_height"]] - start
    }

    invisible(start)
}


#' Get The Diagram Width In Cm
#'
#' @description
#' Get the actual diagram width in cm. Margins on both sides are subtracted.
#'
#' @param dimensions Dimension parameters set with [graphic_dimensions()].
#' @param visuals Visual parameters set with [graphic_visuals()].
#' @param stacked FALSE by default. If TRUE, the segments are stacked instead of grouped.
#' Only possible for certain diagram types.
#'
#' @return
#' Returns a numeric width in cm.
#'
#' @noRd
get_diagram_width_cm <- function(dimensions  = .qol_options[["graphic_dimensions"]],
                                 visuals     = .qol_options[["graphic_visuals"]],
                                 fine_tuning = .qol_options[["fine_tuning"]],
                                 stacked     = FALSE,
                                 segments    = ""){
    width <- dimensions[["diagram_width"]]

    # Measure diagram width automatically and set it to span from side to side or
    # take the manually set height.
    if (width == "auto"){
        width <- (dimensions[["graphic_width"]] - (dimensions[["margins"]] * 2))

        # When drawing stacked diagrams with direct labels, reserve space on the
        # side for the texts.
        # TODO: THIS WILL NARROW THE WIDTH ON HORIZONTAL BARS AS WELL, EVEN THOUGH
        #       LABELS WON'T BE AT THE SIDE!
        if (stacked && visuals[["segment_label_type"]] == "lines"){
            label_width <- max(get_text_width(segments, "label", dimensions, visuals, "cm")) +
                           grid::convertWidth(grid::unit(fine_tuning[["segment_line_length_stacked"]] + fine_tuning[["diagram_margin"]], "npc"), "cm", valueOnly = TRUE)

            width <- width - min(dimensions[["textbox_width"]], label_width)
        }
    }

    invisible(width)
}


#' Get The Diagram Height In Cm
#'
#' @description
#' Get the actual diagram height in cm. If a titles and footnotes are set then
#' their height is subtracted with additional margins from.
#'
#' @param dimensions Dimension parameters set with [graphic_dimensions()].
#' @param title_height Height of the graphic titles.
#' @param footnote_height Height of the graphic footnotes
#' @param origin_height Height of the graphic origin text.
#'
#' @return
#' Returns a numeric height in cm.
#'
#' @noRd
get_diagram_height_cm <- function(dimensions      = .qol_options[["graphic_dimensions"]],
                                  title_height    = 0,
                                  footnote_height = 0,
                                  origin_height   = 0){
    height <- dimensions[["diagram_height"]]

    # If no footnote is set, take the origin height
    footnote_height <- ifelse(footnote_height == 0, origin_height, footnote_height)

    # Measure diagram height automatically and set it to span between title and
    # footnotes or take the manually set height.
    if (height == "auto"){
        # Calculate new diagram viewport height
        height <- (dimensions[["graphic_height"]]
                   - (title_height
                   + footnote_height
                   + (dimensions[["margins"]] * 5)))
    }

    invisible(height)
}


###############################################################################
# Adjust labels
###############################################################################
#' Decollision Segment Labels Horizontal
#'
#' @description
#' Tries to decollide segment labels, which are drawn with segment lines on the
#' same height. The function basically shifts the labels a bit outward which is
#' not a guarantee that they are actually not colliding. But this is enough for
#' smaller labels.
#'
#' @param segment_labels The actual segment label texts.
#' @param x_label_positions The current label x positions.
#' @param dimensions Dimension parameters set with [graphic_dimensions()].
#' @param visuals Visual parameters set with [graphic_visuals()].
#' @param fine_tuning Fine tuning parameters set with [graphic_fine_tuning()].
#'
#' @return
#' Returns adjusted segment label x positions.
#'
#' @noRd
decollide_group_labels <- function(segment_labels,
                                   x_label_positions,
                                   dimensions,
                                   visuals,
                                   fine_tuning){
    number_of_labels <- length(segment_labels)

    # If there are no labels or just one, nothing can overlap
    if (number_of_labels <= 1){
        return(x_label_positions)
    }

    # Maximum shift is a bit more than half of the textbox width, meaning a label
    # can only move half its own size away from its centered point, so its far
    # edge ends up at the segment line at most. A bit more than half because
    # somehow otherwise labels get shifted too far occasionally.
    half_widths <- get_text_width(segment_labels, "labels", dimensions, visuals) / 2.5
    max_shifts  <- rep(half_widths, number_of_labels)

    # Split labels into a left half and right half. With an odd label count the
    # exact middle label belongs to neither half and is used only as a fixed anchor
    # without moving itself.
    has_middle_label <- number_of_labels %% 2 == 1
    left_size        <- floor(number_of_labels / 2)
    right_start      <- left_size + 1 + as.integer(has_middle_label)
    middle_index     <- left_size + 1

    # The first comparison on each side needs a fixed reference: the static
    # middle label on an uneven segment number or with an even number of segments
    # the other side's nearest label, at whatever position it currently
    # holds.
    if (has_middle_label){
        anchor_for_left  <- middle_index
        anchor_for_right <- middle_index
    }
    else{
        anchor_for_left  <- right_start
        anchor_for_right <- left_size
    }

    # Move from middle to outward first. Left and right side will be processed
    # independently.
    movable_left  <- left_size:1
    movable_right <- right_start:number_of_labels

    side_groups <- list(list(movable = movable_left,  anchor = anchor_for_left,  direction = -1), # away from middle -> left
                        list(movable = movable_right, anchor = anchor_for_right, direction =  1)) # away from middle -> right

    shifted_x_positions <- x_label_positions

    for (side in side_groups){
        # Pass 1: push labels apart, away from the middle. The anchor is
        # prepended so the nearest movable label's first overlap check is
        # against it. The anchor itself is only the reference and never
        # the one being shifted.
        shifted_x_positions <- push_labels_apart(c(side[["anchor"]], side[["movable"]]),
                                                 shifted_x_positions,
                                                 half_widths,
                                                 max_shifts,
                                                 side[["direction"]])

        # No second pass needed if there is only one moveable label
        if (length(side[["movable"]]) <= 1){
            next
        }

        # Pass 2: pull back toward the middle, resolving any overlaps pass 1
        # left behind when a label got clamped at max_shift. Only the
        # movable labels are involved.
        shifted_x_positions <- push_labels_apart(rev(side[["movable"]]),
                                                 shifted_x_positions,
                                                 half_widths,
                                                 max_shifts,
                                                 -side[["direction"]])
    }

    shifted_x_positions
}


#' Decollision Segment Labels Vertical
#'
#' @description
#' Tries to decollide segment labels, which are drawn beside stacked segments.
#' Labels are shifted up and down between the corresponding segment bounds to try
#' to decollide labels. This is not a guarantee that labels are actually not
#' colliding, but it is enough for smaller labels.
#'
#' @param segment_labels The actual segment label texts.
#' @param y_label_positions The current label y positions in native units.
#' @param diagram_info The list of measurements generated by [get_diagram_dimensions()].
#' @param dimensions Dimension parameters set with [graphic_dimensions()].
#' @param visuals Visual parameters set with [graphic_visuals()].
#' @param fine_tuning Fine tuning parameters set with [graphic_fine_tuning()].
#'
#' @return
#' Returns adjusted segment label y positions.
#'
#' @noRd
decollide_stack_labels <- function(segment_labels,
                                   y_label_positions,
                                   diagram_info,
                                   dimensions,
                                   visuals,
                                   fine_tuning){
    # If there are no labels or just one, nothing can overlap
    if (length(segment_labels) <= 1) {
        return(y_label_positions)
    }

    # Maximum shift per label is half the segment height of the last stack. This
    # is because labels are centered to the segments.
    stacked_heights <- diagram_info[["stacked_heights"]]
    last_heights    <- unlist(stacked_heights[length(stacked_heights)])
    max_shifts      <- abs(last_heights) / 2

    half_sizes <- get_text_height(segment_labels, "labels", dimensions, visuals) / 2

    # Positive and negative stacks will be processed independently
    sign_groups <- list(list(indices = which(last_heights >= 0), direction =  1), # away from zero -> up
                        list(indices = which(last_heights <  0), direction = -1)) # away from zero -> down

    shifted_y_positions <- y_label_positions

    for (group in sign_groups){
        # Nothing to decollide within this group
        if (length(group[["indices"]]) <= 1){
            next
        }

        # Pass 1: push labels apart, away from zero. With this it can happen, that
        # labels are pushed into each other. Which is why there is a second pass.
        shifted_y_positions <- push_labels_apart(group[["indices"]],
                                                 shifted_y_positions,
                                                 half_sizes,
                                                 max_shifts,
                                                 group[["direction"]])

        # Pass 2: pull back toward zero, resolving any overlaps the previous pass
        # produced. Same operation as before, just reversed.
        shifted_y_positions <- push_labels_apart(rev(group[["indices"]]),
                                                 shifted_y_positions,
                                                 half_sizes,
                                                 max_shifts,
                                                 -group[["direction"]])
    }

    shifted_y_positions
}


#' Push Neighboring Labels Apart
#'
#' @description
#' Loops through all labels and pushes each label apart from the previous one
#' whenever their label boxes overlap, clamped at the boundaries of the corresponding
#' segments.
#'
#' @param ordered_indices A list to tell apart positive and negative values.
#' @param positions The original label positions.
#' @param half_sizes Half the text heights of each label.
#' @param max_shifts The maximum allowed shifts, which correspond to the individual
#' segment heights.
#' @param direction The direction of the pass.
#'
#' @return
#' Returns adjusted segment label positions.
#'
#' @noRd
push_labels_apart <- function(ordered_indices,
                              positions,
                              half_sizes,
                              max_shifts,
                              direction){
    for (step in 2:length(ordered_indices)){
        previous_label <- ordered_indices[step - 1]
        current_label  <- ordered_indices[step]

        # Check whether half the text heights of neighboring labels are greater
        # than the distance between the two labels.
        overlap <- (direction * (positions[previous_label] - positions[current_label])) +
                                 half_sizes[previous_label] + half_sizes[current_label]

        # If labels are overlapping, add the smallest bit to the current label
        # position to decollide them.
        if (overlap > 0){
            positions[current_label] <- positions[current_label] + direction * min(overlap, max_shifts[current_label])
        }
    }

    positions
}
