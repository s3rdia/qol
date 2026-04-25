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
