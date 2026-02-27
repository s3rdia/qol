###############################################################################
# Colors
###############################################################################
#' Set Up Basic Color Themes
#'
#' @description
#' Set up basic color themes for global options.
#'
#' @return
#' Returns a named list.
#'
#' @noRd
base_color_themes <- function(){
    greys <- c("#2B2B2B", "#5A5A5A", "#9A9A9A", "#DADADA")

    list(ocean = c(
            "#12324A", "#1F4E6E", "#3E6F8E", "#7EB1C8", "#A7C9D8", "#E5F0F6", greys),

         sunset = c(
            "#5A1E22", "#7F2F2E", "#9C5A2F", "#D38A45", "#F0BF78", "#F6D49B", greys),

         ember = c(
            "#4A1423", "#6B2335", "#7A3F57", "#AF8692", "#D7B8BE", "#E9D3D6", greys),

         forest = c(
            "#15342A", "#2C5A42", "#4F6F4A", "#8EA872", "#B1C48E", "#E6EFD8", greys),

         lagoon = c(
            "#0C3A42", "#1E5F68", "#2F7F86", "#75B9BA", "#9ACCCD", "#E3F2F2", greys),

         dusk = c(
            "#2D1F4A", "#45346F", "#5E4E7A", "#9388B1", "#B8ACCA", "#ECE8F2", greys),

         clay = c(
            "#4A2A1E", "#6E4432", "#7E5E46", "#B6947A", "#DED0C1", "#EFE6DD", greys),

         olive_gold = c(
            "#3E4415", "#646B2A", "#7F8538", "#BFC27F", "#E6E0B3", "#F4F1D6", greys),

         steel = c(
            "#273A4F", "#3B546E", "#5E6F82", "#9FB1C2", "#C1CCD7", "#EDF1F5", greys),

         graphite = c(
            "#0F0F10", "#1E1E20", "#2F2F32", "#444448", "#5E5E63",
            "#7A7A80", "#9A9AA0", "#BEBEC3", "#E0E0E3", "#F2F2F4"),

         aurora = c(
            "#1A2438", "#1F3A52", "#275C6E", "#2E7F86", "#4EA18F",
            "#7BBE87", "#A8D68A", "#D1E39C", "#EAF0C4", "#F6F8E6"),

         forest_sun = c(
            "#1F2B1C", "#2E4225", "#456633", "#6E8C3F", "#9FB552",
            "#C7D56A", "#E2EA8E", "#F0F4B5", "#F8F9D9", "#FCFDEE"),

         clay_mint = c(
            "#3A2A1F", "#5A3E2A", "#7A5734", "#9A7A42", "#8FA15B",
            "#6EB07A", "#7FC9A1", "#A8DEC2", "#D4F0E3", "#F0FAF6"),

         violet_gold = c(
            "#2A1F3D", "#3E2A5C", "#56387A", "#70509A", "#8E75B5",
            "#AFA0C8", "#CFC3C5", "#E6D8A8", "#F3E8C7", "#FBF6E8"),

         night_lime = c(
            "#182624", "#1F3F3A", "#2C6257", "#3E8771", "#5AAD7E",
            "#7FCC7A", "#A6E37B", "#D0F09A", "#ECF8C9", "#F8FCE9"),

         plum_citrus = c(
            "#321826", "#4A2135", "#663047", "#83485D", "#9E6E75",
            "#B89A8A", "#D0C28F", "#E5DEA7", "#F3F0CE", "#FBF9EC"),

         violet_fire = c(
             "#280959", "#4b1d91", "#731498", "#a1119d", "#d23194",
             "#ef6375", "#f28066", "#f2a65f", "#eebd6e", "#eebd6e"))
}


#' Set Up Basic Font Colors Themes
#'
#' @description
#' Set up fitting font colors for base theme colors in global options.
#'
#' @return
#' Returns a named list.
#'
#' @noRd
font_color_themes <- function(){
    greys <- c("#FFFFFF", "#FFFFFF", "#000000", "#000000")

    list(ocean = c(
            "#FFFFFF", "#FFFFFF", "#FFFFFF", "#000000", "#000000", "#000000", greys),

        sunset = c(
            "#FFFFFF", "#FFFFFF", "#FFFFFF", "#000000", "#000000", "#000000", greys),

        ember = c(
            "#FFFFFF", "#FFFFFF", "#FFFFFF", "#000000", "#000000", "#000000", greys),

        forest = c(
            "#FFFFFF", "#FFFFFF", "#FFFFFF", "#000000", "#000000", "#000000", greys),

        lagoon = c(
            "#FFFFFF", "#FFFFFF", "#FFFFFF", "#000000", "#000000", "#000000", greys),

        dusk = c(
            "#FFFFFF", "#FFFFFF", "#FFFFFF", "#000000", "#000000", "#000000", greys),

        clay = c(
            "#FFFFFF", "#FFFFFF", "#FFFFFF", "#000000", "#000000", "#000000", greys),

        olive_gold = c(
            "#FFFFFF", "#FFFFFF", "#FFFFFF", "#000000", "#000000", "#000000", greys),

        steel = c(
            "#FFFFFF", "#FFFFFF", "#FFFFFF", "#000000", "#000000", "#000000", greys),

        graphite = c(
            "#FFFFFF", "#FFFFFF", "#FFFFFF", "#FFFFFF", "#FFFFFF",
            "#FFFFFF", "#000000", "#000000", "#000000", "#000000"),

        aurora = c(
            "#FFFFFF", "#FFFFFF", "#FFFFFF", "#FFFFFF", "#FFFFFF",
            "#000000", "#000000", "#000000", "#000000", "#000000"),

        forest_sun = c(
            "#FFFFFF", "#FFFFFF", "#FFFFFF", "#FFFFFF", "#000000",
            "#000000", "#000000", "#000000", "#000000", "#000000"),

        clay_mint = c(
            "#FFFFFF", "#FFFFFF", "#FFFFFF", "#FFFFFF", "#FFFFFF",
            "#000000", "#000000", "#000000", "#000000", "#000000"),

        violet_gold = c(
            "#FFFFFF", "#FFFFFF", "#FFFFFF", "#FFFFFF", "#FFFFFF",
            "#000000", "#000000", "#000000", "#000000", "#000000"),

        night_lime = c(
            "#FFFFFF", "#FFFFFF", "#FFFFFF", "#FFFFFF", "#000000",
            "#000000", "#000000", "#000000", "#000000", "#000000"),

        plum_citrus = c(
            "#FFFFFF", "#FFFFFF", "#FFFFFF", "#FFFFFF", "#000000",
            "#000000", "#000000", "#000000", "#000000", "#000000"),

        violet_fire = c(
            "#FFFFFF", "#FFFFFF", "#FFFFFF", "#FFFFFF", "#FFFFFF",
            "#000000", "#000000", "#000000", "#000000", "#000000"))
}


#' Create The Default Global Theme List
#'
#' @description
#' Creates the default global list of themes consisting of the base and corresponding
#' font colors.
#'
#' @return
#' Returns a named list.
#'
#' @noRd
create_global_themes <- function(){
    global_list <- list()
    base_colors <- base_color_themes()
    font_colors <- font_color_themes()

    # Loop through provided colors and check if they are valid
    for (i in seq_along(base_colors)){
        name <- names(base_colors)[i]
        base <- base_colors[[i]]
        font <- font_colors[[i]]

        # Build theme list
        theme <- stats::setNames(list(list(base = base,
                                           font = font)),
                          name)

        # Add to global list
        global_list <- c(global_list, theme)
    }

    global_list
}


# These are the two main sequences in which colors are used. The first one is just
# sequential from 1 to 10, the second one skips colors to use the whole color range
# as much as possible and to put colors with more contrasts together (if the colors
# themselves are ordered sequentially).
sequential_usage <- list(c(1),
                         c(1, 2),
                         c(1, 2, 3),
                         c(1, 2, 3, 4),
                         c(1, 2, 3, 4, 5),
                         c(1, 2, 3, 4, 5, 6),
                         c(1, 2, 3, 4, 5, 6, 7),
                         c(1, 2, 3, 4, 5, 6, 7, 8),
                         c(1, 2, 3, 4, 5, 6, 7, 8, 9),
                         c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))

contrast_usage <- list(c(1),
                       c(1, 3),
                       c(1, 3, 5),
                       c(1, 3, 5, 7),
                       c(1, 3, 5, 7, 9),
                       c(1, 2, 3, 5, 7, 9),
                       c(1, 2, 3, 4, 5, 7, 9),
                       c(1, 2, 3, 4, 5, 6, 7, 9),
                       c(1, 2, 3, 4, 5, 6, 7, 8, 9),
                       c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))


#' Managing Global Color Themes
#'
#' @name graphic_themes
#'
#' @description
#' [add_color_theme()]: Adds a new color theme containing base and font colors to the
#' global theme list.
#'
#' @param theme_name The name of the theme.
#' @param base_colors The base colors for the segments.
#' @param font_colors The individual font colors used on the corresponding base color.
#'
#' @return
#' [add_color_theme()]: Returns modified global theme list.
#'
#' @seealso
#' Graphic functions: [design_graphic()]
#'
#' Global graphic options: [add_color_theme()], [get_theme_base_colors()], [get_theme_font_colors()],
#' [reset_color_themes()]
#'
#' View colors and themes: [display_colors()], [display_themes()]
#'
#' @examples
#' # Adding and getting color themes
#' add_color_theme(theme_name  = "tropic",
#'                 base_colors = c("#1B3A2E", "#1F5A3F", "#257F54", "#2FA36A", "#56C07E",
#'                                 "#85D49A", "#B2E4B8", "#D7F0D5", "#ECF8EB", "#F7FCF7"),
#'                 font_colors = c("#FFFFFF", "#FFFFFF", "#FFFFFF", "#FFFFFF", "#000000",
#'                                 "#000000", "#000000", "#000000", "#000000", "#000000"))
#'
#' @rdname graphic_themes
#'
#' @export
add_color_theme <- function(theme_name,
                            base_colors,
                            font_colors = c("#FFFFFF", "#FFFFFF", "#FFFFFF", "#FFFFFF", "#000000",
                                            "#000000", "#000000", "#000000", "#000000", "#000000")){
    theme_name <- get_origin_as_char(theme_name, substitute(theme_name))

    if (length(base_colors) == 0){
        message(" X ERROR: No base colors provided. Color theme won't be added.")
        return(invisible(.qol_options[["graphic_themes"]]))
    }

    if (length(font_colors) == 0){
        message(" X ERROR: No font colors provided. Color theme won't be added.")
        return(invisible(.qol_options[["graphic_themes"]]))
    }

    # Loop through provided base colors and check if they are valid
    for (single_color in base_colors){
        if (!grepl("^#?[A-Fa-f0-9]{6}$", single_color)){
            message(" X ERROR: Base color '", single_color, "' must be a 6 character <hex code>. Color theme won't be added.")
            return(invisible(.qol_options[["graphic_themes"]]))
        }
    }

    # Loop through provided font colors and check if they are valid
    for (single_color in font_colors){
        if (!grepl("^#?[A-Fa-f0-9]{6}$", single_color)){
            message(" X ERROR: Font color '", single_color, "' must be a 6 character <hex code>. Color theme won't be added.")
            return(invisible(.qol_options[["graphic_themes"]]))
        }
    }

    # Build theme list
    theme <- stats::setNames(list(list(base = base_colors,
                                       font = font_colors)),
                             theme_name)

    # Update the internal global theme list
    .qol_options[["graphic_themes"]] <- utils::modifyList(.qol_options[["graphic_themes"]], theme)
    invisible(.qol_options[["graphic_themes"]])
}


#' @description
#' [get_theme_colors()]: Retrieve a list with two vectors of hex colors from the
#' globally set up themes containing the base colors for the segments and the
#' corresponding font colors.
#'
#' @param theme_name The theme name to look up.
#'
#' @return
#' [get_theme_colors()]: A list with two vectors of hex color codes.
#'
#' @examples
#' get_theme_colors("ocean")
#'
#' @rdname graphic_themes
#'
#' @export
get_theme_colors <- function(theme_name){
    if (length(theme_name) > 1){
        message(" ! WARNING: Only a single theme can be retrieved. First vector element will be used.")

        theme_name <- theme_name[[1]]
    }

    if (!theme_name %in% names(.qol_options[["graphic_themes"]])){
        message(" X ERROR: Theme '", theme_name, "' doesn't exist. Default theme will be used.")
        return(invisible(.qol_options[["graphic_themes"]][[1]]))
    }

    invisible(.qol_options[["graphic_themes"]][[theme_name]])
}


#' @description
#' [get_theme_base_colors()]: Retrieve a vector of hex colors from the globally set up themes
#' containing the base colors for the segments.
#'
#' @param theme_name The theme name to look up.
#'
#' @return
#' [get_theme_base_colors()]: A vector of hex color codes.
#'
#' @examples
#' get_theme_base_colors("ocean")
#'
#' @rdname graphic_themes
#'
#' @export
get_theme_base_colors <- function(theme_name){
    if (length(theme_name) > 1){
        message(" ! WARNING: Only a single theme can be retrieved. First vector element will be used.")

        theme_name <- theme_name[[1]]
    }

    if (!theme_name %in% names(.qol_options[["graphic_themes"]])){
        message(" X ERROR: Theme '", theme_name, "' doesn't exist. Default theme will be used.")
        return(invisible(.qol_options[["graphic_themes"]][[1]][["base"]]))
    }

    invisible(.qol_options[["graphic_themes"]][[theme_name]][["base"]])
}


#' @description
#' [get_theme_font_colors()]: Retrieve a vector of hex colors from the globally set up themes
#' containing the individual font colors corresponding to the base segment colors.
#'
#' @param theme_name The theme name to look up.
#'
#' @return
#' [get_theme_font_colors()]: A vector of hex color codes.
#'
#' @examples
#' get_theme_font_colors("ocean")
#'
#' @rdname graphic_themes
#'
#' @export
get_theme_font_colors <- function(theme_name){
    if (length(theme_name) > 1){
        message(" ! WARNING: Only a single theme can be retrieved. First vector element will be used.")

        theme_name <- theme_name[[1]][["font"]]
    }

    if (!theme_name %in% names(.qol_options[["graphic_themes"]])){
        message(" X ERROR: Theme '", theme_name, "' doesn't exist. Default theme will be used.")
        return(invisible(.qol_options[["graphic_themes"]][[1]][["font"]]))
    }

    invisible(.qol_options[["graphic_themes"]][[theme_name]][["font"]])
}


#' @description
#' [reset_color_themes()]: Resets the global color themes back to default.
#'
#' @param clear_themes FALSE by default. If TRUE clears the global theme list.
#'
#' @return
#' [reset_color_themes()]: Default global color list or empty list.
#'
#' @examples
#' # Clear all globally stored themes in case only self created themes should be stored
#' reset_color_themes(clear_themes = TRUE)
#'
#' # Reset global themes to default
#' reset_color_themes()
#'
#' @rdname graphic_themes
#'
#' @export
reset_color_themes <- function(clear_themes = FALSE){
    if (clear_themes){
        .qol_options[["graphic_themes"]] <- list()
    }
    else{
        .qol_options[["graphic_themes"]] <- create_global_themes()
    }

    invisible(.qol_options[["graphic_themes"]])
}


#' @description
#' [display_colors()]: Displays all colors of a theme with the corresponding hex codes.
#'
#' @param theme_name The name of a globally stored theme.
#'
#' @return
#' [display_colors()]: Returns a list of base and font colors.
#'
#' @examples
#' # Displaying colors and themes
#' display_colors("ocean")
#'
#' @rdname graphic_themes
#'
#' @export
display_colors <- function(theme_name){
    if (length(theme_name) > 1 || !is.character(theme_name)){
        message(" X ERROR: Only a single theme can be displayed. Use 'display_themes()' to display all\n",
                "          currently stored themes with their respective colors.")
        return(invisible(NULL))
    }

    base_colors <- get_theme_base_colors(theme_name)
    font_colors <- get_theme_font_colors(theme_name)

    # Set up a new graphic
    grid::grid.newpage()

    # To evenly draw rectangles side by side, the space each rectangle takes has to be
    # calculated and at what position it has to be drawn. Do do that, each position is
    # divided by the total number of colors. This basically gives the positions of the
    # right edges of the rectangles. Since the draw function draws rectangles from the
    # center, the positions have to be reduced by a half, to get the center positions.
    number_of_colors <- length(base_colors)
    rect_centers     <- (seq_len(number_of_colors) - 0.5) / number_of_colors

    # Draw rectangles side by side
    grid::grid.rect(x      = rect_centers,
                    y      = 0.5,
                    width  = 1 / number_of_colors,
                    height = 0.6,
                    gp     = grid::gpar(fill = base_colors, col = NA))

    # Draw hex codes in rectangles
    grid::grid.text(base_colors,
                    x   = rect_centers,
                    y   = 0.5,
                    rot = 90,
                    gp  = grid::gpar(col = font_colors))

    invisible(list(base = base_colors,
                   font = font_colors))
}


#' @description
#' [display_themes()]: Display all colors of all themes currently present in the
#' global environment.
#'
#' @return
#' [display_themes()]: A list containing all globally stored themes with their color
#' codew vectors.
#'
#' @examples
#' display_themes()
#'
#' @rdname graphic_themes
#'
#' @export
display_themes <- function(){
    global_themes    <- .qol_options[["graphic_themes"]]
    number_of_themes <- length(global_themes)

    if (number_of_themes == 0){
        message("~ NOTE: No themes stored. Add themes by calling 'set_color_theme()' or use\n",
                "        'reset_color_theme()' to get back default themes.")
        return(invisible(global_themes))
    }

    # Set up a new graphic
    grid::grid.newpage()

    # Set up a viewport with a basic structural layout which contains a label column
    # for the theme names and a column for the color ramps. Theme names receive a smaller
    # space. Afterwards add the viewport to the stack to work within it.
    label_width <- 0.2

    grid_layout <- grid::grid.layout(nrow   = number_of_themes,
                                     ncol   = 2,
                                     widths = grid::unit.c(grid::unit(label_width, "npc"),
                                                           grid::unit(1 - label_width, "npc")))

    grid::viewport(layout = grid_layout) |> grid::pushViewport()

    # Draw all theme names in the first column and spread them evenly among the rows
    grid::grid.text(label = names(global_themes),
                    x     = 0.95,
                    y     = grid::unit(seq(number_of_themes, 1) - 0.5, "native"),
                    just  = "right",
                    vp    = grid::viewport(layout.pos.col = 1, yscale = c(0, number_of_themes)))

    # Draw the individual color ramps one after another and evenly spread the colors based
    # on the individual number of colors in a theme.
    for (i in seq_len(number_of_themes)){
        base_colors      <- global_themes[[i]][["base"]]
        font_colors      <- global_themes[[i]][["font"]]
        number_of_colors <- length(base_colors)
        rect_centers     <- (seq_len(number_of_colors) - 0.5) / number_of_colors

        # Draw rectangles with base colors
        grid::grid.rect(x = rect_centers,
                        width  = 1 / number_of_colors,
                        height = 0.8,
                        gp     = grid::gpar(fill = base_colors, col = NA),
                        vp     = grid::viewport(layout.pos.row = i, layout.pos.col = 2))

        # Draw numbers in rectangles
        grid::grid.text(1:number_of_colors,
                        x   = rect_centers,
                        y   = 0.5,
                        gp  = grid::gpar(col = font_colors),
                        vp  = grid::viewport(layout.pos.row = i, layout.pos.col = 2))
    }

    # Remove viewport from the stack
    grid::popViewport()

    invisible(global_themes)
}
