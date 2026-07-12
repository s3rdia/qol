set_no_print(TRUE)

default_themes <- reset_color_themes()


# Get theme colors
theme_colors <- get_theme_colors("ocean")

expect_true(inherits(theme_colors, "qol_color_theme"), info = "Get theme colors")


# Adding color themes and resetting
add_color_theme("rainbow", rainbow(10))

current_themes <- display_themes()

expect_true(!"rainbow" %in% names(default_themes), info = "Adding color themes and resetting")
expect_true("rainbow" %in% names(current_themes), info = "Adding color themes and resetting")

reset_color_themes()

current_themes <- display_themes()

expect_equal(current_themes, default_themes, info = "Adding color themes and resetting")

reset_color_themes(clear_themes = TRUE)

current_themes <- display_themes()

expect_message(print_stack_as_messages("NOTE"), "No themes stored. Add themes by calling", info = "Adding color themes and resetting")
expect_equal(length(current_themes), 0, info = "Adding color themes and resetting")

reset_color_themes()

###############################################################################
# Warning checks
###############################################################################

# Theme name doesn't exist when getting theme colors
theme_colors <- get_theme_colors(c("ocean", "forest"))

expect_true(inherits(theme_colors, "qol_color_theme"), info = "Get theme colors")
expect_warning(print_stack_as_messages("WARNING"), "Only a single theme can be retrieved. First vector element will be used.")

###############################################################################
# Abort checks
###############################################################################

# Abort if empty list provided for color themes
add_color_theme("test", list())

expect_error(print_stack_as_messages("ERROR"), "No base colors provided. Color theme won't be added.",
             info = "Abort if empty list provided for color themes")


# Abort if something other than named list provided for color themes
add_color_theme("test", list(1, 2, 3))

expect_error(print_stack_as_messages("ERROR"), "Base color '1' must be a 6 character <hex code>. Color theme won't be added.",
             info = "Abort if something other than named list provided for color themes")


# Abort if theme name doesn't exist when getting theme colors
get_theme_colors("test")

expect_error(print_stack_as_messages("ERROR"), "Theme 'test' doesn't exist. Default theme will be used.",
             info = "Abort if theme name doesn't exist when getting theme colors")


# Abort display_colors if no color vector provided
display_colors(1)

expect_error(print_stack_as_messages("ERROR"), "Only a single theme can be displayed. Use",
             info = "Abort display_colors if no color vector provided")


# Abort display_colors if invalid hex code found
display_colors("#fffffz")

expect_error(print_stack_as_messages("ERROR"), "Theme '#fffffz' doesn't exist. Default theme will be used.",
             info = "Abort display_colors if invalid hex code found")


set_no_print()
