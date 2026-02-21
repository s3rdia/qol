default_themes <- reset_color_themes()


test_that("Adding color themes and resetting", {
    set_color_theme(list(tropic = c(
                              "#1B3A2E", "#1F5A3F", "#257F54", "#2FA36A", "#56C07E",
                              "#85D49A", "#B2E4B8", "#D7F0D5", "#ECF8EB", "#F7FCF7"),

                          rosewood = c(
                              "#3A1E28", "#542334", "#6F2D40", "#8C4256", "#AA6A78",
                              "#C28E9B", "#D7B1BB", "#E7D0D6", "#F3E7EB", "#FAF3F5")))

    current_themes <- display_themes()

    expect_true(!any(c("tropic", "rosewood") %in% names(default_themes)))
    expect_true(all(c("tropic", "rosewood") %in% names(current_themes)))

    reset_color_themes()

    current_themes <- display_themes()

    expect_equal(current_themes, default_themes)

    reset_color_themes(clear_themes = TRUE)

    expect_message(current_themes <- display_themes(),
                   "~ NOTE: No themes stored. Add themes by calling")

    expect_equal(length(current_themes), 0)

    reset_color_themes()
})


test_that("Get theme colors", {
    theme_colors <- get_theme_colors("ocean")

    expect_equal(length(theme_colors), 10)
})


test_that("Display theme colors", {
    theme_colors <- get_theme_colors("ocean")

    expect_equal(theme_colors, display_colors(theme_colors))
})

###############################################################################
# Warning checks
###############################################################################

test_that("Theme name doesn't exist when getting theme colors", {
    expect_message(theme_colors <- get_theme_colors(c("ocean", "forest")),
                   " ! WARNING: Only a single theme can be retrieved. First vector element will be used.")

    expect_equal(length(theme_colors), 10)
})

###############################################################################
# Abort checks
###############################################################################

test_that("Abort if empty list provided for color themes", {
    expect_message(set_color_theme(list()),
                   " X ERROR: Empty list found. Color theme won't be added.")
})


test_that("Abort if something other than named list provided for color themes", {
    expect_message(set_color_theme(list(1, 2, 3)),
                   " X ERROR: Colors must be provided as a named list. Color theme won't be added.")
})


test_that("Abort if theme name doesn't exist when getting theme colors", {
    expect_message(get_theme_colors("test"),
                   " X ERROR: Theme 'test' doesn't exist. Default theme will be used.")
})


test_that("Abort display_colors if no color vector provided", {
    expect_message(display_colors(1),
                   " X ERROR: Only a single theme can be displayed. Use")
})


test_that("Abort display_colors if invalid hex code found", {
    expect_message(display_colors(c("#fffffz")),
                   " X ERROR: '#fffffz' must be a 6 character <hex code>. Color theme can't be displayed.")
})
