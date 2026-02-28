default_themes <- reset_color_themes()


test_that("Adding color themes and resetting", {
    add_color_theme("tropic",
                    c("#1B3A2E", "#1F5A3F", "#257F54", "#2FA36A", "#56C07E",
                      "#85D49A", "#B2E4B8", "#D7F0D5", "#ECF8EB", "#F7FCF7"))

    current_themes <- display_themes()

    expect_true(!"tropic" %in% names(default_themes))
    expect_true("tropic"  %in% names(current_themes))

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
    theme_colors <- get_theme_base_colors("ocean")

    expect_equal(length(theme_colors), 10)
})


test_that("Display theme colors", {
    theme <- display_colors("ocean")

    expect_true(all(c("base", "font_inside", "font_outside") %in% names(theme)))
})

###############################################################################
# Warning checks
###############################################################################

test_that("Theme name doesn't exist when getting theme colors", {
    expect_message(theme_colors <- get_theme_base_colors(c("ocean", "violet_fire")),
                   " ! WARNING: Only a single theme can be retrieved. First vector element will be used.")

    expect_equal(length(theme_colors), 10)
})

###############################################################################
# Abort checks
###############################################################################

test_that("Abort if empty list provided for color themes", {
    expect_message(add_color_theme("test",
                                   list()),
                   " X ERROR: No base colors provided. Color theme won't be added.")
})


test_that("Abort if something other than named list provided for color themes", {
    expect_message(add_color_theme("test",
                                   list(1, 2, 3)),
                   " X ERROR: Base color '")
})


test_that("Abort if theme name doesn't exist when getting theme colors", {
    expect_message(get_theme_colors("test"),
                   " X ERROR: Theme 'test' doesn't exist. Default theme will be used.")
})


test_that("Abort display_colors if no color vector provided", {
    expect_message(display_colors(1),
                   " X ERROR: Only a single theme can be displayed. Use")
})
