test_that("handling factors works", {
    island. <- discrete_format(
        "Total"  = c(1:2, 3),
        "Bsc" = 1,
        "Drm" = 2,
        "Other"  = 3
    )

    sex. <- discrete_format(
        "Total"  = c("male", "female"),
        "M"   = "male",
        "F" = "female"
    )

    expect_warning(
        any_table(
            data_frame = penguins,
            rows       = c("sex + island", "sex", "island"),
            columns    = c("year", "species + year"),
            values     = body_mass,
            statistics = c("sum", "pct_group"),
            pct_group  = c("sex", "island"),
            formats    = list(
                sex = sex.,
                island = island.
            ),
            na.rm      = TRUE
        ),
        "Converting factor columns to base types"
    )
})
