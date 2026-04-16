# Print content report
dummy_df    <- dummy_data()
result_list <- content_report(dummy_df)

expect_equal(names(result_list), c("global", "variables"), info = "Print content report")


# Print tinytest custom report
result_file <- system.file("extdata", "qol_tinytest_results.fst", package = "qol")
results     <- load_file(dirname(result_file), basename(result_file))

output <- results |> report_test_results()

expect_equal(results, output, info = "Print content report")
