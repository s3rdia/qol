# Print Styled Tinytest Results

Styles the results of tinytest to get a better visual overview.

`test_package()`: Runs all set up unit tests with tinytest and outputs
the results with the custom reporter.

`test_single_file()`: Runs a single unit test file with tinytest.

## Usage

``` r
report_test_results(tiny_results, utf8 = .qol_messages[["format"]][["utf8"]])

test_package(package_name, multithread = FALSE)

test_single_file(file_name)
```

## Arguments

- tiny_results:

  The results produced by tinytest.

- utf8:

  Whether to display complex characters or just plain text.

- package_name:

  Name of the package to test.

- multithread:

  FALSE by default. Whether to run tests multithreaded. NOTE: To make
  this work you have to manually run
  [`devtools::install()`](https://devtools.r-lib.org/reference/install.html)
  first.

- file_name:

  Name of the file to test.

## Value

Returns the results.

## Examples

``` r
# Example results
result_file <- system.file("extdata", "qol_tinytest_results.fst", package = "qol")
results     <- load_file(dirname(result_file), basename(result_file))

# Display results
results |> report_test_results()

# Normally you would do this:
# tinytest::test_all("PATH TO PACKAGE") |> report_test_results()
# or
# tinytest::test_package("PACKAGE NAME", testdir = "inst/tinytest") |> report_test_results()

# To test the whole package with the custom reporter use:
# test_package("qol")
```
