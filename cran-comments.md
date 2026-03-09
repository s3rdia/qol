# Resubmission qol 1.2.2
Last CRAN release was on 11.02.2026.

### New functions

* `round_values()`: Rounds values according to the round half up rule.

### New functionality

* `any_table()`: Added `row_pct` and `col_pct` keywords to `pct_group` parameter. With this the function can calculate total percentages for rows and columns regardless of the respective other dimension.
* `rename_multi()`: Variable names can now be passed without quotation marks.
* `any_table()`: Percentages based on single formatted variable expressions can now be computed with the `pct_value` parameter.
* `any_table()`: If the `pct_value or `pct_group` parameters are used but are not part of the statistics parameter, they will be automatically added to statistics.
* `dummy_data()`: Reworked the dummy data generation in mutliple ways:
	* Added new variables: NUTS2, NUTS3, number_of_persons, body_height, body_weight, income_class, expenses, balance.
	* Now generates up to five years.
	* Variables are now sorted by year, state, household_id, person_id.
	* household_id is now numbered individually within each state.
	* Weights now adapt to the number of observations so that weighted results stay roughly the same.
	* Brought variety in most of the variables so that different distributions aren't evenly spread anymore.
	* Adding NA values is now optional.

### Changed functionality

* `frequencies()`, `crosstabs()`, `any_table()`, `content_report()`: Now uses round half up rule for rounding values.

### Fixed

* `any_table()`: When there was an NA value in the first table column while the row label column was deleted, the NA symbol wasn't set. This is fixed now.
* `any_table()`: If the custom NA symbol is set to a number, Excels number stored as text error is now ignored.
* `transpose_plus()`: Took out a debug print I forgot in the function.
* `summarise_plus()`: Fixed formats not matching data when computing percentages, if numeric values are stored as character.
* `if.()`, `else_if.()`, `else.()`: When passing a vector to a new variable, the functions don't error any more if there are NA values.
* `summarise_plus()`: Fixed "." in a variable expression is now preserved as intended.

### Optimization

* `dummy_data()`: Got rid of the loop for generating multiple years at random. Now the dummy data is based on a smaller set of observations, which allows faster variable generation. Years are only added as cartesian product at the end and values altered per year. This allowed the rework mentioned above: More variables and more variety in the same amount of time as before.

### New Error Checks

* `any_table()`: Added an error check in case all caLculations are done and no value variable was computed.

### Additionally

* `frequencies()`: Fixed two of the examples where `list` was missing in formats parameter.


## R CMD check results

0 errors | 0 warnings | 0 note
