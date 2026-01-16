# Resubmission qol 1.2.1
Last CRAN release was on 13.01.2026.

New functions

* expand_formats(): Generates a data frame which contains all nested combinations of the provided format labels.
* Global style options:
	* set_print_miss(), get_print_miss(): Additional global setters and getters.

New functionality

* summarise_plus(), any_table(), frequencies(), crosstabs(): The new parameter print_miss outputs all possible categories of the grouping variables based on the provided formats, even if there are no observations for a combination.
* retain_variables(): The ":" can now be used as a placeholder for "starts with" ("text:"), "ends with" (":text") and "contains" (":text:").

Optimization

* summarise_plus(): When only statistics based on sums are selected, the function already pre summarises the data frame, to apply the formats on a much smaller data frame. When using nesting = "all"/"single" the data frame is now pre summarised a second time before applying formats, this time only using the grouping variables of the combination beeing processed. This drastically cuts down memory allocation - especially for larger data frames - and speeds up every iteration significantly. In addition to this function `any_table()` benefits greatly from this optimization.

## R CMD check results

0 errors | 0 warnings | 0 note
