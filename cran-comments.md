# Resubmission qol 1.2.1
Last CRAN release was on 13.01.2026.

New functions

* expand_formats(): Generates a data frame which contains all nested combinations of the provided format labels.
* Global style options:
	* set_print_miss(), get_print_miss(): Additional global setters and getters.

New functionality

* summarise_plus(), any_table(), frequencies(), crosstabs(): The new parameter print_miss outputs all possible categories of the grouping variables based on the provided formats, even if there are no observations for a combination.
* retain_variables(): The ":" can now be used as a placeholder for "starts with" ("text:"), "ends with" (":text") and "contains" (":text:").

## R CMD check results

0 errors | 0 warnings | 0 note
