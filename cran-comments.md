# Resubmission qol 1.2.1
Last CRAN release was on 13.01.2026.

New functions

* expand_formats(): Generates a data frame which contains all nested combinations of the provided format labels.
* Global style options:
	* set_print_miss(), get_print_miss(): Additional global setters and getters.

New functionality

* summarise_plus(), any_table(), frequencies(), crosstabs(): The new parameter print_miss outputs all possible categories of the grouping variables based on the provided formats, even if there are no observations for a combination.
* retain_variables(): The ":" can now be used as a placeholder for "starts with" ("text:"), "ends with" (":text") and "contains" (":text:").
* frequencies(): Until now the function always printed a means and a frequencies table as default. Now it only prints a frequencies table as default to get the main results as fast as possible on screen. The means table can be activated again with the new means parameter.
* frequencies(): When using multiple by variables with excel output, worksheets are now ordered in provided variable order instead of alternating.

Fixed

* frequencies(): Mean tables are now printed when by variables are provided.
* frequencies(): No empty "total" table is printed when by variables are provided.
* frequencies(): "total" row is not printed anymore, when variables use multilabels and are computed with a by group.

Optimization

* summarise_plus(): When only statistics based on sums are selected, the function already pre summarises the data frame, to apply the formats on a much smaller data frame. When using nesting = "all"/"single" the data frame is now pre summarised a second time before applying formats, this time only using the grouping variables of the combination beeing processed. This drastically cuts down memory allocation - especially for larger data frames - and speeds up every iteration significantly. In addition to this function any_table() benefits greatly from this optimization.
* any_table(), frequencies(), crosstabs(): When using by variables in excel output, the new print_miss option enables a shortcut in formatting the sheets after the first one. Since the option guarantees that all follow up sheets are printed with the exact same table width and height, because all categories are printed, only the first sheet must be formatted. All other sheets can clone the entire style from the first sheet.

New Error Checks

* combine_into_workbook(): Added an error check in case a provided object is no any_table() result list. (17.01.2026)

## R CMD check results

0 errors | 0 warnings | 0 note
