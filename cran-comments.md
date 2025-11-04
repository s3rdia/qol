# Resubmission qol 1.1.0
Last CRAN release was on 14.10.2025. This release contains some additions to further enhance the package and some bug fixes:

New functions
* build_master(): Reads a given folder structure, which contains scripts, and builds a master script as a markdown file.
* build_rstheme(): Build a complete theme file, which can be used to change the visual appearance of RStudio.
* combine_into_workbook(): Combines any number of tables created with `any_table()` into one workbook and styles them according to their meta information.
* replace_except(): Replaces a provided pattern with another, while protecting exceptions.
* mark_case(): Marks first or last cases within a provided group.
* retain_value(): Retains the first value for all cases of the same group.
* retain_sum(): Retains the summarised values for all cases of the same group.

New functionality
* interval_format(): Implemented keywords "low "and "high" with which one can define pseudo low or high values, if one doesn't know, what the minimum or maximum value of a variable is.
* discrete_format(): Implemented keyword "other" with which one can catch any other value not covered by the explicitly specified values.
* any_table(): Now returns styling meta information as a third list element. This meta information can be used by combine_into_workbook().
* any_table(): Now supports underscores in variable names.
* any_table(): Added new column ordering possibilities by "columns" or "values_stats".

Fixed
* In any_table() the header and table row heights as well as the column widths set by the style option where 1 row/column to short.
* In any_table() the row heights didn't catch the whole table.
* any_table() ran into an error, if a variable was provided as pct_group, which was not part of the row and column variables.
* Fixed typos in frequencies examples, where it said "frequency" instead of "frequencies".
* In Excel outputs the number stored as text error is now ignored.
* Fixed table length captured with too many rows.
* any_table(): Fixed order_by stats not working as intended in some cases.
* any_table(): Fixed table rows not ordered correctly in some cases.
* any_table(): Depending on variable constellation and ordering of the column header it could happen, that the header wasn't merged correct in the Excel workbook. This was fixed. 

Changed functionality
* In excel_output_style() the options column_widths and row_heights now start at the first column/row instead of the beginning of the table.
* In summarise_plus(), if summarised values should be merged back, the variables TYPE, TYPE_NR and DEPTHS are now not merged back anymore.

Additionally
* Added missing functions export_with_style() and get_excel_range() to the ?qol overview page.
* Added information to the startup message to use ?qol to get an overview.
* Now using openxlsx2 helper to convert row and column numbers to Excel ranges.
* All Excel tables now have named ranges for the table and the values.
* Added an example to the README showing how to save an Excel workbook to the filesystem.
* Used lintr package for some code cleanup.
* Added custom theme section to the README.
* Added message, if a format is applied to a factor variable.
* Corrected typos in snippets.

## R CMD check results

0 errors | 0 warnings | 0 note
