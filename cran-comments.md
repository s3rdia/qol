# Resubmission qol 1.0.3
Last CRAN release was on 14.10.2025. This release contains some additions and bug fixes:

New functions
* build_master(): Reads a given folder structure, which contains scripts, and builds a master script as a markdown file.
* build_rstheme(): Build a complete theme file, which can be used to change the visual appearance of RStudio.

New functionality
* interval_format(): Implemented keywords "low "and "high" with which one can define pseudo low or high values, if one doesn't know, what the minimum or maximum value of a variable is. 

Fixed
* In any_table() the header and table row heights as well as the column widths set by the style option where 1 row/column to short.
* In any_table() the row heights didn't catch the whole table.
* any_table() ran into an error, if a variable was provided as pct_group, which was not part of the row and column variables.
* Fixed typos in frequencies examples, where it said "frequency" instead of "frequencies".
* In Excel outputs the number stored as text error is now ignored.
* Fixed table length captured with too many rows.

Changed functionality
* In excel_output_style() the options column_widths and row_heights now start at the first column/row instead of the beginning of the table.

Additionally
* Added missing functions export_with_style() and get_excel_range() to the ?qol overview page.
* Added information to the startup message to use ?qol to get an overview.
* Now using openxlsx2 helper to convert row and column numbers to Excel ranges.
* All Excel tables now have named ranges for the table and the values.
* Added an example to the README showing how to save an Excel workbook to the filesystem.
* Used lintr package for some code cleanup.
* Added custom theme section to the README.

## R CMD check results

0 errors | 0 warnings | 0 note
