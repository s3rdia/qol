# Resubmission dd.mm.yyyy - qol 1.0.3

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


# Resubmission 13.10.2025 - qol 1.0.2

This is a resubmission which fixes some problems I came across while further testing the package.

DESCRIPTION file
* Fixed brackets in DESCRIPTION so that auto linking works.
* Added another URL.

Fixed
* There could be an error in summarise_plus(), any_table() and args_to_char(), if there where to many variables provided.
* summarise_plus() ran into an error, if a value label from a format contained a ".".
* any_table() could lead to wrong results if the pre_summed option was used and a variable was part of multiple combinations in the summarised data.

Changed functionality
* In summarise_plus(), when types are defined, the total row is now removed if not explicitly defined as type.
* In summarise_plus(), when the nesting option "deepest" is used, the variables TYPE, TYPE_NR and DEPTHS are now also generated.

Unit tests
* Adjusted tests according to changed functionalities

Additionally
* Added openxlsx2 as import in the qol main help file
* Corrected a typo in a warning message in any_table() concerning variable order.


# Resubmission 01.10.2025 - qol 1.0.1

This is a resubmission based on the e-mail feedback from October 1st 2025:
* Added references to specific SAS functions in the description field of the Description file where they are mentioned.
* Removed specific seed in R/dummy_data.R

Additionally:
* Fixed a bug in dummy_data where it could happen that not enough observations where generated.
  

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
