# qol 1.1.0 - DEVELOPMENT

### New functions
* `build_master()`: Reads a given folder structure, which contains scripts, and builds a master script as a markdown file. (18.10.2025)
* `build_rstheme()`: Build a complete theme file, which can be used to change the visual appearance of RStudio. (23.10.2025)
* `combine_into_workbook()`: Combines any number of tables created with `any_table()` into one workbook and styles them according to their meta information. (26.10.2025)

### New functionality
* `interval_format()`: Implemented keywords "low "and "high" with which one can define pseudo low or high values, if one doesn't know, what the minimum or maximum value of a variable is. (20.10.2025)
* `discrete_format()`: Implemented keyword "other" with which one can catch any other value not covered by the explicitly specified values. (27.10.2025)
* `any_table()`: Now returns styling meta information as a third list element. This meta information can be used by `combine_into_workbook()`. (26.10.2025)

### Fixed
* In `any_table()` the header and table row heights as well as the column widths set by the style option where 1 row/column to short. (14.10.2025)
* In `any_table()` the row heights didn't catch the whole table. (14.10.2025)
* `any_table()` ran into an error, if a variable was provided as pct_group, which was not part of the row and column variables. (14.10.2025)
* Fixed typos in frequencies examples, where it said "frequency" instead of "frequencies". (18.10.2025)
* In Excel outputs the number stored as text error is now ignored. (19.10.2025, thanks to @JanMarvin)
* Fixed table length captured with too many rows. (19.10.2025)

### Changed functionality
* In `excel_output_style()` the options `column_widths` and `row_heights` now start at the first column/row instead of the beginning of the table. (19.10.2025)

### Additionally
* Added missing functions `export_with_style()` and `get_excel_range()` to the ?qol overview page. (14.10.2025)
* Added information to the startup message to use ?qol to get an overview. (14.10.2025)
* Now using openxlsx2 helper to convert row and column numbers to Excel ranges. (19.10.2025, thanks to @JanMarvin)
* All Excel tables now have named ranges for the table and the values. (19.10.2025, thanks to @JanMarvin)
* Added an example to the README showing how to save an Excel workbook to the filesystem. (19.10.2025)
* Used lintr package for some code cleanup. (20.10.2025, thanks to @JanMarvin for the advise)
* Added custom theme section to the README. (23.10.2025)
* Added message, if a format is applied to a factor variable. (27.10.2025)

### GitHub related
* Added article comparing this package with SAS. (14.10.2025)
* Changed GitHub Page style. (14.10.2025)
* Added example themes with corresponding code. (23.10.2025)


# qol 1.0.2 

### DESCRIPTION file
* Fixed brackets in DESCRIPTION so that auto linking works.
* Added another URL.

### Fixed
* There could be an error in `summarise_plus()`, `any_table()` and `args_to_char()`, if there where to many variables provided.
* `summarise_plus()` ran into an error, if a value label from a format contained a ".".
* `any_table()` could lead to wrong results if the pre_summed option was used and a variable was part of multiple combinations in the summarised data.

### Changed functionality
* In `summarise_plus()`, when types are defined, the total row is now removed if not explicitly defined as type.
* In `summarise_plus()`, when the nesting option "deepest" is used, the variables TYPE, TYPE_NR and DEPTHS are now also generated.

### Unit tests
* Adjusted tests according to changed functionalities

### Additionally
* Added openxlsx2 as import in the qol main help file
* Corrected a typo in a warning message in `any_table()` concerning variable order.


# qol 1.0.1 

* Added references to specific SAS functions in the description field of the Description file where they are mentioned.
* Removed specific seed in R/dummy_data.R
* Fixed a bug in dummy_data where it could happen that not enough observations where generated.


# qol 1.0.0 

* Initial CRAN submission.
