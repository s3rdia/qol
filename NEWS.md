# qol 1.0.3 - DEVELOPMENT

### Fixed
* In `any_table()` the header and table row heights as well as the column widths set by the style option where 1 row/column to short. (14.10.2025)
* In `any_table()` the row heights didn't catch the whole table. (14.10.2025)
* `any_table()` ran into an error, if a variable was provided as pct_group, which was not part of the row and column variables. (14.10.2025)

### Additionally
* Added missing functions `export_with_style()` and `get_excel_range()` to the ?qol overview page. (14.10.2025)
* Added information to the startup message to use ?qol to get an overview. (14.10.2025)

### GitHub related
* Added article comparing this package with SAS. (14.10.2025)
* Changed GitHub Page style. (14.10.2025)


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
