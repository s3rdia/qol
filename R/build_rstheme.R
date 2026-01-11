#' Build a Theme From Scratch
#'
#' @description
#' Build your own theme by just setting up the colors for the different parts of
#' RStudio. A theme file will be exported which can be added by going to:
#'
#' Tools -> Global Options -> Appearance -> Add
#'
#' @param file_path The path to which the theme file should be saved.
#' @param theme_name The themes name.
#' @param dark_theme Handles some elements not covered with the other parameters.
#' @param editor_background Base background color in the editor.
#' @param editor_headline Mostly used for the headlines of the environment panel.
#' @param editor_font Base font color of the editor.
#' @param toolbar Base toolbar and frame color.
#' @param tab Color of inactive tabs.
#' @param selected_tab Color of active tabs.
#' @param line_number The color of the line numbers on the left.
#' @param print_margin Color of the vertical line showing the print margin.
#' @param cursor Cursor color.
#' @param selection The background color of the current selection.
#' @param smart_highlight Background color of smart highlighted words.
#' @param bracket_highlight Background color of highlighted bracket pairs.
#' @param active_line Color for the active line the cursor is in.
#' @param whitespace Color for whitespace characters.
#' @param debug_line Color of the current debug line.
#' @param scrollbar Color of the scrollbars.
#' @param scrollbar_hover Highlight color when hovering over a scrollbar.
#' @param scrollbar_active Highlight color when clicking on a scrollbar.
#' @param class_name Code color for class names (like package names).
#' @param keyword Code color for fixed keywords (like function, if, else).
#' @param language_constant Code color for language constants (like the @ keywords).
#' @param function_name Code color for base and package functions.
#' @param numeric Code color for numeric values.
#' @param string Code color for string values.
#' @param regex Code color for regex expressions.
#' @param variable Code color for variables, parameters and arguments.
#' @param comment Code color for comments.
#' @param symbol Code Color of symbols (like <-, brackets).
#' @param console_code Color of executed Code in the Console.
#' @param markdown_code Background color of code passages in a markdown file.
#'
#' @details
#' In the 'SAS Enterprise Guide' the user is able to not only choose a given theme, but
#' to also pick the colors for the different parts of the editor by themselves. Everyone
#' has a different taste of what colors look pleasing to the eyes, so you should be able
#' to choose them by yourself.
#'
#' @return
#' Saves a complete theme file.
#'
#' @examples
#' # Example export file paths
#' # NOTE: These tempfiles are only for the examples. In reality you just call the
#' # main function and put in your desired path and name directly.
#' temp_file <- tempfile(fileext = ".rstheme")
#' file_name <- basename(tools::file_path_sans_ext(temp_file))
#'
#' # Example theme
#' build_rstheme(file_path         = dirname(temp_file),
#'               theme_name        = file_name,
#'               editor_background = "#417291",
#'               editor_headline   = "#602BCA",
#'               editor_font       = "#C75C48")
#'
#' # Manual cleanup for example
#' unlink(temp_file)
#'
#' @export
build_rstheme <- function(file_path,
                          theme_name        = "qol_green",
                          dark_theme        = TRUE,
                          editor_background = "#062625",
                          editor_headline   = "#3B3B3B",
                          editor_font       = "#C3B79D",
                          toolbar           = "#2E2E2E",
                          tab               = "#3B3B3B",
                          selected_tab      = "#062625",
                          line_number       = "#C3B79D",
                          print_margin      = "#3B3B3B",
                          cursor            = "#CCCCCC",
                          selection         = "#1B436E",
                          smart_highlight   = "#3686dc",
                          bracket_highlight = "#595959",
                          active_line       = "#202324",
                          whitespace        = "#CCCCCC",
                          debug_line        = "#F18889",
                          scrollbar         = "#3B3B3B",
                          scrollbar_hover   = "#595959",
                          scrollbar_active  = "#BFBFBF",
                          class_name        = "#BEDD1A",
                          keyword           = "#FFC90E",
                          language_constant = "#FFC90E",
                          function_name     = "#C3B79D",
                          numeric           = "#C93F3F",
                          string            = "#63C2C9",
                          regex             = "#E8E6E3",
                          variable          = "#E8E6E3",
                          comment           = "#32CD32",
                          symbol            = "#C3B79D",
                          console_code      = "#C3B79D",
                          markdown_code     = "#083332"){
    # Measure the time
    start_time <- Sys.time()

    # Check if folder exists; ... is for testing
    if (!dir.exists(file_path) || dirname(file_path) == "."){
        message(" X ERROR: Directory '", file_path, "' does not exist.")
        return(invisible(NULL))
    }

    # Change main body to light theme if specified
    if (dark_theme == FALSE){
        rstheme_body <- gsub(".rstudio-themes-dark", ".rstudio-themes-flat", rstheme_body)
        rstheme_body <- gsub(".rstudio-themes-flat > .rstudio-themes-flat",
                             ".rstudio-themes-flat > .rstudio-themes-default", rstheme_body)
    }

    # Put together the complete theme file
    rstheme <- c(paste0("/* rs-theme-name: ", theme_name, " */"),
                 paste0("/* rs-theme-is-dark: ", dark_theme, " */"),
                 ":root {",
                 "  /* editor */",
                 paste0("  --editor-background: ", editor_background, ";"),
                 paste0("  --editor-headline: ", editor_headline, ";"),
                 paste0("  --editor-font: ", editor_font, ";"),
                 "",
                 paste0("  --toolbar: ", toolbar, ";"),
                 paste0("  --tab: ", tab, ";"),
                 paste0("  --selected-tab: ", selected_tab, ";"),
                 "",
                 paste0("  --line-number: ", line_number, ";"),
                 paste0("  --print_margin: ", print_margin, ";"),
                 "",
                 paste0("  --cursor: ", cursor, ";"),
                 paste0("  --selection: ", selection, ";"),
                 paste0("  --smart-highlight: ", smart_highlight, ";"),
                 paste0("  --bracket-highlight: ", bracket_highlight, ";"),
                 paste0("  --active-line: ", active_line, ";"),
                 paste0("  --whitespace: ", whitespace, ";"),
                 paste0("  --debug-line: ", debug_line, ";"),
                 "",
                 paste0("  --scrollbar: ", scrollbar, ";"),
                 paste0("  --scrollbar-hover: ", scrollbar_hover, ";"),
                 paste0("  --scrollbar-active: ", scrollbar_active, ";"),
                 "",
                 "  /* syntax */",
                 paste0("  --class-name: ", class_name, ";"),
                 paste0("  --keyword: ", keyword, ";"),
                 paste0("  --language-constant: ", language_constant, ";"),
                 paste0("  --function-declaration: ", function_name, ";"),
                 paste0("  --numeric-constant: ", numeric, ";"),
                 paste0("  --string: ", string, ";"),
                 paste0("  --regex: ", regex, ";"),
                 paste0("  --variable: ", variable, ";"),
                 paste0("  --comment: ", comment, ";"),
                 paste0("  --symbols: ", symbol, ";"),
                 "",
                 "  /* console */",
                 paste0("  --console-code: ", console_code, ";"),
                 "",
                 "  /* markdown */",
                 paste0("  --code-line: ", markdown_code, ";"),
                 "",
                 "  /* ??? */",
                 paste0("  --invalid-font: #ced2cf;"),
                 paste0("  --invalid-background: #b798bf;"),
                 "}",
                 rstheme_body,
                 generate_xterm_color_block())

    # Save file
    path <- ifelse(grepl("/$", file_path), file_path, paste0(file_path, "/"))

    writeLines(rstheme, paste0(path, theme_name, ".rstheme"))

    end_time <- round(difftime(Sys.time(), start_time, units = "secs"), 3)
    message("\n- - - 'build_rstheme' execution time: ", end_time, " seconds\n")

    invisible(rstheme)
}


#' Auto Generate Last Block of Theme File
#'
#' @description
#' Build the rather large last block of color code values automatically.
#'
#' @return
#' Returns the fully put together last block of a theme file as character vector.
#'
#' @noRd
generate_xterm_color_block <- function(){
    # Generate full 256-color xterm CSS file
    # Define the 16 base ANSI colors
    base_colors <- c("#000000", "#800000", "#008000", "#808000",
                     "#000080", "#800080", "#008080", "#c0c0c0",
                     "#808080", "#ff0000", "#00ff00", "#ffff00",
                     "#0000ff", "#ff00ff", "#00ffff", "#ffffff")

    # Build the 6x6x6 color cube (indices 16-231)
    cube_levels <- c(0, 95, 135, 175, 215, 255)
    cube_colors <- c()

    for (r in cube_levels){
        for (g in cube_levels){
            for (b in cube_levels){
                cube_colors <- c(cube_colors, sprintf("#%02x%02x%02x", r, g, b))
            }
        }
    }

    # Build grayscale ramp (indices 232-255)
    gray_levels <- seq(8, 238, by = 10)
    gray_colors <- sprintf("#%02x%02x%02x", gray_levels, gray_levels, gray_levels)

    # Combine all 256 colors
    xterm_palette <- c(base_colors, cube_colors, gray_colors)

    # Generate CSS lines for each color
    unlist(lapply(seq_along(xterm_palette) - 1, function(i){
        color <- xterm_palette[i + 1]

        c(sprintf(".xtermColor%d { color: %s !important; }", i, color),
          sprintf(".xtermBgColor%d { background-color: %s; }", i, color))
    }))
}


# The main body which remains unchanged because the colors are saved in global variables.
rstheme_body <- '.ace_gutter {
  color: var(--line-number);
}

.ace_print-margin {
  width: 2px;
  box-shadow: 1px 0 0 0 var(--print_margin) inset;
}

.ace_editor,
.rstudio-themes-flat.ace_editor_theme .profvis-flamegraph,
.rstudio-themes-flat.ace_editor_theme,
.rstudio-themes-flat .ace_editor_theme {
  background-color: var(--editor-background);
  color: var(--editor-font);
  line-height: 1.3 !important;
}

.ace_cursor {
  border-color: var(--cursor);
  width: 2px;
}

.ace_marker-layer .ace_selection {
  background-color: var(--selection);
}

.ace_selection.ace_start {
  box-shadow: 0 0 3px 0px var(--selection);
  border-radius: 2px;
}

.ace_marker-layer .ace_step {
  background: var(--selection);
}

.ace_marker-layer .ace_bracket {
  outline: 1px solid var(--bracket-highlight);
  background-color: var(--bracket-highlight);
}

.ace_marker-layer .ace_active-line {
  outline: 2px solid var(--active-line);
  background-color: var(--active-line);
}

.ace_gutter-active-line {
  background-color: var(--active-line);
}

.ace_marker-layer .ace_selected-word {
  background-color: var(--smart-highlight);
}

.ace_invisible {
  color: var(--whitespace);
}

.ace_operator{
  color: var(--symbols);
}

.ace_keyword:not(.ace_operator),
.ace_meta,
.ace_storage,
.ace_storage.ace_type,
.ace_support.ace_type {
  color: var(--keyword);
}

.ace_constant.ace_character,
.ace_keyword.ace_other.ace_unit,
.ace_support.ace_constant,
.ace_variable.ace_parameter {
  color: var(--string);
}

.ace_constant.ace_language,
.ace_keyword.ace_virtual-comment {
  color: var(--language-constant);
}

.ace_constant.ace_numeric,
.ace_constant.ace_other {
  color: var(--numeric-constant);
}

.ace_invalid {
  color: var(--invalid-font);
  background-color: var(invalid-background);
}

.ace_invalid.ace_deprecated {
  color: var(--invalid-font);
  background-color: var(invalid-background);
}

.ace_gutter-cell:not([style*="top"]) {
  position: relative;
}

.ace_line .ace_fold {
  background: none;
  height: inherit;
  margin: 0;
  border: none;
  position: relative;
}

.ace_line .ace_fold::before {
  color: grey;
  content: "...";
  position: absolute;
  left: 50%;
  transform: translateX(-50%);
}

.ace_gutter .ace_fold-widget {
  cursor: pointer;
  background: no-repeat border-box transparent;
  border: none;
  border-radius: 0;
  box-shadow: none;
  width: 17px;
  position: absolute;
  top: 50%;
  right: 0;
  transform: translate(-50%, -50%);
}

.ace_gutter .ace_fold-widget.ace_closed {
  background-image: url("data:image/svg+xml;charset=utf-8,%3Csvg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 15 15"%3E%3Cpath opacity=".1" fill="%23fff" d="M3 3h9v9H3z"/%3E%3Cpath d="M11 4v7H4V4h7m1-1H3v9h9V3z" fill="%235a5a5a"/%3E%3Cpath fill="none" stroke="%23c5c5c5" stroke-miterlimit="10" d="M10 7.5H5M7.5 5v5"/%3E%3C/svg%3E");
}

.ace_gutter .ace_fold-widget.ace_open {
  background-image: url("data:image/svg+xml;charset=utf-8,%3Csvg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 15 15"%3E%3Cpath d="M11 4v7H4V4h7m1-1H3v9h9V3z" fill="%235a5a5a"/%3E%3Cpath fill="none" stroke="%23c5c5c5" stroke-miterlimit="10" d="M10 7.5H5"/%3E%3C/svg%3E");
  opacity: 0;
  transition: opacity 0.5s;
}

.ace_gutter:hover .ace_fold-widget.ace_open {
  opacity: 1;
}

.ace_entity.ace_name.ace_function,
.ace_identifier.ace_support.ace_function:not(.ace_virtual-comment) {
  color: var(--function-declaration);
}

.ace_identifier,
.ace_variable {
  color: var(--variable);
}

.ace_support.ace_class,
.ace_support.ace_type {
  color: var(--class-name);
}

.ace_heading,
.ace_markup.ace_heading {
  color: var(--language-constant);
}

.ace_markup.ace_heading .ace_entity.ace_name.ace_tag,
.ace_entity.ace_other.ace_attribute-name,
.ace_meta.ace_tag,
.ace_string {
  color: var(--string);
}

.ace_string.ace_regexp {
  color: var(--regex);
}

.ace_comment {
  color: var(--comment);
}

/* rmarkdown code */
[class="ace_support ace_function"] {
  color: var(--string);
}

/* rmarkdown emphasis */
.ace_constant.ace_language.ace_boolean.ace_text {
  font-style: italic;
}

/* rmarkdown strong emphasis */
.ace_constant.ace_numeric.ace_text {
  font-weight: bold;
}

.ace_indent-guide {
  box-shadow: 1px 0 0 0 var(--whitespace);
  display: inline-block;
}

.nocolor.ace_editor .ace_line span {
  color: var(--string);
}

.ace_marker-layer .ace_foreign_line {
  position: absolute;
  z-index: -1;
  background-color: var(--code-line);
}

.ace_marker-layer .ace_active_debug_line {
  position: absolute;
  width: 100% !important;
  z-index: -1;
  background-color: var(--debug-line);
}

.ace_marker-layer .ace_find_line {
  position: absolute;
  z-index: -1;
  background-color: var(--smart-highlight);
}lsl

.ace_console_error {
  color: var(--invalid-font);
  background-color: var(--invalid-background);
  font-weight: bold;
}

#rstudio_console_output .ace_keyword {
  color: var(--console-code);
  font-weight: normal;
}

#rstudio_console_output .ace_constant.ace_language {
  color: var(--language-constant);
}

.rstudio-themes-flat.editor_dark.ace_editor_theme a {
  color: var(--editor-font) !important;
}

.ace_layer {
  z-index: 3;
}

.ace_layer.ace_print-margin-layer {
  z-index: 2;
}

.ace_layer.ace_marker-layer {
  z-index: 1;
}

.rstudio-themes-flat.rstudio-themes-dark-menus .ace_editor.ace_autocomplete {
  background: var(--tab);
  border: solid 1px var(--toolbar) !important;
  color: var(--editor-font);
}

.rstudio-themes-flat.rstudio-themes-dark-menus
  .ace_editor.ace_autocomplete
  .ace_marker-layer
  .ace_active-line,
.rstudio-themes-flat.rstudio-themes-dark-menus
  .ace_editor.ace_autocomplete
  .ace_marker-layer
  .ace_line-hover {
  background: var(--editor-background);
  border: none;
}

/* positioning (exclude rstudio server) */
.rstudio-themes-flat > .rstudio-themes-dark div:last-child[style*="top: 29px"] {
  top: 24px !important;
  right: 0 !important;
  bottom: 0 !important;
  left: 0 !important;
}

.rstudio-themes-flat > .rstudio-themes-dark div:last-child[style*="top: 5px"] {
  top: 0 !important;
  right: 0 !important;
  bottom: 0 !important;
  left: 0 !important;
}

.rstudio-themes-dark
  :-webkit-any(div:last-child[style*="top: 5px"], div:last-child[style*="top: 29px"])
  .gwt-SplitLayoutPanel-Workbench {
  top: 0 !important;
  right: 0 !important;
  bottom: 0 !important;
  left: 0 !important;
}

/* background */
.rstudio-themes-flat > .rstudio-themes-dark,
.rstudio-themes-flat
  > .rstudio-themes-dark
  :-webkit-any(.gwt-TabLayoutPanel, .gwt-TabLayoutPanelContent) {
  background: var(--toolbar) !important;
}

/* splitters */
.rstudio-themes-flat > .rstudio-themes-dark .gwt-SplitLayoutPanel-HDragger {
  background: var(--toolbar) !important;
  cursor: ew-resize;
  border-color: var(--toolbar);
}

.rstudio-themes-flat > .rstudio-themes-dark .gwt-SplitLayoutPanel-VDragger {
  background: var(--toolbar) !important;
  cursor: ns-resize;
  border-color: var(--toolbar);
  /* box-shadow: 0 -1px 0 var(--editor-background) inset; */
}

/* window containers */
.rstudio-themes-flat > .rstudio-themes-dark
  :-webkit-any(.windowframe, .rstheme_minimizedWindowObject)
  > div:last-child {
  border-radius: 0;
  border: none !important;
}

/* main toolbar */
.rstudio-themes-dark > div:last-child > div > div > .rstheme_toolbarWrapper,
.rstudio-themes-dark [role="application"] > div > .rstheme_toolbarWrapper {
  background-color: var(--toolbar) !important;
  border-color: var(--toolbar) !important;
}

/* toolbars */
.rstudio-themes-dark .rstheme_toolbarWrapper,
.rstudio-themes-dark .rstheme_secondaryToolbar {
  background-color: var(--editor-background) !important;
  border-color: var(--editor-background) !important;
}

/* other toolbars */
.rstudio-themes-dark .rstudio-themes-background {
  background-color: var(--editor-headline) !important;
  border-top-color: var(--editor-headline) !important;
  border-bottom-color: var(--editor-headline) !important;
  border-radius: 0;
}

/* git status toolbar */
.rstudio-themes-flat .rstudio-themes-dark .GD15MCFCB2C,
.rstudio-themes-flat .rstudio-themes-dark .GGBOEFPDC4C {
  background-color: var(--editor-headline) !important;
}

/* files breadcrumb gradient */
.rstudio-themes-dark .breadcrumb > div:last-child {
  background: var(--editor-background) !important;
  left: inherit !important;
}

/* tabs container */
.rstudio-themes-dark .gwt-TabLayoutPanelTabs,
/* windows */
.rstudio-themes-dark .windowframe,
/* remove gradients */
.rstudio-themes-dark :-webkit-any(.rstheme_multiPodUtilityTabArea, .GD15MCFCCS, .GJQ3LUQCCS, .GGBOEFPDDS) {
  background: var(--toolbar) !important;
  border-color: var(--toolbar) !important;
}

/* tabs */
.rstudio-themes-dark
  .gwt-TabLayoutPanelTab:not(.gwt-TabLayoutPanelTab-selected)
  .rstheme_tabLayoutCenter,
.rstudio-themes-dark .rstheme_minimizedWindowObject .rstheme_center {
  background-color: var(--tab) !important;
  color: var(--editor-font) !important;
  border-color: var(--tab) !important;
  border-right: 1px solid var(--toolbar) !important;
  border-radius: 0;
  cursor: pointer;
}

.rstudio-themes-dark .gwt-TabLayoutPanelTab .gwt-Label {
  cursor: pointer;
}

/* active tab */
.rstudio-themes-dark
  .gwt-TabLayoutPanelTab.gwt-TabLayoutPanelTab-selected
  .rstheme_tabLayoutCenter {
  background-color: var(--selected-tab) !important;
  color: white !important;
}

/* editor */
#rstudio_shell_widget.ace_editor.ace_scroller,
.ace_editor[id^="rstudio_source_text_editor"]:not(#rstudio_source_text_editor_0)
  :-webkit-any(.ace_gutter, .ace_scroller, .ace_scrollbar-v) {
  box-shadow: inset 0 6px 6px -6px black;
}

/* datagrid */
.rstudio-themes-dark .dataGridHeader,
.rstudio-themes-dark tr[__gwt_header_row] > :-webkit-any(td, th),
.rstudio-themes-dark .dataTables_info {
  background-color: var(--editor-headline) !important;
}

/* data table */
.rstudio-themes-dark .dataTable :-webkit-any(th, .first-child) {
  background-color: var(--editor-headline) !important;
}

.rstudio-themes-dark .dataTable :-webkit-any(th, td, tr) {
  border-color: silver !important;
}

.rstudio-themes-dark .dataTable tbody > tr:hover > td {
  background-color: var(--editor-headline) !important;
}

/* search background */
.rstudio-themes-dark
  .gwt-TabLayoutPanelContent
  > div:nth-last-child(2):nth-child(4)
  .rstudio-themes-background {
  background-color: var(--toolbar) !important;
}

/* search input */
.rstudio-themes-dark
  :-webkit-any(.rstheme_toolbarWrapper, #rstudio_find_replace_bar)
  .search,
.rstudio-themes-flat .themedPopupPanel .search {
  background-color: var(--editor-headline) !important;
  color: var(--editor-font) !important;
  border-radius: 0 !important;
  border: 1px solid transparent;
}

.rstudio-themes-dark
  .gwt-TabLayoutPanelContent
  > div:nth-last-child(2):nth-child(4)
  .rstudio-themes-background
  .search {
  height: 18px;
}

.rstudio-themes-dark .search:-webkit-any(:focus, :focus-within) {
  outline: 1px solid var(--editor-font);
  outline-offset: -1px;
}

/* scrollbars */
::-webkit-scrollbar {
  height: calc(17px * 10 / 14) !important;
}
::-webkit-scrollbar-thumb {
  border: none !important;
  border-radius: 0 !important;
  background-color: var(--scrollbar) !important;
}

::-webkit-scrollbar-thumb:hover {
  background-color: var(--scrollbar-hover) !important;
}

::-webkit-scrollbar-thumb:active {
  background-color: var(--scrollbar-active) !important;
}

::-webkit-scrollbar,
::-webkit-scrollbar-corner,
::-webkit-scrollbar-track {
  background-color: transparent !important;
}

::-webkit-scrollbar-track:vertical {
  box-shadow: inset 1px 0 0 0 var(--editor-background);
}

/* menus */
.rstudio-themes-flat .themedPopupPanel,
.rstudio-themes-flat .popupMiddleCenter,
.rstudio-themes-flat .menuPopupMiddleCenter {
  color: var(--editor-font) !important;
  background-color: var(--tab) !important;
  border-color: var(--editor-headline) !important;
}

.rstudio-themes-flat .themedPopupPanel ::-webkit-scrollbar:vertical {
  width: 10px !important;
}

.rstudio-themes-flat .themedPopupPanel ::-webkit-scrollbar-track:vertical {
  box-shadow: none;
}

.rstudio-themes-flat .gwt-MenuItem-selected {
  background-color: var(--editor-headline) !important;
}

.rstudio-themes-flat .gwt-MenuItemSeparator > .menuSeparatorInner {
  border-color: var(--editor-headline) !important;
}

/* terminal */
.terminal {
  background-color: var(--editor-background);
  color: var(--editor-font);
  font-feature-settings: "liga" 12;
  position: relative;
  user-select: none;
  -ms-user-select: none;
  -webkit-user-select: none;
}
.terminal.xterm-cursor-style-block.focus:not(.xterm-cursor-blink-on)
  .terminal-cursor {
  background-color: var(--editor-headline);
  color: var(--cursor);
}
.terminal.focus.xterm-cursor-style-bar:not(.xterm-cursor-blink-on)
  .terminal-cursor::before,
.terminal.focus.xterm-cursor-style-underline:not(.xterm-cursor-blink-on)
  .terminal-cursor::before {
  content: "";
  position: absolute;
  background-color: var(--editor-headline);
}
.terminal:not(.focus) .terminal-cursor {
  outline: 1px solid var(--editor-headline);
  outline-offset: -1px;
}
.terminal .xterm-selection div {
  position: absolute;
  background-color: var(--editor-headline);
}
.terminal .xterm-viewport {
  background-color: var(--string);
  overflow-y: scroll;
}
.xtermInvertColor {
  color: var(--editor-background);
}
.xtermInvertBgColor {
  background-color: var(--editor-background);
}
.xtermBold {
  font-weight: bold;
}
.xtermUnderline {
  text-decoration: underline;
}
.xtermBlink {
  text-decoration: blink;
}
.xtermHidden {
  visibility: hidden;
}
.xtermItalic {
  font-style: italic;
}
.xtermStrike {
  text-decoration: line-through;
}
'
