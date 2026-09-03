# ggbothbar 1.2.0

## Breaking changes

- The minimum supported R version is now 4.1.
- `Geomerrorbarb`, `errorbarbGrob`, `create_errorbarb`, and
  `makeContent.errorbarb` are no longer exported. These rendering backends are
  internal implementation details; use `geom_errorbarb()` for plots. The grid
  S3 method remains registered for internal grob dispatch.
- `write_sheets()` now defaults to `overwrite = FALSE`. The transitional
  `overwrite = NULL` state from 1.1.2 is no longer accepted; use an explicit
  `TRUE` to replace an existing local or downloaded xlsx file.

## Dependency and I/O boundaries

- `googlesheets4` and `googledrive` moved from Imports to Suggests. Google
  Sheets remains the default destination. When a selected Google workflow is
  unavailable, `write_sheets()` raises a `ggbothbar_missing_dependency` error
  that names the missing packages and shows an installation command; it never
  installs packages or silently changes the destination.
- `local = TRUE` takes precedence when `download = TRUE` is also supplied, so
  this path requires only `openxlsx` and performs no Google operation.

## Structure and presentation

- Split calculation, label, plotting-helper, validation, and spreadsheet I/O
  responsibilities into focused source files. Public spreadsheet calls now
  pass through validation before reaching local or Google side-effect
  adapters.
- Isolated ggplot2 panel-range and coordinate handling behind an internal
  compatibility adapter, and separated the shared two-axis summary logic from
  its Stat and Geom implementations without changing validated summaries.
- `theme_aca()` and its compatibility alias `theme_isotope()` no longer discard
  mapped legends. Other theme settings are unchanged.
- Added focused behavioral, API-surface, dependency-routing, and mapped-legend
  regressions for the structural release.

# ggbothbar 1.1.2

## Bug fixes

- `geom_errorbarb()` now uses an internal summary stat by default, calculates
  uncertainty in the original data space before scale transformation, draws a
  valid one-axis interval when the other axis has zero variance, and honours
  `lineend`. Set `stat = "identity"` to draw precomputed `xmin`, `xmax`, `ymin`,
  and `ymax` endpoints.
- Numeric `errorbar_tip_size` values are now documented in millimetres, matching
  their historical rendered size. The default remains `2`, producing an
  approximately 2 mm cap, and explicit numeric values retain their previous
  physical width (#15).
- `geom_errorbox()` now honours its `stat` argument and, like
  `geom_errorbarb()`, calculates summaries in the original data space before
  applying scale transformations.
- `adjust_axis_scales()` and `align_axis_scales()` preserve continuous scale
  transformations and `coord_flip()`. Free-scale facets and unsupported
  coordinates now produce a classed best-effort fallback warning.
- Replaced the stale non-linear-coordinate warning text for
  `geom_errorbarb()`.

## Calculation and file-safety changes

- `calc_enrichment()` now validates numeric isotope columns and unique input
  and output names. It protects existing output columns unless
  `overwrite = TRUE`, excludes missing grouping values from reference rows,
  and warns while returning `NA` only for enrichment columns whose reference
  mean is not finite.
- `calc_error()` removes missing values before calling custom functions when
  requested and validates every method result as one finite numeric value or
  `NA_real_`.
- `write_sheets()` now accepts `overwrite`. Explicit `FALSE` protects existing
  local files and explicit `TRUE` replaces them. For this transition release,
  the default `NULL` keeps the historical replacement behaviour and warns when
  local output may be written. Local workbooks are first saved to a temporary
  file in the destination directory and then replaced atomically where the
  platform permits.

## Maintenance

- Added testthat Edition 3 regression tests, a focused vdiffr snapshot, and a
  multi-platform R-CMD-check workflow including the R 3.5 floor.
- Added an English package vignette and corrected README examples and package
  metadata. The minimum supported R version is now declared as 3.5.

# ggbothbar 1.1.1

## New features

- Added `stat_mean_label()` for placing labels at group mean positions.
- Added `theme_aca()` as the preferred academic plotting theme name; `theme_isotope()` remains available for backward compatibility.
- Added `fix_aspect_ratio()` as the preferred aspect-ratio helper name; `fix_limit()` remains available for backward compatibility.
- Added formatted spreadsheet output options to `write_sheets()`: header filters, first-row freezing, and automatic column widths for local xlsx and Google Sheets exports.

## Improvements

- Improved `write_sheets()` input validation for empty data lists, invalid logical options, duplicate sheet names, long sheet names, and Excel-incompatible sheet names.
- Changed Google Sheets downloads to use the newly created spreadsheet ID instead of name-based Drive search, avoiding accidental downloads of similarly named files.
- Improved `fix_aspect_ratio()` error messages for discrete axes, empty plots, zero-width ranges, and invalid `.ratio` values.
- Made `label_isotope()` use Unicode escapes internally while preserving rendered isotope labels.

## Bug fixes

- Avoided empty panel output in errorbar grob rendering.
- Made undefined confidence intervals in `calc_error(fun.errorbar = "ci")` return `NA_real_` instead of producing `NaN` warnings.
- Added a warning when `stat_mean_label()` receives multiple labels within one group while preserving the existing first-label behavior.

## Maintenance

- Added repository agent instructions and kept generated Rd files synchronized with `devtools::document()`.

# ggbothbar 1.0.0

## Breaking changes

- Deprecated `draw_reference_box()` in favor of `geom_errorbox()` (#4)

## New features

- Added `geom_errorbox()` for enhanced error box visualization (#4)
- Added `fix_aspect_ratio()` function to adjust plot aspect ratios based on plot dimensions; `fix_limit()` remains available for backward compatibility (#6)
- Added customizable theme system `theme_aca()`; `theme_isotope()` remains available for backward compatibility
- Added `delta` parameter to `calc_enrichment()` for flexible isotope column specification (#5)
- Added `label_isotope()` for creating labels (e.g., axis title) of isotope plot.
- Added `write_sheets()` for writting multiple data.frame into one Excel sheet / Google Spreadsheet.

## Improvements

- Enhanced documentation with more detailed examples
- Standardized theme settings with customizable parameters
