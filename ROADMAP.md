# ggbothbar roadmap

This roadmap records the agreed work for the next two GitHub releases. It
separates correctness repairs from broader restructuring so that behavior can
be fixed and tested before internal boundaries change.

## Guiding decisions

- Scientific and data-integrity fixes take priority over preserving incorrect
  behavior.
- Work proceeds sequentially: finish the 1.1.2 release candidate before
  starting 1.2.0.
- The package remains focused on isotope visualization and directly supporting
  calculations. No new analysis domain is added in either milestone.
- Both releases target CRAN-grade package checks, although CRAN submission is
  not part of this roadmap.
- Unsupported plotting combinations use detailed warnings and best-effort
  fallback rather than hard errors. The limitations of that policy are listed
  under [Accepted risks](#accepted-risks).

## Relative sizing

- **S** — a localized implementation and its direct tests.
- **M** — a change spanning several functions, contracts, or test types.
- **L** — architectural or cross-platform work requiring staged integration.

Sizes compare implementation and review scope; they are not calendar estimates.

## Release overview

| Milestone | Purpose | Minimum R | Compatibility |
|---|---|---:|---|
| 1.1.2 | Repair release with tests, CI, and documentation | 3.5 | Preserve valid workflows; correct faulty results and contracts |
| 1.2.0 | Structural cleanup and dependency-boundary revision | 4.1 | Contains explicitly accepted breaking changes |

Dependency order:

`tests and CI` → `calculation and rendering repairs` → `API and I/O repairs` →
`documentation and package check` → `1.1.2 candidate` → `structural changes` →
`1.2.0 candidate`

## Milestone 1.1.2 — repair release

### 1. Establish the safety net — L

- Restore a tracked testthat Edition 3 suite, organized to mirror the relevant
  `R/` files.
- Add a regression test that fails for every confirmed defect before fixing it.
- Use numeric, grob, and physical-unit assertions as the primary rendering
  checks. Add vdiffr snapshots only for a small set of representative plots.
- Add CI jobs for:
  - the R 3.5 floor, oldrel, release, and devel on Ubuntu;
  - release R on Windows and macOS.
- Treat behavioral coverage as the release gate. Code-coverage percentages may
  be reported but are not pass/fail criteria.
- Mock Google services in CI. Keep a manual live Google smoke test as an
  optional release checklist item.

Acceptance criteria:

- Tests run from a clean checkout without credentials or network writes.
- Every confirmed bug has a focused observed-versus-expected regression test.
- Package build and check are reproducible on the declared matrix.

### 2. Repair enrichment and error calculations — M

#### `calc_enrichment()`

- Require numeric, uniquely named `delta` inputs.
- Require unique `epsilon` names.
- Add `overwrite = FALSE`; an existing output column may be replaced only when
  the caller explicitly uses `overwrite = TRUE`.
- Exclude missing grouping keys from reference selection.
- If a reference mean is not finite, emit a column-specific warning and fill
  only the affected enrichment column with `NA`.
- Preserve input row order and data-frame class.

#### `calc_error()`

- Apply `na.rm = TRUE` before calling a user-supplied function.
- Require a length-one numeric result.
- Permit `NA_real_` for mathematically undefined uncertainty; reject `NaN`,
  infinity, non-numeric results, and results of length other than one.

Acceptance criteria:

- Duplicate output names always fail before the result is modified.
- Existing columns fail by default and change only with explicit overwrite.
- Missing grouping keys cannot enter the reference subset.
- One invalid reference isotope does not discard valid calculations for other
  isotopes.

### 3. Repair two-axis uncertainty rendering — L

- Introduce an internal summary stat and make it the default for
  `geom_errorbarb()`.
- Change the default contract to `stat = "errorbarb"`.
- Define `stat = "identity"` as drawing precomputed `xmin`, `xmax`, `ymin`, and
  `ymax` endpoints.
- Compute centers and SD/SE intervals in the original data space, then transform
  the resulting coordinates for display.
- Draw the valid direction when only one axis has non-zero variance.
- Honor the public `lineend` argument.
- Correct the cap-size unit conversion. Numeric `errorbar_tip_size` remains in
  centimeters, while its default changes from `2` to `0.2` so that the current
  default visual width remains approximately 2 mm.
- Record the change to explicitly supplied cap sizes in NEWS and close
  [issue #15](https://github.com/Hide-Fun/ggbothbar/issues/15) only after the
  physical-unit regression test passes.

Acceptance criteria:

- A 0.18 cm cap measures 1.8 mm in the resulting grob.
- The default cap retains its intentional approximately 2 mm appearance.
- Summary values are independent of viewport dimensions.
- A group with zero x variance and positive y variance still draws the y
  interval, and vice versa.
- `lineend = "square"` reaches the final grid graphical parameters.

### 4. Repair scale and coordinate helpers — L

- Preserve non-identity continuous scale transformations, including log scales.
- Preserve `coord_flip()` semantics while applying requested ranges and breaks.
- Validate break steps, limits, aspect ratios, and clipping options before
  modifying a plot.
- For free facets and other unsupported combinations, emit a classed warning
  that identifies the affected plot and the scale, coordinate, or facet
  property that may be lost, then return the best-effort fallback.
- Keep warnings in the R condition stream only; do not add captions or
  watermarks to saved figures.

Acceptance criteria:

- Supported log scales remain log-transformed after adjustment.
- `coord_flip()` remains in force after adjustment.
- Free-facet fallback is never silent and its warning is testable by class.
- The documented supported and best-effort cases match the regression matrix.

### 5. Repair remaining API and file-safety contracts — M

- Honor the public `stat` argument in `geom_errorbox()`.
- Correct the stale `spring geom` warning text.
- Add an explicit overwrite argument to `write_sheets()` and its local/download
  adapters.
- During 1.1.2, use `overwrite = NULL` as the compatibility state: preserve the
  previous overwrite behavior but emit a migration warning. Accept explicit
  `TRUE` and `FALSE` without that warning.
- Prefer a same-directory temporary file and atomic replacement for local xlsx
  output where the platform supports it.

Acceptance criteria:

- Public arguments either alter behavior as documented or fail informatively;
  none are silently ignored.
- An existing xlsx destination has a documented, testable outcome for all three
  overwrite states.
- Automated tests do not write to Google services.

### 6. Complete release documentation and metadata — M

- Add a short English package vignette covering the central isotope workflow:
  enrichment, grouped uncertainty, labels, and the package theme.
- Track only the new vignette source. Do not modify or delete the existing
  ignored local vignette drafts or generated files.
- Update the targeted `.gitignore` rules when the new vignette source is added,
  without exposing the existing ignored drafts.
- Correct DESCRIPTION title/description metadata, the author-name typo, and the
  effective R requirement.
- Remove `LazyData` while no package dataset exists.
- Keep vignette metadata only when it matches the tracked vignette source.
- Correct README naming and dependency examples; regenerate README figures from
  source.
- Record every user-facing behavior change in NEWS.
- Regenerate NAMESPACE and Rd files from roxygen comments with
  `devtools::document()`; do not edit `man/*.Rd` by hand.

Release gate:

- 0 errors and 0 warnings from package check.
- 0 package-origin actionable NOTEs. Platform or repository-policy diagnostics
  are documented separately when they cannot be eliminated by the package.
- Examples, tests, vignette build, README rendering, and isolated package check
  succeed from a clean checkout.
- Source version, future tag, and future GitHub release are prepared to refer to
  the same commit. Tagging and publishing require separate authorization.

## Milestone 1.2.0 — structural release

### 1. Split responsibilities and stabilize internal boundaries — L

- Divide the current `utilities.R` responsibilities into calculation,
  plotting-helper, label, sheet-I/O, and validation files.
- Organize internals as public wrapper → validation/pure calculation → side
  effect adapter.
- Isolate ggplot2 panel-range and coordinate compatibility code behind a small,
  tested adapter.
- Finish the Stat/Geom separation begun in 1.1.2 without changing validated
  summary semantics.
- Use the base pipe in files changed for structural work; avoid repository-wide
  style-only rewrites.

### 2. Revise dependency and I/O boundaries — M

- Raise the minimum R version to 4.1 and run the agreed CI matrix with an R 4.1
  floor job.
- Move Google client packages from Imports to Suggests.
- Keep the existing Google destination as the `write_sheets()` default.
- If optional Google packages are absent, raise a classed error naming the
  required packages and installation command. Never auto-install packages or
  silently switch to local output.
- Change the xlsx overwrite default to `FALSE`, completing the 1.1.2 migration.
- Keep ggplot2 in Depends; attaching ggbothbar continues to attach ggplot2.

### 3. Reduce the exported backend surface — M, breaking

Immediately stop exporting these rendering backends:

- `Geomerrorbarb`
- `errorbarbGrob`
- `create_errorbarb`
- `makeContent.errorbarb`

Keep the required S3 method registration. Do not provide compatibility aliases
for these four objects. This intentionally breaking change remains versioned as
1.2.0 rather than 2.0.0 and must be prominent in NEWS.

Retain these existing compatibility names:

- `fix_limit()` and `fix_aspect_ratio()`
- `theme_aca()` and `theme_isotope()`

### 4. Correct theme information loss — S

- Stop forcing `legend.position = "none"` in the shared theme implementation.
- Keep both public theme names and otherwise preserve their appearance.
- Add a visual regression covering a mapped legend.

### 5. Validate the structural release — M

- Re-run all 1.1.2 behavioral and visual regressions unchanged unless a
  documented breaking contract requires an reviewed update.
- Verify installation both with and without optional Google packages.
- Verify that removed backend exports are absent while the public geoms still
  render correctly.
- Require the same clean build/check gate as 1.1.2.

## Deferred and out of scope

- Group-specific reference means such as `calc_enrichment(by = ...)` remain a
  discovery item until a second independent workflow demonstrates demand.
- Asymmetric/precomputed convenience geoms, one-call isotope plotting wrappers,
  ellipses, niche metrics, mixing models, and broad isotope calculations are not
  added in these milestones.
- Prefer interoperability documentation over duplicating SIBER, simmr, MixSIAR,
  or isocalcR functionality.
- CRAN submission, Git tags, pushes, and GitHub releases require a separate
  explicit decision.

## Accepted risks

- Free-facet and other unsupported best-effort fallbacks may return a plot whose
  scientific meaning is degraded. The warning is console-only, so a saved image
  may still look plausible without carrying that warning.
- Removing four exported backend objects in 1.2.0 is a breaking public-API
  change despite using a minor version number.
- Keeping Google output as the default while making its dependencies optional
  means that a default call can fail until those optional packages are installed.
- Keeping ggplot2 in Depends retains package-attachment side effects.

These risks are deliberate decisions, not accidental omissions. Tests and NEWS
must describe them without claiming full support where only fallback exists.

## Git and artifact policy

- Develop 1.1.2 and 1.2.0 sequentially on separate topic branches based on the
  up-to-date `dev` branch.
- Commit verified changes in semantically coherent local commits.
- Do not tag, push, publish a release, or create external issues without explicit
  authorization.
- Keep `.collective/` as ignored local audit state. Maintain this tracked roadmap
  as the user-facing plan.
- Preserve unrelated tracked changes and all existing ignored vignette drafts.
