# Refactor Evaluation: `{ggbreak}`

**Date:** 2026-06-08

**Package version:** 0.0.0.9000

**Scope:** `R/add_break_symbol.R`, `NAMESPACE`, `DESCRIPTION`, `README.md`, `man/`

**Agent:** Claude Sonet 4.6

---

## Summary

`{ggbreak}` is a focused, single-function package that fulfils a clear purpose. The core logic works and the README gives good practical guidance. This evaluation identifies bugs, code design issues, documentation inaccuracies, and future development opportunities to support the package's ongoing development.

---

## 1. Bugs

### 1.1 `y_labels` rejects vector inputs — undocumented breaking behaviour

**File:** `R/add_break_symbol.R`

The `@param y_labels` documentation states the argument accepts:

> *"Either a vector of labels corresponding to `y_breaks`, a labeling function … or `waiver()`."*

However, the validation block rejects anything that is not a function:

```r
if (!is_waive(y_labels)) {
  if (!is.function(y_labels)) {
    cli::cli_abort(c(
      x = "{.var y_labels} must be a labeller function from the {.pkg scales} package.",
      ...
    ))
  }
}
```

Passing a character or numeric vector (e.g., `y_labels = c("Low", "Mid", "High")`) will throw an error at runtime despite being explicitly documented as valid. Either the validation must be relaxed to accept character/numeric vectors, or the documentation must be corrected and the vector-label pathway removed.

---

### 1.2 Copy-paste error in `cli_abort` message

**File:** `R/add_break_symbol.R`

Inside the check that all `break_style` elements are numeric, the error message references the wrong variable:

```r
# Actual code:
if (any(!sapply(break_style[style_keys], is.numeric))) {
  cli::cli_abort("Each element in {.var y_limits} must be a numeric.")
  #                                   ^^^^^^^^ should be `break_style`
}
```

A user encountering this error will be told to check `y_limits` when the problem is with `break_style`. The variable name in the message should be corrected to `{.var break_style}`.

---

### 1.3 Grammar error in `@details`

**File:** `R/add_break_symbol.R`

The `@details` section reads:

> *"The function adds a vertical dark intersected by two diagonal parallel lines…"*

The word **"line"** is missing. It should read:

> *"The function adds a vertical dark **line** intersected by two diagonal parallel lines…"*

---

## 2. Performance

### 2.1 `ggplot_build()` called three times unnecessarily

**File:** `R/add_break_symbol.R`

`ggplot_build()` is an expensive operation — it fully renders the plot. It is called separately three times to extract values that could all be retrieved from a single call:

```r
# Call 1:
x_p_range <- ggplot2::ggplot_build(plot)$layout$panel_params[[1]]$x.range

# Call 2:
y_p_range <- ggplot2::ggplot_build(plot)$layout$panel_params[[1]]$y.range

# Call 3 (inside the if-block for auto-calculating y_breaks):
y_p_breaks <- ggplot2::ggplot_build(plot)$layout$panel_params[[1]]$y$breaks

# Call 4 (for data range):
y_d_range <- range(ggplot2::ggplot_build(plot)$data[[1]]$y)
```

The result should be cached once at the top of the function body and reused:

```r
built   <- ggplot2::ggplot_build(plot)
params  <- built$layout$panel_params[[1]]
x_p_range <- params$x.range
y_p_range <- params$y.range
y_d_range <- range(built$data[[1]]$y)
# ... and later:
y_p_breaks <- params$y$breaks
```

This is particularly important for plots with many layers or large underlying datasets.

---

## 3. Code Design

### 3.1 Private helpers defined inside the exported function

**File:** `R/add_break_symbol.R`

Three utility functions are defined inside `add_break_symbol()`:

```r
ith_geom <- function(p, i) { ... }
is_waive <- function(x) inherits(x, "waiver")
between  <- function(x, left, right) { x >= left & x <= right }
```

Defining helpers inside an exported function means they are:

- **Re-created on every function call**, adding minor but unnecessary overhead.
- **Untestable** — they cannot be referenced or called directly in a `testthat` test suite.
- **Harder to read** — they increase the apparent complexity of `add_break_symbol()` itself.

These functions should be moved to a private `utils.R` file (or `helpers.R`) with no `@export` tag, consistent with standard R package conventions.

---

### 3.2 `y_origin_override` parameter overwritten mid-function

**File:** `R/add_break_symbol.R`

The user-facing parameter `y_origin_override` is silently reassigned inside the function to hold the *computed labels vector* passed to `scale_y_continuous()`:

```r
# User passes in, e.g., y_origin_override = 0
# ...then later:
y_origin_override <- y_labels            # branch 1
y_origin_override <- y_labels(y_labels_fmt) # branch 2
y_origin_override <- y_breaks            # branch 3
```

By the end of the function, `y_origin_override` no longer holds the user's original value — it holds the computed label output. This makes the code difficult to follow and debug. A clearly-named internal variable (e.g., `y_labels_out`) should be used for the computed result, keeping the parameter name stable throughout.

---

### 3.3 `sapply` used inconsistently alongside `vapply`

**File:** `R/add_break_symbol.R`

`vapply` is correctly used for type-safe iteration when extracting geom class names:

```r
plot_type <- vapply(seq_len(n_layers), ith_geom, character(1), p = plot)
```

But `sapply` (which has no enforced return type) is used in the `break_style` element check:

```r
if (any(!sapply(break_style[style_keys], is.numeric))) { ... }
```

For consistency and defensive programming, this should use `vapply`:

```r
if (any(!vapply(break_style[style_keys], is.numeric, logical(1)))) { ... }
```

---

### 3.4 Full `import(scales)` in `NAMESPACE`

**File:** `NAMESPACE`

```
import(scales)
```

This is a blanket import of the entire `{scales}` namespace. All other dependencies use selective `importFrom()`. The blanket import is unnecessary — only specific `{scales}` functions are used. Furthermore, `waiver()` is already re-exported by `{ggplot2}`, so the `{scales}` import in `NAMESPACE` may not be needed at all and should be reviewed. Selective `importFrom()` calls are preferable because they:

- Make dependencies explicit and auditable.
- Avoid namespace pollution.
- Reduce the risk of conflicts if `{scales}` changes its exports.

---

### 3.5 Redundant `coord_cartesian` + `scale_y_continuous(limits = ...)` with identical values

**File:** `R/add_break_symbol.R`

The returned plot object has both of these added with the same `y_limits` value:

```r
ggplot2::scale_y_continuous(limits = y_limits) +
ggplot2::coord_cartesian(ylim = y_limits)
```

`scale_y_continuous(limits = ...)` clips data outside the range (removing data points before rendering), whereas `coord_cartesian(ylim = ...)` zooms without clipping. When both are applied to the same range, `coord_cartesian` effectively overrides the zoom behaviour. The `@details` section hints at this distinction but does not explain why both are present simultaneously. The code should either:

1. Use `coord_cartesian` alone (safer — avoids data clipping), and remove `limits` from `scale_y_continuous()`, or
2. Add an inline comment clearly explaining why both are intentionally present.

---

### 3.6 Hard-coded break symbol colour

**File:** `R/add_break_symbol.R`

The colour `"#3D3D3D"` is hard-coded into all four `annotate("segment", ...)` calls:

```r
ggplot2::annotate("segment", ..., colour = "#3D3D3D")
```

This gives users no way to adjust the break symbol colour without modifying the package source. A `colour` element should be added to `break_style` with `"#3D3D3D"` as the default:

```r
break_style = list(height = 1, width = 1, linewidth = 0.5, colour = "#3D3D3D")
```

---

## 4. Documentation

### 4.1 `@param y_labels` — inaccurate, implies vector support

As described under Bug 1.1, the `@param y_labels` documentation must be updated to accurately reflect the inputs the function actually accepts. If vector labels are not supported, the documentation should say so and the error message should guide users towards `scales::label_*()` functions.

---

### 4.2 `@examples` uses `mtcars` — poor illustration of the use case

**File:** `R/add_break_symbol.R`

The `@examples` block uses `geom_line(aes(wt, mpg))` from `mtcars`, which produces a nonsensical zig-zag line with no clear narrative. The break symbol is most meaningful on a time-series or trend chart where the y-axis is intentionally truncated above zero. The README examples (using `rnorm()` data with a stable trend) are far more illustrative. The examples in `@examples` should mirror the README examples so that `?add_break_symbol` is as informative as the README.

---

### 4.3 No `@seealso` section

**File:** `R/add_break_symbol.R`

The `add_break_symbol()` documentation would benefit from a `@seealso` section linking to:

- `ggplot2::scale_y_continuous()`
- `ggplot2::coord_cartesian()`
- `scales::label_comma()`, `scales::label_percent()`, etc.

This helps users discover the related functions they need to work with the package effectively.

---

### 4.4 `ggbreak-package.R` is nearly empty

**File:** `R/ggbreak-package.R`

The package-level documentation file contains only the bare minimum:

```r
#' @keywords internal
"_PACKAGE"
```

It could usefully include a `@description` summarising the package purpose, a `@references` link to the Analysis Function data visualisation guidance, and an acknowledgements note — making `?ggbreak` a useful entry point for new users rather than a dead end.

---

### 4.5 README: `scale_y_continuous()` warning should be more visible

**File:** `README.md`

The important instruction not to use `scale_y_continuous()` separately is currently embedded in paragraph text:

> *"For this reason, you should not use `scale_y_continuous()` to format your y-axis…"*

Given that this is a common mistake with significant consequences (the function will error or produce unexpected output), this should be promoted to a `> [!WARNING]` callout block — consistent with the `[!IMPORTANT]` and `[!CAUTION]` callouts already used in the README.

---

## 5. Future Development

### 5.1 X-axis break support

The function is currently restricted to the y-axis. An `axis` parameter (accepting `"y"` or `"x"`) would allow equivalent break symbols on horizontal axes — useful for charts with a truncated x-axis (e.g., time series that do not start at the epoch). The coordinate calculation logic would need adapting for horizontal orientation, and `scale_x_continuous()` / `coord_cartesian(xlim = ...)` would be used in place of their y-axis equivalents.

---

### 5.2 Multiple breaks (vectorised `break_at`)

`break_at` currently accepts only a length-1 scalar. Supporting a numeric vector (e.g., `break_at = c(0.3, 0.8)`) would allow multi-segment axes — useful for charts where values cluster in two distant ranges. This would require refactoring the coordinate calculation and `annotate("segment", ...)` calls into a loop or `lapply` over each break position.

---

### 5.3 Expand chart type support

The hard restriction to `geom_line` (which throws an error for all other geom types) could be relaxed. A y-axis break is conceptually valid for:

- **Scatter plots** — where y values cluster well above zero.

Options to consider:

- Replace the error with a **warning**, leaving the decision to the user.
- Accept a `chart_type` parameter that opts the user in to non-line support explicitly.
- Maintain a broader allowlist (e.g., `c("line", "point")`).

---

### 5.4 Implement the break symbol as a ggplot2 `Geom` (`ggproto`)

Currently, `add_break_symbol()` post-processes an existing built plot by appending `annotate()` and scale layers. A more idiomatic ggplot2 approach would implement the break symbol as a proper `Geom` class via `ggproto`, so it can be added with `+` directly in a ggplot2 pipeline:

```r
ggplot(df, aes(x, y)) +
  geom_line() +
  geom_break(break_at = 0.4)   # hypothetical
```

The advantages of a `ggproto` approach include:

- Full compatibility with faceting (`facet_wrap`, `facet_grid`).
- Correct handling of coordinate system transforms.
- Natural participation in ggplot2's rendering pipeline and theming.
- No need to call `ggplot_build()` manually — the geom receives the computed panel parameters from the renderer.

This is a significant rewrite but would make the package far more composable and robust.

---

### 5.5 Add a `testthat` unit test suite

There is currently no `tests/` directory. A test suite using `{testthat}` and `{vdiffr}` (for visual regression testing of plot output) would:

- Catch regressions in break symbol position when internal logic changes.
- Validate all input-validation paths (each `cli_abort()` call).
- Test the `y_breaks` / `y_limits` auto-calculation logic for edge cases.
- Allow safe refactoring with confidence.

A recommended minimal structure:

```
tests/
  testthat/
    test-add_break_symbol.R   # input validation and output structure
    test-utils.R              # helper functions (once extracted)
    _snaps/                   # vdiffr visual snapshots
```

---

### 5.6 GitHub Actions CI for `R CMD check`

No CI workflow is currently present in the repository. A standard `r-lib/actions` workflow running `R CMD check` on push and pull requests would:

- Catch package-level errors and `NOTE`s early.
- Provide a passing-checks badge for the README.
- Ensure the package is installable on multiple R versions and operating systems.

A minimal workflow file at `.github/workflows/R-CMD-check.yaml` using `r-lib/actions/setup-r-dependencies` is straightforward to set up.

---

### 5.7 `pkgdown` documentation site

A `pkgdown` site would render the `@examples` with live plot images, display the full function reference, and make the package much easier to evaluate for new users. The existing README SVG images and structured examples provide a good foundation. This pairs well with GitHub Actions CI — a deploy step can publish the site to GitHub Pages automatically on each push to `main`.

---

## Prioritised Action List

| Priority   | Area          | Action                                                                     |
| ---------- | ------------- | -------------------------------------------------------------------------- |
| 🔴 High   | Bug           | Fix `y_labels` validation to match documented behaviour (Bug 1.1)          |
| 🔴 High   | Bug           | Fix `{.var y_limits}` → `{.var break_style}` in error message (Bug 1.2)    |
| 🔴 High   | Performance   | Cache `ggplot_build()` result — call once, not three/four times (§2.1)     |
| 🟡 Medium | Code design   | Extract `ith_geom`, `is_waive`, `between` to `utils.R` (§3.1)              |
| 🟡 Medium | Code design   | Replace `y_origin_override` reassignment with `y_labels_out` (§3.2)        |
| 🟡 Medium | Code design   | Add `colour` to `break_style` list; remove hard-coded hex (§3.6)           |
| 🟡 Medium | Code design   | Replace `sapply` with `vapply` in `break_style` check (§3.3)               |
| 🟡 Medium | Code design   | Replace `import(scales)` with selective `importFrom()` (§3.4)              |
| 🟡 Medium | Documentation | Fix grammar error in `@details` (Bug 1.3)                                  |
| 🟡 Medium | Documentation | Replace `mtcars` `@examples` with trend-line example (§4.2)                |
| 🟡 Medium | Documentation | Add `@seealso` section (§4.3)                                              |
| 🟢 Low    | Code design   | Clarify `coord_cartesian` + `scale_y_continuous(limits)` redundancy (§3.5) |
| 🟢 Low    | Documentation | Expand `ggbreak-package.R` with description and references (§4.4)          |
| 🟢 Low    | Documentation | Promote `scale_y_continuous()` warning to callout in README (§4.5)         |
| 🟢 Low    | Future dev    | Add `testthat` / `vdiffr` test suite (§5.5)                                |
| 🟢 Low    | Future dev    | Add GitHub Actions `R CMD check` CI (§5.6)                                 |
| ⚪ Backlog | Future dev    | X-axis break support (§5.1)                                                |
| ⚪ Backlog | Future dev    | Multiple breaks — vectorised `break_at` (§5.2)                             |
| ⚪ Backlog | Future dev    | Expand beyond `geom_line` (§5.3)                                           |
| ⚪ Backlog | Future dev    | Reimplement as `ggproto` Geom (§5.4)                                       |
| ⚪ Backlog | Future dev    | `pkgdown` documentation site (§5.7)                                        |