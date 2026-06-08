#' Add break symbol to the y-axis of a ggplot2 line chart
#'
#' Adds a visual break symbol (two diagonal parallel lines) to indicate
#' a broken or truncated y-axis in a ggplot2 line chart. Useful to highlight
#' that the y-axis does not start at zero or a natural baseline.
#'
#' @param plot A ggplot2 plot object to which the break symbol and axis
#'   modifications will be added.
#' @param break_at Numeric scalar specifying the y-axis coordinate at which
#'   to place the break symbol.
#' @param y_breaks Numeric vector or \code{waiver()} specifying the breaks
#'   on the y-axis. Passed to \code{\link[ggplot2]{scale_y_continuous}(breaks =)}.
#'   Defaults to \code{waiver()}, letting ggplot2 determine breaks automatically.
#' @param y_labels Either a vector of labels corresponding to \code{y_breaks},
#'   a labeling function (e.g., from \code{\link[scales]{label_percent}}), or
#'   \code{waiver()}. Passed to
#'   \code{\link[ggplot2]{scale_y_continuous}(labels=)}.
#' @param y_limits Numeric vector of length two specifying the limits of the
#'   y-axis. Passed to \code{\link[ggplot2]{scale_y_continuous}(limits =)}.
#'   Defaults to \code{NULL}, which lets ggplot2 use default limits.
#' @param break_style A named list controlling the break symbol appearance:
#'   \describe{
#'     \item{height}{Numeric scalar indicating the vertical size of the break symbol
#'       (default 1).}
#'     \item{width}{Numeric scalar indicating the horizontal size of the break symbol
#'       (default 1).}
#'     \item{linewidth}{Numeric scalar controlling the width of the break symbol
#'       (default 0.5).}
#'   }
#' @param y_origin_override Optional numeric value to replace the first
#'   y-axis value before applying \code{y_labels}. This allows customisation of
#'   the label under the break symbol (for example, setting \code{0.4} to appear
#'   as \code{0}).
#'
#' @return A modified ggplot2 plot object with the y-axis break symbol and
#'   adjusted axis scale applied.
#'
#' @details
#' The function adds a vertical dark intersected by two diagonal parallel lines on the
#' left hand side of the plot at the specified \code{break_at} y-value, visually indicating
#' that the y-axis has been broken or truncated. It updates the y-axis breaks, labels, and
#' limits based on the provided arguments.
#'
#' The \code{y_labels} argument can be a vector of labels, a formatting function
#' (e.g., \code{scales::label_percent()}), or \code{waiver()} to use default labels.
#' If \code{y_origin_override} is supplied, the first y-axis value is replaced
#' numerically before the labelling function is applied, ensuring consistent formatting.
#'
#' Note that if you want ticks to appear outside the visible \code{y_limits},
#' consider using \code{\link[ggplot2]{coord_cartesian}(ylim = ...)} instead of
#' setting \code{limits} in \code{scale_y_continuous()}, as the latter clips ticks
#' outside the range.
#'
#' @importFrom stats median
#' @importFrom cli cli_abort
#' @importFrom ggplot2 is_ggplot
#' @importFrom ggplot2 ggplot_build
#' @importFrom ggplot2 annotate
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom ggplot2 coord_cartesian
#' @importFrom ggplot2 waiver
#' @import scales
#'
#' @examples
#' library(ggplot2)
#' library(scales)
#'
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_line()
#'
#' # Simple break symbol with default breaks and limits:
#' add_break_symbol(p, break_at = 10)
#'
#' # Custom breaks, labels, limits, and override first label to zero:
#' add_break_symbol(
#'   p,
#'   break_at = 7.5,
#'   y_breaks = seq(5, 35, 5),
#'   y_labels = label_number(accuracy = 1),
#'   y_limits = c(5, 35),
#'   y_origin_override = 0
#'  )
#'
#' @export
add_break_symbol <- function(
    plot,
    break_at,
    y_breaks = waiver(),
    y_labels = waiver(),
    y_limits = NULL,
    y_origin_override = NULL,
    break_style = list(height = 1, width = 1, linewidth = 0.5)
) {

  # Check `plot` is a gg or ggplot object:
  if (!ggplot2::is_ggplot(plot)) {
    cli::cli_abort("{.var plot} must be a {.cls gg} or {.cls ggplot} object.")
  }

  # Get number of layers (geoms) in `plot`:
  n_layers <- length(plot$layers)

  # Create function to extract all layers (geoms) in `plot`:
  # From RStudio's {ggbcheck} package.
  ith_geom <- function(p, i) {
    geom <- class(p$layers[[i]]$geom)[1]
    gsub("geom", "", tolower(geom))
  }

  # Extract chart type:
  plot_type <- vapply(seq_len(n_layers), ith_geom, character(1), p = plot)

  # Is there a line geom in the chart?
  is_line <- any(plot_type %in% "line")

  # Check `plot` is a line chart:
  if (!is_line) {
    cli::cli_abort(
      c(
        "!" = "{.fn add_break_symbol} will {.strong not} add a break symbol to a {plot_type} chart.",
        "i" = "{.var plot} must be a line chart."
      )
    )
  }

  # Check `break_at` is numeric of length 1:
  if (!is.numeric(break_at) || length(break_at) != 1) {
    cli::cli_abort("{.var break_at} must be a numeric vector of length 1.")
  }

  # Function to check if argument has been waived:
  is_waive <- function(x) inherits(x, "waiver")

  # Check `y_breaks` is a numeric vector:
  if (!is_waive(y_breaks)) {
    if (!is.numeric(y_breaks)) {
      cli::cli_abort("{.var y_breaks} must be a numeric vector.")
    }
  }

  # Check `y_label` is a function
  if (!is_waive(y_labels)) {
    if (!is.function(y_labels)) {
      cli::cli_abort(c(
        x = "{.var y_labels} must be a labeller function from the {.pkg scales} package.",
        i = "For example: {.fn scales::label_comma} or {.fn scales::label_percent}."
      ))
    }
  }

  # Check `y_limits` is a numeric vector of length 2:
  if (!is.null(y_limits)) {
    if (!is.numeric(y_limits) || length(y_limits) != 2) {
      cli::cli_abort("{.var y_limits} must be a numeric vector of length 2: {.code y_limits = c(ymin, ymax)}.")
    }
  }

  # Check `y_origin_override` is a numeric scalar (if not NULL):
  if (!is.null(y_origin_override)) {
    if (!is.numeric(y_origin_override)) {
      cli::cli_abort("{.var y_origin_override} must be a numeric (default: NULL).")
    }
  }

  style_keys <- c("height", "width", "linewidth")

  # Check `break_style` is a named list containing the break symbol formatting info:
  if (!is.list(break_style) || !all(style_keys %in% names(break_style))) {
    cli::cli_abort(
      c(
        "!" = "{.var break_style} must be a list defining the height, width and linewidth of the break symbol.}.",
        "i" = "Default: {.code break_style = list(height = 1, width = 1, linewidth = 0.5)}"
      )
    )
  }

  # Check all elements in `break_style` are numeric:
  if (any(!sapply(break_style[style_keys], is.numeric))) {
    cli::cli_abort("Each element in {.var break_style} must be a numeric.")
  }

  # Extract height, width and linewidth from list:
  height <- break_style$height
  width <- break_style$width
  linewidth <- break_style$linewidth

  # Extract xmin and xmax from the `plot`:
  x_p_range <- ggplot2::ggplot_build(plot)$layout$panel_params[[1]]$x.range
  x_p_min <- min(x_p_range, na.rm = TRUE)
  x_p_max <- max(x_p_range, na.rm = TRUE)

  # Extract ymin and ymax from the `plot`:
  y_p_range <- ggplot2::ggplot_build(plot)$layout$panel_params[[1]]$y.range
  y_p_min <- min(y_p_range, na.rm = TRUE)
  y_p_max <- max(y_p_range, na.rm = TRUE)

  # Extract ymin and ymax from the data:
  y_d_range <- range(ggplot2::ggplot_build(plot)$data[[1]]$y)
  y_d_min <- min(y_d_range)
  y_d_max <- max(y_d_range)

  # Create function to check value falls within a defined range:
  between <- function(x, left, right) {
    x >= left & x <= right
  }

  # Check `break_at` is outside the range of the plot data:
  if (between(break_at,
              y_d_min,
              y_d_max)) {
    cli::cli_abort("{.var break_at} must lie outside the range of the underlying chart data.")
  }

  # If `y_breaks` and `y_limits` are not supplied by the user, work out sensible values from the plot data and break symbol coordinate:
  if (is_waive(y_breaks) && is.null(y_limits)) {

    # Get y-axis breaks from plot:
    y_p_breaks <- ggplot2::ggplot_build(plot)$layout$panel_params[[1]]$y$breaks

    # Calculate differences between y-axis breaks:
    y_break_diff <- diff(y_p_breaks)

    # Get spacing between y-axis breaks:
    y_break_spacing <- stats::median(y_break_diff, na.rm = TRUE)

    # Find appropriate break points
    # Start from a round number below `break_at` (better to use `break_at` which has to be lower than the minimum value in the plot data):
    start_break <- floor((break_at - y_break_spacing)  / y_break_spacing) * y_break_spacing # adds in some buffer below the break symbol equal to the y-axis spacing
    # End at a round number above the maximum value in the plot data:
    end_break <- ceiling(y_d_max / y_break_spacing) * y_break_spacing

    # Generate new breaks:
    y_breaks <- seq(start_break, end_break, by = y_break_spacing)

    # Create new limits:
    y_limits <- c(start_break, end_break)

  }

  # If label style is a function and label override is not provided:
  if (is.function(y_labels) && is.null(y_origin_override)) {

    # User supplied vector of labels:
    y_origin_override <- y_labels

    # If label style is a function and label override is provided:
  } else if (is.function(y_labels) && !is.null(y_origin_override)) {

    y_labels_fmt <- y_breaks
    y_labels_fmt[1] <- y_origin_override
    y_origin_override <- y_labels(y_labels_fmt)

    # If label style is waived use the breaks (either user-supplied or derived):
  } else if (is_waive(y_labels)) {

    y_origin_override <- y_breaks

  }

  # Round minimum and maximum x values in the plot down to nearest integer:
  x_p_min <- floor(x_p_min)
  x_p_max <- floor(x_p_max)

  # Define coordinates for the break symbol:
  ydiff <- ((y_d_max - break_at) * 0.02) * height
  yend <- break_at - ydiff
  ystart <- break_at + ydiff

  xdiff <- ((x_p_max - x_p_min) * 0.015) * width
  xend <- x_p_min + xdiff
  xstart <- x_p_min - xdiff

  # Take plot and add:
  plot +

    # Vertical line above break symbol:
    ggplot2::annotate(
      "segment",
      x = x_p_min,
      xend = x_p_min,
      y = ystart,
      yend = y_limits[2],
      linewidth = linewidth,
      colour = "#3D3D3D"
    ) +

    # Vertical line under break symbol:
    ggplot2::annotate(
      "segment",
      x = x_p_min,
      xend = x_p_min,
      y = y_limits[1],
      yend = yend,
      linewidth = linewidth,
      colour = "#3D3D3D"
    ) +

    # # Lower diagonal break symbol line:
    ggplot2::annotate(
      "segment",
      x = xstart,
      xend = xend,
      y = yend - ydiff,
      yend = yend + ydiff,
      linewidth = linewidth,
      colour = "#3D3D3D"
    ) +

    # # Upper diagonal break symbol line:
    ggplot2::annotate(
      "segment",
      x = xstart,
      xend = xend,
      y = ystart - ydiff,
      yend = ystart + ydiff,
      linewidth = linewidth,
      colour = "#3D3D3D"
    ) +

    # Define new y-axis scale:
    ggplot2::scale_y_continuous(
      breaks = y_breaks,
      expand = ggplot2::expansion(mult = c(0, 0.02)),
      labels = y_origin_override,
      limits = y_limits
    ) +

    ggplot2::coord_cartesian(ylim = y_limits)

}
