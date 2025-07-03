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
#' @importFrom cli cli_abort
#' @importFrom ggplot2 is_ggplot
#' @importFrom ggplot2 ggplot_build
#' @importFrom ggplot2 annotate
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom ggplot2 coord_cartesian
#' @importFrom ggplot2 waiver
#' @importFrom ggcheck get_geoms
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
    cli::cli_abort("{.var plot} {.strong must} be a {.cls gg} or {.cls ggplot} object.")
  }

  # Extract chart type:
  plot_type <- ggcheck::get_geoms(p)
  is_line <- plot_type == "line"

  # Function to check if argument has been waived:
  is_waive <- function(x) inherits(x, "waiver")

  # Check `plot` is a line chart:
  if (!is_line) {
    cli::cli_abort(
      c(
        "!" = "{.fn add_break_symbol} will {.strong not} add a break symbol to a {plot_type} chart.",
        "i" = "{.var plot} {.strong must} be a line chart."
      )
    )
  }

  # Check `break_at` is numeric of length 1:
  if (!is.numeric(break_at) || length(break_at) != 1) {
    cli::cli_abort("{.var break_at} {.strong must} be a numeric vector of length 1.")
  }

  # Check `y_breaks` is a numeric vector:
  if (!is_waive(y_breaks)) {
    if (!is.numeric(y_breaks)) {
      cli::cli_abort("{.var y_breaks} {.strong must} be a numeric vector.")
    }
  }

  # Check `y_label` is of format...
  # scales::label_*

  # Check `y_limits` is a numeric vector of length 2:
  if (!is.null(y_limits)) {
    if (!is.numeric(y_limits) || length(y_limits) != 2) {
      cli::cli_abort("{.var y_limits} {.strong must} be a numeric vector of length 2: {.code y_limits = c(ymin, ymax)}.")
    }
  }

  # Check `y_origin_override` is a numeric scalar (if not NULL):
  if (!is.null(y_origin_override)) {
    if (!is.numeric(y_origin_override)) {
      cli::cli_abort("{.var y_origin_override} {.strong must} be a numeric (default: NULL).")
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
    cli::cli_abort("Each element in {.var y_limits} {.strong must} be a numeric.")
  }

  # Extract height, width and linewidth from list:
  height <- break_style$height
  width <- break_style$width
  linewidth <- break_style$linewidth

  # Extract xmin and xmax from the plot:
  x_p_range <- ggplot2::ggplot_build(p)$layout$panel_params[[1]]$x.range
  x_p_min <- min(x_p_range, na.rm = TRUE)
  x_p_max <- max(x_p_range, na.rm = TRUE)

  # Extract ymin and ymax from the plot:
  y_p_range <- ggplot2::ggplot_build(p)$layout$panel_params[[1]]$y.range
  y_p_min <- min(y_p_range, na.rm = TRUE)
  y_p_max <- max(y_p_range, na.rm = TRUE)

  # Extract ymin and ymax from the data:
  y_d_range <- range(ggplot2::ggplot_build(p)$data[[1]]$y)
  y_d_min <- min(y_d_range)
  y_d_max <- max(y_d_range)

  # If user has specified `y_limits` then use min and max supplied over data or plot:
  if (is.null(y_limits)) {
    y_min <- min(c(c(y_d_min, y_p_min)))
    y_max <- max(c(y_d_max, y_p_max))
  } else {
    y_min <- min(y_limits)
    y_max <- max(y_limits)
  }

  # Create function to check value falls within a defined range:
  between <- function(x, left, right) {
    x >= left & x <= right
  }

  # Check break_at is inside the range of the y variable in the underlying data:
  if (between(break_at,
              y_d_min,
              y_d_max)) {
    cli::cli_abort("{.var break_at} must lie outside the range of the underlying chart data.")
  }

  # y_breaks must be numeric vector (or coercible)
  breaks <- y_breaks

  # If override is provided, replace the first break value
  if (!is.null(y_origin_override)) {
    if (length(breaks) < 1) stop("No breaks to override.")
    breaks[1] <- y_origin_override
  }

  # Now apply label formatting
  if (is.function(y_labels)) {
    y_origin_override <- y_labels(breaks)
  } else if (identical(y_labels, waiver())) {
    y_origin_override <- breaks
  } else {
    # user supplied vector of labels (character or numeric)
    y_origin_override <- y_labels
  }

  # Round minimum and maximum x values in the plot down to nearest integer:
  x_p_min <- floor(x_p_min)
  x_p_max <- floor(x_p_max) # ceiling?

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
      x = x_p_min, # origin should be 0
      xend = x_p_min, # origin should be 0
      y = ystart,
      yend = y_max,
      linewidth = linewidth,
      colour = "#3D3D3D"
    ) +

    # Vertical line under break symbol:
    ggplot2::annotate(
      "segment",
      x = x_p_min,
      xend = x_p_min,
      y = y_min,
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
      labels = y_origin_override,
      limits = y_limits
    ) +

    ggplot2::coord_cartesian(ylim = y_limits)

}
