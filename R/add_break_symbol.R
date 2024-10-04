#' Add break symbol to ggplot object
#'
#' @param p a ggplot object.
#' @param y a numeric vector (default: NULL). Position of the break symbol on the y-axis. If empty, placement of break symbol determined internally.
#' @param y_spacing a numeric vector (default: NULL).
#' @param y_origin a numeric vector (default: zero).
#' @param y_fmt a character vector (default: NULL). Character string: comma, percent, number, currency, etc.
#' @param y_fmt_args a list. Additional arguments (such as accuracy and prefix) which can be passed to `label_*()`.
#' @param height a numeric vector (default: 1). The gap in the break symbol.
#' @param width a numeric vector (default: 1). The width of the break symbol.
#' @param linewidth a numeric vector (default: 0.5). The line width of the break symbol.
#'
#' @import ggplot2
#' @import scales
#'
#' @return a ggplot object.
#' @export
#'
#' @author Si Maxwell (simon.maxwell@@ukhsa.gov.uk)
#'
#' @examples
#' # Load pkgs:
#' library(ggplot2)
#' library(scales)
#'
#' # Create data set:
#' df <- data.frame(x = 1:20, y = rnorm(20, mean = 12000, sd = 2000))
#'
#' # Create basic plot:
#' p <- ggplot(df, aes(x, y)) + geom_line()
#'
#' # Show plot:
#' p
#'
#' # Add break symbol to plot:
#' p + add_break_symbol(p)
#'
#' # Add break symbol and re-format y-axis:
#' p + add_break_symbol(p, y_fmt = "comma", y_fmt_args = list(accuracy = 0.1))
add_break_symbol <- function(p, y = NULL, y_spacing = NULL, y_origin = 0, y_fmt = NULL, y_fmt_args = list(), height = 1, width = 1, linewidth = 0.5) {

  # Check plot type from `p`:
  plot_type <- class(ggplot2::ggplot_build(p)$plot$layers[[1]]$geom)[1]

  # Stop if `p` is not a line chart:
  if (plot_type != "GeomLine") {
    stop("`add_break_symbol` should only be used for line charts.",
         call. = FALSE)
  }

  # Check if `y` is NULL and numeric:
  y_is_null <- is.null(y)
  y_is_num <- is.numeric(y)

  # Stop if `y` is not NULL or numeric:
  if (!y_is_null) {
    if (!y_is_num) {
      stop("`y` must be numeric or NULL (default).",
           call. = FALSE)
    }
  }

  # Stop if `y_spacing` is not NULL or numeric:
  if (!is.null(y_spacing)) {
    if (!is.numeric(y_spacing)) {
      stop("`y_spacing` must be numeric or NULL (default).",
           call. = FALSE)
    }
  }

  # Stop if `y_origin` is not numeric:
  if (!is.numeric(y_origin)) {
    stop("`y_origin` must be numeric.")
  }

  # Create vector of possible y scale formats:
  y_fmt_possible_labels <- c("bytes", "comma", "currency", "log", "math", "number", "number_auto", "ordinal", "parse", "percent", "pvalue", "scientific")

  # Stop if `y_fmt` is not a possible label_* function from the {scales} package:
  if (!is.null(y_fmt)) {
    if (!y_fmt %in% y_fmt_possible_labels) {
      stop("`y_fmt` must match one of the labels for continuous scales functions from the {scales} package",
           call. = FALSE)
    }
  }

  # Stop if `y_fmt_args` is not a list:
  if (!is.list(y_fmt_args)) {
    stop("`y_fmt_args` must be a list.",
         call. = FALSE)
  }

  # Stop if `height` is not numeric:
  if (!is.numeric(height)) {
    stop("`height` must be numeric.",
         call. = FALSE)
  }

  # Stop if `width` is not numeric:
  if (!is.numeric(width)) {
    stop("`width` must be numeric.",
         call. = FALSE)
  }

  # Stop if `linewidth` is not numeric:
  if (!is.numeric(linewidth)) {
    stop("`linewidth` must be numeric.",
         call. = FALSE)
  }

  # Extract min and max x axis values from the plot:
  x_var <- ggplot2::ggplot_build(p)$layout$panel_params[[1]]$x.range
  xmin <- min(x_var, na.rm = TRUE)
  xmax <- max(x_var, na.rm = TRUE)

  xmin <- floor(xmin)
  xmax <- floor(xmax)

  # Extract min and max y axis values from the plot's underlying data:
  y_var <- ggplot2::ggplot_build(p)$data[[1]]$y
  ymin <- min(y_var, na.rm = TRUE)
  ymax <- max(y_var, na.rm = TRUE)

  # Extract the y breaks from p and remove any NAs:
  y_breaks <- ggplot2::ggplot_build(p)$layout$panel_params[[1]]$y$breaks
  y_breaks <- y_breaks[!is.na(y_breaks)]

  # If `y_spacing` is not user specified then derive breaks from ggplot object:
  if (is.null(y_spacing)) {

    y_spacing <- unique(diff(y_breaks))

  }

  # Get the minimum y limit which includes all of the data:
  y_floor <- floor(ymin / y_spacing) * y_spacing

  # Get the maximum y limit which includes all of the data:
  y_ceiling <- ceiling(ymax / y_spacing) * y_spacing

  # To ensure there's enough space for the break symbol add a buffer to the bottom of the y-axis if needed:
  if ((ymin - y_floor) < y_spacing) {

    y_floor <- y_floor - y_spacing

  }

  # If `y` is NULL then work out a sensible value:
  if (y_is_null) {

    # Create sequence breaks spanning full range of data:
    new_breaks <- seq(from = y_floor,
                      to = y_ceiling,
                      by = y_spacing)

    # Work out position of break symbol from y-axis limits and default spacing:
    y <- new_breaks[1] + ((new_breaks[2] - new_breaks[1]) / 2)

  }

  # If `y` is a user-supplied value:
  if(!y_is_null) {

    # Create sequence breaks spanning full range of data:
    new_breaks <- seq(from = y_floor,
                      to = y_ceiling,
                      by = y_spacing)

    # Test for appropriate `y` value passed by the user.
    # This will throw an error if `y` is not low enough:
    if (y > ymin) {

      stop(paste0("Please provide a value for `y` that is less than ", ymin, ". Recommended range for `y` is ", new_breaks[1], " to ", new_breaks[2], "."))

    }

    # This will throw an error if `y` is too low:
    if (y < y_floor) {

      stop(paste0("Please provide a value for `y` that is greater than ", y_floor, ". Recommended range for `y` is ", new_breaks[1], " to ", new_breaks[2]))

    }

  }

  # Define coordinates for the break symbol:
  ydiff <- ((ymax - y) * 0.02) * height
  yend <- y - ydiff
  ystart <- y + ydiff

  xdiff <- ((xmax - xmin) * 0.015) * width
  xend <- xmin + xdiff
  xstart <- xmin - xdiff

  # Create a vector of the plot breaks:
  plot_breaks <- c(y_origin, new_breaks[-1])

  #
  if (length(y_fmt) == 0) {

    fmt_type <- plot_breaks

  } else {

    # Switch:
    # label_date/time options excluded as `y` should be an object of class <numeric>
    fmt_type <- switch(y_fmt,
                       "bytes" = do.call(scales::label_bytes, y_fmt_args)(plot_breaks),
                       "comma" = do.call(scales::label_comma, y_fmt_args)(plot_breaks),
                       "currency" = do.call(scales::label_currency, y_fmt_args)(plot_breaks),
                       "log" = do.call(scales::label_log, y_fmt_args)(plot_breaks),
                       "math" = do.call(scales::label_math, y_fmt_args)(plot_breaks),
                       "number" = do.call(scales::label_number, y_fmt_args)(plot_breaks),
                       "number_auto" = do.call(scales::label_number_auto, y_fmt_args)(plot_breaks),
                       "ordinal" = do.call(scales::label_ordinal, y_fmt_args)(plot_breaks),
                       "parse" = do.call(scales::label_parse, y_fmt_args)(plot_breaks),
                       "percent" = do.call(scales::label_percent, y_fmt_args)(plot_breaks),
                       "pvalue" = do.call(scales::label_pvalue, y_fmt_args)(plot_breaks),
                       "scientific" = do.call(scales::label_scientific, y_fmt_args)(plot_breaks),
                       stop("Invalid `y_fmt` specified."))

  }

  # Combine the break symbol annotations and new y-axis information:
  list(

    # Vertical line under break symbol:
    ggplot2::annotate("segment",
                      x = xmin,
                      xend = xmin,
                      y = y_floor,
                      yend = yend,
                      linewidth = linewidth,
                      colour = "#3D3D3D"),

    # Vertical line above break symbol:
    ggplot2::annotate("segment",
                      x = xmin,
                      xend = xmin,
                      y = ystart,
                      yend = y_ceiling,
                      linewidth = linewidth,
                      colour = "#3D3D3D"),

    # Lower break symbol line:
    ggplot2::annotate("segment",
                      x = xstart,
                      xend = xend,
                      y = yend - ydiff,
                      yend = yend + ydiff,
                      linewidth = linewidth,
                      colour = "#3D3D3D"),

    # Upper break symbol line:
    ggplot2::annotate("segment",
                      x = xstart,
                      xend = xend,
                      y = ystart - ydiff,
                      yend = ystart + ydiff,
                      linewidth = linewidth,
                      colour = "#3D3D3D"),

    # Define new y-axis scale:
    ggplot2::scale_y_continuous(breaks = new_breaks,
                                expand = ggplot2::expansion(mult = c(0, 0.02)),
                                labels = fmt_type,
                                limits = c(y_floor, y_ceiling))

  )

}
