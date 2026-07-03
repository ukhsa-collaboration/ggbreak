#' Extract the i-th geom from a ggplot object:
#' @param p a ggplot or gg object
#' @param i the index of the layer to extract
#' @export
ith_geom <- function(p, i) {
  geom <- class(p$layers[[i]]$geom)[1]
  gsub("geom", "", tolower(geom))
}

#' Check if argument has been waived:
#' @param x an object to check
#' @export
is_waive <- function(x) inherits(x, "waiver")

#' Check value falls within a defined range:
#' @param x a numeric value to check
#' @param left the lower bound of the range
#' @param right the upper bound of the range
#' @export
between <- function(x, left, right) {
  x >= left & x <= right
}