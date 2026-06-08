#' Extract the i-th geom from a ggplot object:
#' @export
ith_geom <- function(p, i) {
  geom <- class(p$layers[[i]]$geom)[1]
  gsub("geom", "", tolower(geom))
}

#' Check if argument has been waived:
#' @export
is_waive <- function(x) inherits(x, "waiver")

#' Check value falls within a defined range:
#' @export
between <- function(x, left, right) {
  x >= left & x <= right
}