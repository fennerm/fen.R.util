#' Calculate the mode of a vector.
#'
#' Returns the first value in case of ties
#' @export
mode <- function(x) {
  ux <- unique(x)
  mode <- ux[which.max(tabulate(match(x, ux)))]
  mode
}
