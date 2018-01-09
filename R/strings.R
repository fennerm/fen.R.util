#' Grab all the numbers from a list as a string
#' @param x A string
#' @return Numeric vector
#' @export
extract_numeric <- function(x) {
  as.numeric(unlist(regmatches(x, gregexpr("[[:digit:]]+", x))))
}
