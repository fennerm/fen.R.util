#' @export
ulapply <- function(...) {
    unlist(lapply(...))
}

#' Apply a function across elements of a list
#' @export
apply_across <- function(lst, func, ...) {
  len <- nrow(lst[[1]])
  if (is.null(len)) {
    len <- length(lst[[1]])
  }
  lapply(1:len, function(i) {
           func(ulapply(lst, "[[", i))
  })
}
