#' Convert a list to a space separated string
#' @param lst A list
#' @return A string
#' @export
list_to_string <- function(lst) {
  paste(unlist(lst), collapse = " ")
}

#' Read a text file as a single string
#' @param filename File path
#' @return Character
#' @export
read_text <- function(filename) {
  readChar(filename, file.info(filename)$size)
}

#' Read a tab separated file
#' @param f File path
#' @return Data.frame
#' @export
read_tsv <- function(f) {
  read.csv(f, sep = "\t", stringsAsFactors = FALSE)
}

#' Write a table to file with added header using write.table
#' @param x The table
#' @param file File path to write to
#' @param header The header
#' @export
write_table_with_header <- function(x, file, header, ...) {
  cat(header, "\n", file = file)
  write.table(x, file, append = T, ...)
}
