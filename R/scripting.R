# Functions for use in R scripts

#' Get the path to the executing R script
#' @export
script_name <- function() {
    opts <- commandArgs(trailingOnly = FALSE)
    file_arg_name <- "--file="
    name <- sub(file_arg_name, "", opts[grep(file_arg_name, opts)])
    name <- normalizePath(name)
    name
}

#' Get the executing R script's directory path
#' @export
script_dir <- function() {
    dirname(script_name())
}
