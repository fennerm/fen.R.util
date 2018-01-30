#' Add missing columns to a table
#' @param table A data.frame or matrix
#' @param column_names Character vector; List of column names to add
#' @param fill Value to fill the new columns
#' @return data.frame or matrix
#' @export
add_missing_columns <- function(table, column_names, fill = NA) {
  column_names <- unique(c(colnames(table), column_names))

  if (is.matrix(table)) {
    # Add missing column as NAs
    table <- table[, match(column_names, colnames(table))]

    # Name the extra columns
    colnames(table) <- column_names

    # Replace NAs if necessary
    if (!is.na(fill)) {
      table[is.na(table)] <- fill
    }
  } else if (is.data.frame(table)) {
    missing <- setdiff(column_names, colnames(table))
    table[, missing] <- fill
  }
  table
}

#' Replace rows in a data.frame by index
#' @param df data.frame
#' @param old_index Numeric; Index of the old row in df
#' @param new_row Vector; New row to add to df
#' @return data.frame
#' @export
replace_rows <- function(df, old_index, new_row) {
  # exclude rows from table
  new_df <- df[-old_index, ]

  pre_half <- df[1:(old_index[1] - 1), ]
  post_half <- df[(tail(old_index, n = 1) + 1):nrow(df), ]
  # replace with the new_row

  new.df <- rbind(pre_half, new_row, post_half)
  row.names(new.df) <- NULL
  new.df
}

#' Filter a data.frame to exclude all rows which are not in 'groups'
#' @param df A data.frame
#' @param by Character; A column name from 'df'
#' @param groups Factor; Vector of levels
#' @return A data.frame
#' @import dplyr
#' @export
select_groups <- function(df, by, groups) {
  grouping <- quo(df[, by])
  subset <- filter(df, !!grouping %in% groups)
  subset
}
