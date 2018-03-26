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

#' Split a data.frame by a column
#'
#' @param df A data.frame
#' @param by A column name
#' @param levels A set of possible values in by. If NULL, the table is
#'               split on all unique v
#' @return A list of data.frames
#' @export
split_table <- function(df, by = NULL, levels = NULL) {
  if (is.null(by)) {
    dfs <- list(df)
  } else {
    if (is.null(levels)) {
      dfs <- split(df, df[, by])
    } else {
      per_level <- lapply(levels, function(x) which(df[, by] == x))
      dfs <- lapply(per_level, function(x) df[x, ])
    }
  }
  dfs
}


#' Split a nested tibble by a column
#'
#' @param tbl A nested tibble
#' @param by A column name within the nested tibble to split by
#' @return A list of nested tibbles
split_nested_tibble <- function(tbl, by) {
  grouping <- colnames(tbl)[1]
  if (!is.null(by)) {
    tbls <- tbl %>%
      unnest %>%
      split_table(by = by) %>%
      map(~group_by(., !!as.name(grouping))) %>%
      map(nest)
  } else {
    tbls <- list(tbl)
  }
  tbls
}

#' Apply a function across all combinations of groups in a nested tibble
#'
#' All extra parameters are passed to the input function
#' @param tbl A nested tibble
#' @param set_size Number of rows in dat to compare in each iteration
#' @param func function Function to apply to each set of rows
#' @param within character A column name from the inner nested tibbles in `tbl`.
#'                         If given, comparisons will only be made between rows
#'                         which share the same group in the within column.
#' @return A list of results from 'func'
#' @importFrom tibble as.tibble
#' @importFrom magrittr "%>%"
#' @importFrom dplyr group_by filter mutate bind_rows
#' @importFrom purrr map
#' @export
tibble_combn <- function(tbl, set_size, func, within = NULL, ...) {
  grouping <- colnames(tbl)[1]
  results <- tbl %>%
    split_nested_tibble(by = within) %>%
    # Apply the function to each within group separately
    map(function(x) {
      if (nrow(x) > 1) {
        combinations <- combn(unlist(x[, 1]), set_size, simplify = FALSE)
        func_output <- combinations %>%
          # Filter the table
          map(~filter(x, !!as.name(grouping) %in% .)) %>%
          # Apply the function
          map(function(x) func(x, ...))
        # Bind the group names with the function results
        output_table <- as.tibble(do.call("rbind", combinations)) %>%
          mutate(result = unlist(func_output))
      } else {
        output_table <- NULL
      }
      output_table
    }) %>%
    filter_null %>%
    bind_rows
  results
}

#' Unlist without changing the names of the inner objects
#' @importFrom purrr map
#' @export
unlist_preserving_names <- function(x, recursive = FALSE) {
  existing_names <- unlist(map(x, names))
  x <- unlist(x, recursive = recursive)
  names(x) <- existing_names
  x
}

#' Remove all NULL values from a list
#' @importFrom magrittr "%>%"
#' @importFrom purrr map_lgl
#' @export
filter_null <- function(x) {
  x[x %>% map_lgl(is.null)] <- NULL
  x
}
