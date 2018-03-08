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

#' Apply a function across all combinations of levels of a tibble group
#' 
#' All extra parameters are passed to the input function
#' @param dat tbl_df A grouped tibble
#' @param set_size Number of levels in group to compare in each iteration
#' @param func function Function to apply to each set
#' @param func_type Type of dplyr operation, e.g do, summarize
#' @param grouped_input bool If TRUE, groups in 'dat' will be conserved in calls
#'                           to func
#' @param within character A column name from 'dat'. If given, comparisons will
#'                         only be made between values which share the same
#'                         values in the within column.
#' @return A list of results from 'func'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr group_by filter ungroup group_vars
tibble_combn <- function(dat, set_size, func, func_type = summarize,
  grouped_input = FALSE, within = NULL, ...) {
  if (!is.null(within)) {
    splitdat <- split_table(dat, by = within)
  } else {
    splitdat <- list(dat)
  }

  results <- lapply(splitdat, function(dat_within) {
    grouping <- group_vars(dat_within)
    group_levels <- unlist(unique(dat_within[, grouping]))

    pairwise_comparisons <- combn(group_levels, set_size)

    results_within <- apply(pairwise_comparisons, 2, function(x) {
      # Extract the target groups from the tibble
      result <- dat_within %>%
        filter(!!as.name(grouping) %in% x) %>%
        ungroup

      # If 'func' requires grouped input, regroup 'dat_within'
      if (grouped_input) {
        # We need to ungroup then regroup because otherwise dplyr::do would
        # remove the grouping from the input
        result <- result %>%
          func_type(func(group_by(., !!as.name(grouping)), ...))
      } else {
        result <- result %>%
          func_type(func(., ...))
      }
      result})

    # Add names to the outputs
    names(results_within) <- apply(
      pairwise_comparisons, 2, paste0, collapse = "_by_")

    if (!is.null(within)) {
      within_group <- unique(dat_within[, within])
      within_section <- paste0("_within_", within_group)
      names(results_within) <- paste0(names(results_within), within_section)
    }

    results_within
  })
  results <- unlist_preserving_names(results, recursive = FALSE)
  results
}

#' Unlist without changing the names of the inner objects
#' @export
unlist_preserving_names <- function(x, recursive = FALSE) {
  existing_names <- ulapply(x, names)
  x <- unlist(x, recursive = recursive)
  names(x) <- existing_names
  x
}
