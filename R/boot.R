## Utility code related to bootstrapping

#' Bootstrap difference in statistic across two groups
#'
#' See:
#' https://stats.stackexchange.com/questions/136661
#' Also:
#' Efron's and Tibshirani's - intro to the bootstrap, page 223
#' @param mutation_table tibble; A mutation table nested by a group
#' @param statistic function; A valid bootstrap statistic
#' @param reps integer; Number of bootstrap replicates
#' @importFrom magrittr "%>%"
#' @importFrom dplyr summarize mutate
#' @importFrom purrr map map_dbl
#' @import dplyr
#' @return A p-value
#' @export
bootstrap_test <- function(tbl, statistic, reps = 1e5) {
  combined_statistic <- tbl %>%
    unnest %>%
    summarize(statistic(., indices = 1:nrow(.))[1]) %>%
    as.numeric

  # Bootstrap null distributions for each group
  tbl <- tbl %>%
    mutate(obs = map_dbl(data, ~statistic(., indices = 1:nrow(.))[1])) %>%
    mutate(boot_null = map(
        data,
        bootstrap_centered_null,
        statistic = statistic,
        center = combined_statistic,
        reps = reps))

  observed_diff <- abs(tbl$obs[1] - tbl$obs[2])

  boot_diff <- abs(tbl$boot_null[[1]] - tbl$boot_null[[2]])

  p <- (length(which(boot_diff >= observed_diff)) + 1) / length(boot_diff)
  p
}


#' Apply a two group statistical test to each group in a nested tibble
#' @importFrom dplyr group_vars
#' @export
boot_compare_all <- function(
  grouped_mutation_table,
  test,
  within = NULL,
  reps = 1e5) {
  grouping <- group_vars(grouped_mutation_table)
  p_values <- tibble_combn(grouped_mutation_table,
    set_size = 2,
    func = test,
    func_type = summarize,
    grouped_input = TRUE,
    within = within,
    reps = reps)
  output_table <- data.frame(names(p_values), unlist(unname(p_values)))
  colnames(output_table) <- c(grouping, "p_value")
  output_table
}


#' Bootstrap a statistic across a table and produce bca ci and quantiles
#' @param tbl A tibble
#' @param statistic function; A bootstrap statistic
#' @param reps integer; Number of bootstrap replicates
#' @importFrom dplyr mutate
#' @importFrom tidyr unnest
#' @importFrom purrr map map_dbl
#' @importFrom magrittr "%>%"
#' @importFrom boot boot
#' @export
boot_quantiles <- function(tbl, statistic, reps = 1e5) {
  tbl %>%
    mutate(stat = data %>%
      map_dbl(~statistic(., 1:nrow(.))[1])) %>%
    mutate(boot_stat = data %>%
      map(boot, statistic = statistic, R = reps)) %>%
    mutate(quantiles = boot_stat %>%
      map(calc_bca_quantiles)) %>%
    select(1, stat, quantiles) %>%
    unnest
}


#' Center a distribution at a given mean value
#' @param d numeric; the distribution
#' @param target numeric; the new mean
#' @return The centered distribution
center_distribution <- function(d, target) {
  d <- unlist(d)
  d - mean(d) + target
}


#' Bootstrap the null distribution for a statistic centered at a given value
#'
#' This function generates a distribution for the null hypothesis that two
#' groups do not differ in some statistic. For example if we were interested in
#' whether two samples had a different mean; this function would produce a
#' bootstrapped sample for one group centered at the actual mean across both
#' groups. This ensures that the bootstrapped distribution corresponds to the
#' actual null hypothesis. Explained better here:
#' https://stats.stackexchange.com/questions/136661
#' @importFrom boot boot
#' @importFrom broom tidy
#' @importFrom magrittr "%>%" set_colnames
#' @importFrom tibble as_tibble
bootstrap_centered_null <- function(
  mutation_table, statistic, center, ..., reps = 1e5) {
  boot_sample <- boot(mutation_table, statistic = statistic, R = reps, ...)$t
  colnames(boot_sample) <- c("value", "variance")
  centered_boot <- center_distribution(boot_sample[, "value"], target = center)
  centered_boot
}


#' Calculate quantiles and bias corrected confidence intervals for a boot object
#' @importFrom boot boot.ci
#' @importFrom tibble tibble
calc_bca_quantiles <- function(boot_object) {
  ci <- boot.ci(boot_object)$bca[4:5]
  quantiles <- quantile(boot_object$t[, 1], probs = c(0.25, 0.75))
  tibble(ci1 = ci[1], q25 = quantiles[1], q75 = quantiles[2], ci2 = ci[2])
}
