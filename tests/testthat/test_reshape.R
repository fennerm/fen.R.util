context("Tables")
library(dplyr)

grouped_tibble <- tibble(group = c("a", "b", "c"), value = c(1:3)) %>%
  group_by(group)


tibble_summarize_prod <- function(dat, add=0) {
  dat %>% summarize(prod(unlist(value)) + add) %>% unlist
}

test_that("tibble_combn works with simple summary function", {
  result <- tibble_combn(
    dat = grouped_tibble,
    set_size = 2,
    func = tibble_summarize_prod,
    func_type = summarize,
    grouped_input = FALSE
    )
  expect_is(result, "list")
  expect_is(result[[1]], "tbl_df")
  expect_equivalent(unlist(result), c(2, 3, 6))
})

test_that("tibble_combn works with extra params", {
  result <- tibble_combn(
    dat = grouped_tibble,
    set_size = 2,
    func = tibble_summarize_prod,
    func_type = summarize,
    grouped_input = FALSE,
    add = 1
    )
  expect_equal(names(result), c("a_by_b", "a_by_c", "b_by_c"))
  expect_is(result, "list")
  expect_is(result[[1]], "tbl_df")
  expect_equivalent(unlist(result), c(3, 4, 7))
})

tibble_do_add <- function(dat) {
  result <- as.data.frame(dat$value + 1)
  result
}
test_that("tibble_combn, works with simple do function", {
  result <- tibble_combn(
    dat = grouped_tibble,
    set_size = 2,
    func = tibble_do_add,
    func_type = do,
    grouped_input = FALSE
    )
  expected_result <- list(
    as.data.frame(c(2, 3)),
    as.data.frame(c(2, 4)),
    as.data.frame(c(3, 4)))
  expect_is(result, "list")
  expect_is(result[[1]], "data.frame")
  expect_equivalent(result, expected_result)
})

tibble_grouped_add <- function(dat) {
  # Add 1 to the first group value
  grouping <- group_vars(dat)
  group_levels <- unique(unlist(dat[, grouping]))
  result <- dat %>%
    filter(group == group_levels[1]) %>%
    summarize(value + 1) %>%
    select(2) %>%
    unlist
  result
}

test_that("tibble_combn works with grouped_input", {
  result <- tibble_combn(
    dat = grouped_tibble,
    set_size = 2,
    func = tibble_grouped_add,
    func_type = summarize,
    grouped_input = TRUE
    )
  expect_is(result, "list")
  expect_is(result[[1]], "tbl_df")
  expect_equivalent(unlist(result), c(2, 2, 3))
})

multigrouped_tibble <- tibble(
    group1 = c(rep("a", 3), rep("b", 3)),
    group2 = c(rep(c("a", "b", "c"), 2)),
    value = c(1:6)) %>%
  group_by(group2)

test_that("tibble_combn works with within parameter", {
  result <- tibble_combn(
    dat = multigrouped_tibble,
    set_size = 2,
    func = tibble_summarize_prod,
    func_type = summarize,
    grouped_input = FALSE,
    within = "group1"
    )
  expected_names <- c(
    "a_by_b_within_a", "a_by_c_within_a", "b_by_c_within_a", "a_by_b_within_b",
    "a_by_c_within_b", "b_by_c_within_b")
  expect_is(result, "list")
  expect_is(result[[1]], "tbl_df")
  expect_equal(names(result), expected_names)
  expect_equivalent(unlist(result), c(2, 3, 6, 20, 24, 30))
})


test_that("unlist_preserving_names works", {
  lst <- list(a = list(x = 1, y = 2), b = list(z = 0))
  result <- names(unlist_preserving_names(lst, recursive = FALSE))
  expect_equal(result, c("x", "y", "z"))
})
