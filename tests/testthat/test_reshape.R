context("Tables")
library(dplyr)

test_that("unlist_preserving_names works", {
  lst <- list(a = list(x = 1, y = 2), b = list(z = 0))
  result <- names(unlist_preserving_names(lst, recursive = FALSE))
  expect_equal(result, c("x", "y", "z"))
})
