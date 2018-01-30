context("Scripting utilities")

test_that("script_name works", {
              relpath <- "../resources/script_name.R"
              abspath <- normalizePath(relpath)
              name <- system(relpath, intern=TRUE)
              name <- substr(name, 6, nchar(name) - 1)
              expect_equal(name, abspath)
})
