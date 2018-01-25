context("test_tables.R")

column_names <- c("a", "b")
testmat <- matrix(c(1:4), ncol = 2, dimnames = list(NULL, column_names))
extra_columns <- c("c", "d")
expected <- cbind(testmat, 0, 0)
colnames(expected) <- c(column_names, extra_columns)

test_that("add_missing_columns works on matrices", {
            actual <- add_missing_columns(testmat, extra_columns, fill = 0)
            expect_equal(actual, expected)
})

testmat <- as.data.frame(testmat)
expected <- as.data.frame(expected)
test_that("add_missing_columns works on data.frames", {
            actual <- add_missing_columns(testmat, extra_columns, fill = 0)
            expect_equal(actual, expected)
})


test_that("add_missing_columns handles does nothing if not missing", {
            actual <- add_missing_columns(testmat, "a")
            expect_equal(actual, testmat)
})
