
test_that("'make_increment_move' works with valid input - has internal", {
    x <- matrix(1:21, nr = 3)
    ans_obtained <- make_increment_move(x, has_internal = TRUE)
    outs <- rowSums(x[ , 1:5])
    ins <- colSums(x[, 1:3])
    ans_expected <- ins - outs
    expect_identical(ans_obtained, ans_expected)
})

test_that("'make_increment_move' works with valid input - no internal", {
    x <- matrix(1:12, nr = 3)
    ans_obtained <- make_increment_move(x, has_internal = FALSE)
    ans_expected <- as.integer(-rowSums(x[ , 1:2]))
    expect_identical(ans_obtained, ans_expected)
})

