
test_that("'make_increment_move' works with valid input", {
    x <- matrix(1:21, nr = 3)
    ans_obtained <- make_increment_move(x)
    outs <- rowSums(x[ , 1:5])
    ins <- colSums(x[, 1:3])
    ans_expected <- ins - outs
    expect_identical(ans_obtained, ans_expected)
})
