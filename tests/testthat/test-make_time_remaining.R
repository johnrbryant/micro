
test_that("'make_time_remaining' has correct mean when is_lower is TRUE and is_immigrant is TRUE", {
    set.seed(0)
    x <- replicate(n = 1000,
                   make_time_remaining(step_length = 1,
                                       is_lower = TRUE,
                                       is_immigrant = TRUE,
                                       is_final_age = NULL))
    ans_obtained <- mean(x)
    ans_expected <- 2/3
    expect_equal(ans_obtained, ans_expected, tolerance = 0.01)
})

test_that("'make_time_remaining' has correct mean when is_lower is TRUE and is_immigrant is FALSE", {
    set.seed(0)
    x <- replicate(n = 1000,
                   make_time_remaining(step_length = 1,
                                       is_lower = TRUE,
                                       is_immigrant = FALSE,
                                       is_final_age = NULL))
    ans_obtained <- mean(x)
    ans_expected <- 1/2
    expect_equal(ans_obtained, ans_expected, tolerance = 0.01)
})

test_that("'make_time_remaining' has correct mean when is_lower is FALSE and is_immigrant is TRUE is_final_age is TRUE", {
    set.seed(0)
    x <- replicate(n = 1000,
                   make_time_remaining(step_length = 1,
                                       is_lower = FALSE,
                                       is_immigrant = TRUE,
                                       is_final_age = TRUE))
    ans_obtained <- mean(x)
    ans_expected <- 1/2
    expect_equal(ans_obtained, ans_expected, tolerance = 0.01)
})

test_that("'make_time_remaining' has correct value when is_lower is FALSE and is_immigrant is FALSE is_final_age is TRUE", {
    ans_obtained <- make_time_remaining(step_length = 1,
                                       is_lower = FALSE,
                                       is_immigrant = FALSE,
                                       is_final_age = TRUE)
    ans_expected <- 1
    expect_identical(ans_obtained, ans_expected)
})

test_that("'make_time_remaining' has correct mean when is_lower is FALSE and is_immigrant is TRUE is_final_age is TRUE", {
    set.seed(0)
    x <- replicate(n = 1000,
                   make_time_remaining(step_length = 1,
                                       is_lower = FALSE,
                                       is_immigrant = TRUE,
                                       is_final_age = FALSE))
    ans_obtained <- mean(x)
    ans_expected <- 1/3
    expect_equal(ans_obtained, ans_expected, tolerance = 0.01)
})


test_that("'make_time_remaining' has correct mean when is_lower is FALSE and is_immigrant is TRUE is_final_age is TRUE", {
    set.seed(0)
    x <- replicate(n = 1000,
                   make_time_remaining(step_length = 1,
                                       is_lower = FALSE,
                                       is_immigrant = FALSE,
                                       is_final_age = FALSE))
    ans_obtained <- mean(x)
    ans_expected <- 1/2
    expect_equal(ans_obtained, ans_expected, tolerance = 0.01)
})



