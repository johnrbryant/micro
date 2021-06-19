
test_that("'unpack_births' works with valid input - has internal", {
    x <- matrix(1:21, nr = 3)
    expect_identical(unpack_births(x, has_internal = TRUE),
                     matrix(16:21, nr = 2, byrow = TRUE))
})


test_that("'unpack_births' works with valid input - no internal", {
    x <- matrix(1:12, nr = 3)
    expect_identical(unpack_births(x, has_internal = FALSE),
                     matrix(7:12, nr = 2, byrow = TRUE))
})

test_that("'unpack_deaths' works with valid input - has internal", {
    x <- matrix(1:21, nr = 3)
    expect_identical(unpack_deaths(x, has_internal = TRUE),
                     10:12)
})

test_that("'unpack_deaths' works with valid input - no internal", {
    x <- matrix(1:12, nr = 3)
    expect_identical(unpack_deaths(x, has_internal = FALSE),
                     1:3)
})

test_that("'unpack_emigration' works with valid input - has internal", {
    x <- matrix(1:21, nr = 3)
    expect_identical(unpack_emigration(x, has_internal = TRUE),
                     13:15)
})

test_that("'unpack_emigration' works with valid input - no internal", {
    x <- matrix(1:12, nr = 3)
    expect_identical(unpack_emigration(x, has_internal = FALSE), 4:6)
})

test_that("'unpack_internal' works with valid input", {
    x <- matrix(1:21, nr = 3)
    expect_identical(unpack_internal(x), matrix(1:9, nr = 3))
})

