
test_that("'unpack_births' works with valid input", {
    x <- matrix(1:21, nr = 3)
    expect_identical(unpack_births(x), matrix(16:21, nr = 2, byrow = TRUE))
})

test_that("'unpack_deaths' works with valid input", {
    x <- matrix(1:21, nr = 3)
    expect_identical(unpack_deaths(x), 10:12)
})

test_that("'unpack_emigration' works with valid input", {
    x <- matrix(1:21, nr = 3)
    expect_identical(unpack_emigration(x), 13:15)
})

test_that("'unpack_immigration' works with valid input", {
    x <- matrix(1:21, nr = 3)
    expect_identical(unpack_internal(x), matrix(1:9, nr = 3))
})

