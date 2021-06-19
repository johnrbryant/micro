
test_that("'simulate_one_triangle' works with valid input - has births and internal", {
    set.seed(0)
    for (i in 1:100) {
        move_rates_indiv <- matrix(runif(n = 15, max = 0.3),
                                   nr = 3)
        diag(move_rates_indiv) <- 0
        fert_rates_indiv <- matrix(runif(n = 6, max = 0.1),
                                   nr = 3)
        i_region <- 1L
        time_remaining <- 2.5
        generate_births <- TRUE
        ans_obtained <- simulate_one_triangle(move_rates_indiv = move_rates_indiv,
                                              fert_rates_indiv = fert_rates_indiv,
                                              i_region = i_region,
                                              time_remaining = time_remaining,
                                              generate_births = generate_births)
        expect_identical(sum(diag(ans_obtained)), 0L)
        expect_true(sum(ans_obtained[10:15]) %in% 0:1)
        expect_true(all(ans_obtained >= 0L))
    }    
})

test_that("'simulate_one_triangle' works with valid input - no births, has internal", {
    set.seed(0)
    for (i in 1:100) {
        move_rates_indiv <- matrix(runif(n = 15, max = 0.3),
                                   nr = 3)
        diag(move_rates_indiv) <- 0
        fert_rates_indiv <- NULL
        i_region <- 1L
        time_remaining <- 2.5
        generate_births <- FALSE
        ans_obtained <- simulate_one_triangle(move_rates_indiv = move_rates_indiv,
                                              fert_rates_indiv = fert_rates_indiv,
                                              i_region = i_region,
                                              time_remaining = time_remaining,
                                              generate_births = generate_births)
        expect_identical(sum(diag(ans_obtained)), 0L)
        expect_true(sum(ans_obtained[10:15]) %in% 0:1)
        expect_true(all(ans_obtained >= 0L))
        expect_identical(dim(ans_obtained),  c(3L, 5L))
    }    
})

test_that("'simulate_one_triangle' works with valid input - has births, no internal", {
    set.seed(0)
    for (i in 1:100) {
        move_rates_indiv <- matrix(runif(n = 6, max = 0.3),
                                   nr = 3)
        fert_rates_indiv <- matrix(runif(n = 6, max = 0.1),
                                   nr = 3)
        i_region <- 1L
        time_remaining <- 2.5
        generate_births <- TRUE
        ans_obtained <- simulate_one_triangle(move_rates_indiv = move_rates_indiv,
                                              fert_rates_indiv = fert_rates_indiv,
                                              i_region = i_region,
                                              time_remaining = time_remaining,
                                              generate_births = generate_births)
        expect_true(sum(ans_obtained[1:6]) %in% 0:1)
        expect_true(all(ans_obtained >= 0L))
    }    
})

test_that("'simulate_one_triangle' works with valid input - no births, no internal", {
    set.seed(0)
    for (i in 1:100) {
        move_rates_indiv <- matrix(runif(n = 6, max = 0.3),
                                   nr = 3)
        fert_rates_indiv <- NULL
        i_region <- 1L
        time_remaining <- 2.5
        generate_births <- FALSE
        ans_obtained <- simulate_one_triangle(move_rates_indiv = move_rates_indiv,
                                              fert_rates_indiv = fert_rates_indiv,
                                              i_region = i_region,
                                              time_remaining = time_remaining,
                                              generate_births = generate_births)
        expect_true(sum(ans_obtained) %in% 0:1)
        expect_true(all(ans_obtained >= 0L))
    }    
})




