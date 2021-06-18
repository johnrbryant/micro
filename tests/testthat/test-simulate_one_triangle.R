
test_that("'simulate_one_triangle' works with valid input", {
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


