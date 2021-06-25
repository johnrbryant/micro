
test_that("test-microsim works with valid inputs - no iterations", {
    library(dembase)
    initial_popn <- array(rpois(n = 24, lambda = 10),
                          dim = c(4, 2, 3),
                          dimnames = list(age = c("0-4", "5-9", "10-14", "15+"),
                                          sex = c("Female", "Male"),
                                          region = c("A", "B", "C")))
    fertility_rates <- array(runif(240, max = c(0, 0, 2, 0)),
                             dim = c(4, 2, 2, 3, 5),
                             dimnames = list(age = c("0-4", "5-9", "10-14", "15+"),
                                             triangle = c("Lower", "Upper"),
                                             sex = c("Female", "Male"),
                                             region = c("A", "B", "C"),
                                             time = c("2001-2005", "2006-2010",
                                                      "2011-2015", "2016-2020",
                                                      "2021-2025")))
    mortality_rates <- array(runif(240, max = c(0.05, 0.1, 0.1, 0.3)),
                             dim = c(4, 2, 2, 3, 5),
                             dimnames = list(age = c("0-4", "5-9", "10-14", "15+"),
                                             triangle = c("Lower", "Upper"),
                                             sex = c("Female", "Male"),
                                             region = c("A", "B", "C"),
                                             time = c("2001-2005", "2006-2010",
                                                      "2011-2015", "2016-2020",
                                                      "2021-2025")))
    internal_rates <- array(runif(720, max = 0.2),
                            dim = c(4, 2, 2, 3, 3, 5),
                            dimnames = list(age = c("0-4", "5-9", "10-14", "15+"),
                                            triangle = c("Lower", "Upper"),
                                            sex = c("Female", "Male"),
                                            region_orig = c("A", "B", "C"),
                                            region_dest = c("A", "B", "C"),
                                            time = c("2001-2005", "2006-2010",
                                                     "2011-2015", "2016-2020",
                                                     "2021-2025")))
    internal_rates[slice.index(internal_rates, 4) == slice.index(internal_rates, 5)] <- 0
    immigration_means <- array(rpois(n = 240, lambda = 3),
                               dim = c(4, 2, 2, 3, 5),
                               dimnames = list(age = c("0-4", "5-9", "10-14", "15+"),
                                               triangle = c("Lower", "Upper"),
                                               sex = c("Female", "Male"),
                                               region = c("A", "B", "C"),
                                               time = c("2001-2005", "2006-2010",
                                                        "2011-2015", "2016-2020",
                                                        "2021-2025")))
    emigration_rates <- array(runif(n = 240, max = 0.1),
                              dim = c(4, 2, 2, 3, 5),
                              dimnames = list(age = c("0-4", "5-9", "10-14", "15+"),
                                              triangle = c("Lower", "Upper"),
                                              sex = c("Female", "Male"),
                                              region = c("A", "B", "C"),
                                              time = c("2001-2005", "2006-2010",
                                                       "2011-2015", "2016-2020",
                                                       "2021-2025")))
    ## all components
    ans_obtained <- microsim(initial_popn = initial_popn,
                             fertility_rates = fertility_rates,
                             mortality_rates = mortality_rates,
                             internal_rates = internal_rates,
                             immigration_means = immigration_means,
                             emigration_rates = emigration_rates,
                             step_length = 5,
                             dominant_sex = "Female")
    expect_true(all(isConsistent(ans_obtained)))
    expect_identical(componentNames(ans_obtained),
                     c("births", "internal", "immigration", "deaths", "emigration"))
    expect_identical(names(population(ans_obtained)), c("age", "sex", "region", "time"))
    ## no births
    ans_obtained <- microsim(initial_popn = initial_popn,
                             fertility_rates = NULL,
                             mortality_rates = mortality_rates,
                             internal_rates = internal_rates,
                             immigration_means = immigration_means,
                             emigration_rates = emigration_rates,
                             step_length = 5,
                             dominant_sex = "Female")
    expect_true(all(isConsistent(ans_obtained)))
    expect_identical(componentNames(ans_obtained),
                     c("internal", "immigration", "deaths", "emigration"))
    expect_identical(names(population(ans_obtained)), c("age", "sex", "region", "time"))
    ## no internal
    ans_obtained <- microsim(initial_popn = initial_popn,
                             fertility_rates = fertility_rates,
                             mortality_rates = mortality_rates,
                             internal_rates = NULL,
                             immigration_means = immigration_means,
                             emigration_rates = emigration_rates,
                             step_length = 5,
                             dominant_sex = "Female")
    expect_true(all(isConsistent(ans_obtained)))
    expect_identical(componentNames(ans_obtained),
                     c("births", "immigration", "deaths", "emigration"))
    expect_identical(names(population(ans_obtained)), c("age", "sex", "region", "time"))
    ## no births, no internal
    ans_obtained <- microsim(initial_popn = initial_popn,
                             fertility_rates = NULL,
                             mortality_rates = mortality_rates,
                             internal_rates = NULL,
                             immigration_means = immigration_means,
                             emigration_rates = emigration_rates,
                             step_length = 5,
                             dominant_sex = "Female")
    expect_true(all(isConsistent(ans_obtained)))
    expect_identical(componentNames(ans_obtained),
                     c("immigration", "deaths", "emigration"))
    expect_identical(names(population(ans_obtained)), c("age", "sex", "region", "time"))
})


test_that("test-microsim works with valid inputs - has iterations", {
    library(dembase)
    initial_popn <- array(rpois(n = 24, lambda = 10),
                          dim = c(4, 2, 3),
                          dimnames = list(age = c("0-4", "5-9", "10-14", "15+"),
                                          sex = c("Female", "Male"),
                                          region = c("A", "B", "C")))
    fertility_rates <- array(runif(240, max = c(0, 0, 2, 0)),
                             dim = c(4, 2, 2, 3, 5),
                             dimnames = list(age = c("0-4", "5-9", "10-14", "15+"),
                                             triangle = c("Lower", "Upper"),
                                             sex = c("Female", "Male"),
                                             region = c("A", "B", "C"),
                                             time = c("2001-2005", "2006-2010",
                                                      "2011-2015", "2016-2020",
                                                      "2021-2025")))
    mortality_rates <- array(runif(240, max = c(0.05, 0.1, 0.1, 0.3)),
                             dim = c(4, 2, 2, 3, 5),
                             dimnames = list(age = c("0-4", "5-9", "10-14", "15+"),
                                             triangle = c("Lower", "Upper"),
                                             sex = c("Female", "Male"),
                                             region = c("A", "B", "C"),
                                             time = c("2001-2005", "2006-2010",
                                                      "2011-2015", "2016-2020",
                                                      "2021-2025")))
    internal_rates <- array(runif(720, max = 0.2),
                            dim = c(4, 2, 2, 3, 3, 5),
                            dimnames = list(age = c("0-4", "5-9", "10-14", "15+"),
                                            triangle = c("Lower", "Upper"),
                                            sex = c("Female", "Male"),
                                            region_orig = c("A", "B", "C"),
                                            region_dest = c("A", "B", "C"),
                                            time = c("2001-2005", "2006-2010",
                                                     "2011-2015", "2016-2020",
                                                     "2021-2025")))
    internal_rates[slice.index(internal_rates, 4) == slice.index(internal_rates, 5)] <- 0
    immigration_means <- array(rpois(n = 240, lambda = 3),
                               dim = c(4, 2, 2, 3, 5),
                               dimnames = list(age = c("0-4", "5-9", "10-14", "15+"),
                                               triangle = c("Lower", "Upper"),
                                               sex = c("Female", "Male"),
                                               region = c("A", "B", "C"),
                                               time = c("2001-2005", "2006-2010",
                                                        "2011-2015", "2016-2020",
                                                        "2021-2025")))
    emigration_rates <- array(runif(n = 240, max = 0.1),
                              dim = c(4, 2, 2, 3, 5),
                              dimnames = list(age = c("0-4", "5-9", "10-14", "15+"),
                                              triangle = c("Lower", "Upper"),
                                              sex = c("Female", "Male"),
                                              region = c("A", "B", "C"),
                                              time = c("2001-2005", "2006-2010",
                                                       "2011-2015", "2016-2020",
                                                       "2021-2025")))
    ## all components
    ans_obtained <- microsim(initial_popn = initial_popn,
                             fertility_rates = fertility_rates,
                             mortality_rates = mortality_rates,
                             internal_rates = internal_rates,
                             immigration_means = immigration_means,
                             emigration_rates = emigration_rates,
                             step_length = 5,
                             dominant_sex = "Female",
                             n_iter = 2)
    expect_true(all(isConsistent(ans_obtained)))
    expect_identical(componentNames(ans_obtained),
                     c("births", "internal", "immigration", "deaths", "emigration"))
    expect_identical(names(population(ans_obtained)),
                     c("age", "sex", "region", "time", "iteration"))
    ## no births
    ans_obtained <- microsim(initial_popn = initial_popn,
                             fertility_rates = NULL,
                             mortality_rates = mortality_rates,
                             internal_rates = internal_rates,
                             immigration_means = immigration_means,
                             emigration_rates = emigration_rates,
                             step_length = 5,
                             dominant_sex = "Female",
                             n_iter = 2)
    expect_true(all(isConsistent(ans_obtained)))
    expect_identical(componentNames(ans_obtained),
                     c("internal", "immigration", "deaths", "emigration"))
    expect_identical(names(population(ans_obtained)),
                     c("age", "sex", "region", "time", "iteration"))
    ## no internal
    ans_obtained <- microsim(initial_popn = initial_popn,
                             fertility_rates = fertility_rates,
                             mortality_rates = mortality_rates,
                             internal_rates = NULL,
                             immigration_means = immigration_means,
                             emigration_rates = emigration_rates,
                             step_length = 5,
                             dominant_sex = "Female",
                             n_iter = 2)
    expect_true(all(isConsistent(ans_obtained)))
    expect_identical(componentNames(ans_obtained),
                     c("births", "immigration", "deaths", "emigration"))
    expect_identical(names(population(ans_obtained)),
                     c("age", "sex", "region", "time", "iteration"))
    ## no births, no internal
    ans_obtained <- microsim(initial_popn = initial_popn,
                             fertility_rates = NULL,
                             mortality_rates = mortality_rates,
                             internal_rates = NULL,
                             immigration_means = immigration_means,
                             emigration_rates = emigration_rates,
                             step_length = 5,
                             dominant_sex = "Female",
                             n_iter = 2)
    expect_true(all(isConsistent(ans_obtained)))
    expect_identical(componentNames(ans_obtained),
                     c("immigration", "deaths", "emigration"))
    expect_identical(names(population(ans_obtained)),
                     c("age", "sex", "region", "time", "iteration"))
})

    
