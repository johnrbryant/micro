
## HAS_TESTS
simulate_one_triangle <- function(move_rates_indiv,
                                  fert_rates_indiv,
                                  i_region,
                                  time_remaining,
                                  generate_births) {
    n_region <- nrow(move_rates_indiv)
    n_col_move <- ncol(move_rates_indiv)
    n_move <- length(move_rates_indiv)
    n_sex <- ncol(fert_rates_indiv)
    events_indiv <- matrix(0L,
                           nrow = n_region,
                           ncol = n_col_move + n_sex)
    died_or_emigrated <- FALSE
    i_births <- seq.int(from = n_col_move + 1L,
                        length.out = n_sex)
    while ((time_remaining > 0) && !died_or_emigrated) {
        rates_move <- move_rates_indiv[i_region, ]
        times_move <- suppressWarnings(rexp(n = n_col_move,
                                            rate = rates_move)) # ignore warnings from rates of 0
        i_earliest <- which.min(times_move)
        time_earliest <- times_move[[i_earliest]]
        event_occurred <- time_earliest < time_remaining
        if (event_occurred) {
            events_indiv[i_region, i_earliest] <- events_indiv[i_region, i_earliest] + 1L
            died_or_emigrated <- i_earliest > n_region
        }
        if (generate_births) {
            exposure <- min(time_earliest, time_remaining)
            rates_fert <- fert_rates_indiv[i_region, ]
            lambda <- exposure * rates_fert
            births <- rpois(n = n_sex,
                            lambda = lambda)
            events_indiv[i_region, i_births] <- events_indiv[i_region, i_births] + births
        }
        i_region <- i_earliest
        time_remaining <- time_remaining - time_earliest
    }
    events_indiv
}
        
