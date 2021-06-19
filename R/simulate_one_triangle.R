
## HAS_TESTS
simulate_one_triangle <- function(move_rates_indiv,
                                  fert_rates_indiv,
                                  i_region,
                                  time_remaining,
                                  generate_births) {
    n_offset_dth_emig <- 2L
    n_region <- nrow(move_rates_indiv)
    n_col_move <- ncol(move_rates_indiv)
    n_move <- length(move_rates_indiv)
    has_internal <- n_col_move > n_offset_dth_emig
    has_births <- !is.null(fert_rates_indiv)
    n_sex <- if (has_births) ncol(fert_rates_indiv) else 0L
    events_indiv <- matrix(0L,
                           nrow = n_region,
                           ncol = n_col_move + n_sex)
    died_or_emigrated <- FALSE
    if (has_births)
        i_births <- seq.int(from = n_col_move + 1L,
                            length.out = n_sex)
    while ((time_remaining > 0) && !died_or_emigrated) {
        rates_move <- move_rates_indiv[i_region, ]
        if (any(rates_move > 0)) {
            times_move <- suppressWarnings(stats::rexp(n = n_col_move,
                                                       rate = rates_move)) # ignore warnings from rates of 0
            i_earliest <- which.min(times_move)
            time_earliest <- times_move[[i_earliest]]
        }
        else
            time_earliest <- Inf
        event_occurred <- time_earliest < time_remaining
        if (event_occurred) {
            events_indiv[i_region, i_earliest] <- events_indiv[i_region, i_earliest] + 1L
            died_or_emigrated <- !has_internal || (i_earliest > n_region)
        }
        if (generate_births) {
            exposure <- min(time_earliest, time_remaining)
            rates_fert <- fert_rates_indiv[i_region, ]
            lambda <- exposure * rates_fert
            births <- stats::rpois(n = n_sex,
                                   lambda = lambda)
            events_indiv[i_region, i_births] <- events_indiv[i_region, i_births] + births
        }
        i_region <- i_earliest
        time_remaining <- time_remaining - time_earliest
    }
    events_indiv
}
        
