
## HAS_TESTS
make_time_remaining <- function(step_length,
                                is_lower,
                                is_immigrant,
                                is_final_age) {
    if (is_lower) {
        x <- runif(n = 1L)
        if (is_immigrant) 
            ans <- sqrt(x) * step_length            # lower tri, immigrant
        else
            ans <- x * step_length                  # lower tri, non-immigrant
    }
    else {
        if (is_final_age) {
            if (is_immigrant) {
                x <- runif(n = 1L)
                ans <- x * step_length              # upper tri, final age, immigrant
            }
            else
                ans <- step_length                  # upper tri, final age, non-immigrant
        }
        else {
            x <- runif(n = 1L)
            if (is_immigrant)
                ans <- (1 - sqrt(x)) * step_length  # upper tri, not final age, immigrant
            else
                ans <- x * step_length              # upper tri, not final age, not immigrant
        }
    }
    ans
}



        
    
