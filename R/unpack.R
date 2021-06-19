
## HAS_TESTS
## returns matrix with dimensions 'n_sex', 'n_region'
unpack_births <- function(x, has_internal) {
    offset_dth_emig <- 2L
    if (has_internal) {
        n_region <- nrow(x)
        start <- n_region + offset_dth_emig + 1L
    }
    else
        start <- offset_dth_emig + 1L
    end <- ncol(x)
    cols <- seq.int(from = start, to = end)
    ans <- x[ , cols]
    ans <- t(ans) ## sex dimension comes before region dimension
    ans
}

## HAS_TESTS
## returns vector with length 'n_region'
unpack_deaths <- function(x, has_internal) {
    offset_dth_emig <- 2L
    if (has_internal) {
        n_region <- nrow(x)
        i <- n_region + 1L
    }
    else
        i <- 1L
    x[ , i]
}

## HAS_TESTS
## returns vector with length 'n_region'
unpack_emigration <- function(x, has_internal) {
    if (has_internal) {
        n_region <- nrow(x)
        i <- n_region + 2L
    }
    else
        i <- 2L
    x[ , i]
}

## HAS_TESTS
## returns matrix with dimensions 'n_region', 'n_region'
unpack_internal <- function(x) {
    n_region <- nrow(x)
    s <- seq_len(n_region)
    x[s, s]
}




        
    
